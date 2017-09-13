(ns netrunner.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! >!] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.ajax :refer [GET]]))

(def cards-channel (chan))

;; Load in sets and mwl lists
(go (let [sets (:json (<! (GET "/data/sets")))
          mwl (:json (<! (GET "/data/mwl")))]
      (swap! app-state assoc :sets sets :mwl mwl)))

(go (let [cards (sort-by :code (:json (<! (GET "/data/cards"))))]
      (swap! app-state assoc :cards cards)
      (put! cards-channel cards)))

(defn make-span [text symbol class]
  (.replace text (js/RegExp. symbol "g") (str "<span class='anr-icon " class "'></span>")))

(defn- image-url-string
  [code, version-string]
  (str "/img/cards/" code (when version-string (str "-" version-string)) ".png"))

(defn- build-image-url
  [code, version-string]
  (let [version (if (get-in @app-state [:options :show-alt-art]) version-string nil)]
    (image-url-string code version)))

(defn image-url 
  [{:keys [code, version, check_for_alts]}]
  (let [alt-art (get-in @app-state [:alt-arts code])
        allowed_alt (when (and check_for_alts
                               (get-in @app-state [:user :special])
                               alt-art)
                      (first (:versions alt-art)))
        version-string (cond
                         (= version "default") nil
                         (nil? version) allowed_alt
                         :else version)]
    (build-image-url code version-string)))

(defn add-symbols [card-text]
  (-> (if (nil? card-text) "" card-text)
      (make-span "\\[Credits\\]" "credit")
      (make-span "\\[Credit\\]" "credit")
      (make-span "\\[Click\\]" "click")
      (make-span "\\[Subroutine\\]" "subroutine")
      (make-span "\\[Recurring Credits\\]" "recurring-credit")
      (make-span "1\\[Memory Unit\\]" "mu1")
      (make-span "2\\[Memory Unit\\]" "mu2")
      (make-span "3\\[Memory Unit\\]" "mu3")
      (make-span "\\[Memory Unit\\]" "mu")
      (make-span "\\[Link\\]" "link")
      (make-span "\\[Trash\\]" "trash")))

(defn card-view [card owner]
  (reify
    om/IInitState
    (init-state [_] {:showText false})
    om/IRenderState
    (render-state [_ state]
      (let [ifShowText #(when (:showText state) %)]
        (sab/html
         [:div.card-preview.blue-shade
          (ifShowText [:h4 (:title card)])
          (ifShowText (when-let [memory (:memoryunits card)]
                        (if (< memory 3)
                          [:div.anr-icon {:class (str "mu" memory)} ""]
                          [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]])))
          (ifShowText (when-let [cost (:cost card)]
                        [:div.heading (str "Cost: " cost)]))
          (ifShowText (when-let [trash-cost (:trash card)]
                        [:div.heading (str "Trash cost: " trash-cost)]))
          (ifShowText (when-let [strength (:strength card)]
                        [:div.heading (str "Strength: " strength)]))
          (ifShowText (when-let [requirement (:advancementcost card)]
                        [:div.heading (str "Advancement requirement: " requirement)]))
          (ifShowText (when-let [agenda-point (:agendatpoints card)]
                        [:div.heading (str "Agenda points: " agenda-point)]))
          (ifShowText (when-let [min-deck-size (:minimumdecksize card)]
                        [:div.heading (str "Minimum deck size: " min-deck-size)]))
          (ifShowText (when-let [influence-limit (:influencelimit card)]
                        [:div.heading (str "Influence limit: " influence-limit)]))
          (ifShowText (when-let [influence (:factioncost card)]
                        (when-let [faction (:faction card)]
                         [:div.heading "Influence "
                          [:span.influence
                           {:dangerouslySetInnerHTML #js {:__html (apply str (for [i (range influence)] "&#8226;"))}
                            :class                   (-> faction .toLowerCase (.replace " " "-"))}]])))
          (ifShowText [:div.text
                       [:p [:span.type (str (:type card))] (if (empty? (:subtype card))
                                                             "" (str ": " (:subtype card)))]
                       [:pre {:dangerouslySetInnerHTML #js {:__html (add-symbols (:text card))}}]])
          (when-not (:showText state)
            (when-let [url (image-url card)]
              [:img {:src url
                     :onError #(-> (om/set-state! owner {:showText true}))
                     :onLoad #(-> % .-target js/$ .show)}]))])))))

(defn types [side]
  (let [runner-types ["Identity" "Program" "Hardware" "Resource" "Event"]
        corp-types ["Agenda" "Asset" "ICE" "Operation" "Upgrade"]]
    (case side
      "All" (concat runner-types corp-types)
      "Runner" runner-types
      "Corp" (cons "Identity" corp-types))))

(defn factions [side]
  (let [runner-factions ["Anarch" "Criminal" "Shaper"]
        corp-factions ["Jinteki" "Haas-Bioroid" "NBN" "Weyland Consortium" "Neutral"]]
    (case side
      "All" (concat runner-factions corp-factions)
      "Runner" (conj runner-factions "Neutral")
      "Corp" corp-factions)))

(defn options [list]
  (let [options (cons "All" list)]
    (for [option options]
      [:option {:value option :dangerouslySetInnerHTML #js {:__html option}}])))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (field %) filter-value) cards)))

(defn match [query cards]
  (if (empty? query)
    cards
    (filter #(if (= (.indexOf (.toLowerCase (:title %)) query) -1) false true) cards)))

(defn sort-field [fieldname]
  (case fieldname
    "Name" :title
    "Influence" :factioncost
    "Cost" :cost
    "Faction" (juxt :side :faction)
    "Type" (juxt :side :type)
    "Set number" :number))

(defn handle-scroll [e owner {:keys [page]}]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.innerHeight $cardlist))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (om/update-state! owner :page inc))))

(defn handle-search [e owner]
  (doseq [filter [:set-filter :type-filter :sort-filter :faction-filter]]
    (om/set-state! owner filter "All"))
  (om/set-state! owner :search-query (.. e -target -value)))

(defn- add-alt-art
  "Add all alternate art versions for a single card"
  [{:keys [code] :as card}]
  (let [alts (:alt-arts @app-state)
        card-alts (get alts code {:versions ["default"]})]
    (map #(assoc card
                 :version %
                 :code-version (str code "-" %)
                 :check_for_alts false
                 :setname (if (= % "default") (:setname card) "Alternative Art"))
         (:versions card-alts))))

(defn add-alt-arts
  "Add alternate art versions of cards to the list of cards"
  [cards]
  (if (get-in @app-state [:options :show-alt-art]) 
    (flatten (map add-alt-art cards))
    cards))

(defn card-browser [{:keys [sets] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:search-query ""
       :sort-field "Faction"
       :set-filter "All"
       :type-filter "All"
       :side-filter "All"
       :faction-filter "All"
       :page 1
       :filter-ch (chan)})

    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [f (<! (om/get-state owner :filter-ch))]
              (om/set-state! owner (:filter f) (:value f))))))

    om/IRenderState
    (render-state [this state]
      (.focus (js/$ ".search"))
      (sab/html
       [:div.cardbrowser
        [:div.blue-shade.panel.filters
         (let [query (:search-query state)]
           [:div.search-box
            [:span.e.search-icon {:dangerouslySetInnerHTML #js {:__html "&#xe822;"}}]
            (when-not (empty? query)
              [:span.e.search-clear {:dangerouslySetInnerHTML #js {:__html "&#xe819;"}
                                     :on-click #(om/set-state! owner :search-query "")}])
            [:input.search {:on-change #(handle-search % owner)
                            :type "text" :placeholder "Search cards" :value query}]])

         [:div
          [:h4 "Sort by"]
          [:select {:value (:sort-filter state)
                    :on-change #(om/set-state! owner :sort-field (.trim (.. % -target -value)))}
           (for [field ["Faction" "Name" "Type" "Influence" "Cost" "Set number"]]
             [:option {:value field} field])]]

         (let [cycles (conj
                        (for [[cycle cycle-sets] (rest (group-by :cycle sets))]
                          {:name (str cycle " cycle") :available (:available (first cycle-sets))})
                        {:name "Alternative Art" :available "4097-01-01"})
               cycle-sets (map #(if (:cycle %)
                                  (update-in % [:name] (fn [name] (str "&nbsp;&nbsp;&nbsp;&nbsp;" name)))
                                  %)
                               sets)]
           (for [filter [["Set" :set-filter (map :name (sort-by :available (concat cycles cycle-sets)))]
                         ["Side" :side-filter ["Corp" "Runner"]]
                         ["Faction" :faction-filter (factions (:side-filter state))]
                         ["Type" :type-filter (types (:side-filter state))]]]
             [:div
              [:h4 (first filter)]
              [:select {:value ((second filter) state)
                        :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
               (options (last filter))]]))]

        [:div.card-list {:on-scroll #(handle-scroll % owner state)}
         (om/build-all card-view
                       (let [s (-> (:set-filter state)
                                     (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
                                     (.replace " cycle" ""))
                             cycle-sets (set (for [x sets :when (= (:cycle x) s)] (:name x)))
                             cards (if (= s "All")
                                     (add-alt-arts (:cards cursor))
                                     (if (= (.indexOf (:set-filter state) "cycle") -1)
                                       (filter #(= (:setname %) s) (add-alt-arts (:cards cursor)))
                                       (filter #(cycle-sets (:setname %)) (add-alt-arts (:cards cursor)))))]
                         (->> cards
                              (filter-cards (:side-filter state) :side)
                              (filter-cards (:faction-filter state) :faction)
                              (filter-cards (:type-filter state) :type)
                              (match (.toLowerCase (:search-query state)))
                              (sort-by (sort-field (:sort-field state)))
                              (take (* (:page state) 28))))
                       {:key :code-version})]]))))

(om/root card-browser app-state {:target (. js/document (getElementById "cardbrowser"))})
