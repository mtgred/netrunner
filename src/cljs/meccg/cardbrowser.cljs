(ns meccg.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [meccg.appstate :refer [app-state]]
            [meccg.ajax :refer [GET]]
            [superstring.core :as str]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

;; Load in sets and mwl lists
(go (let [sets (:json (<! (GET "/data/sets")))
         ;; cycles (:json (<! (GET "/data/cycles")))
         ;; mwl (:json (<! (GET "/data/mwl")))
         ]
      (swap! app-state assoc :sets sets)))

(go (let [cards (sort-by :code (:json (<! (GET "/data/cards"))))]
      (swap! app-state assoc :cards cards)
      (swap! app-state assoc :cards-loaded true)
      (put! cards-channel cards)))

(defn make-span [text symbol class]
  (.replace text (js/RegExp. symbol "gi") (str "<span class='anr-icon " class "'></span>")))

(defn image-url [card]
  (str "/img/cards/" (:Set card) "/" (:ImageName card))) ;;had ".png"

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

(defn- card-text
  "Generate text html representation a card"
  [card]
  [:div
   [:h4 (:NameEN card)]
   (when-let [memory (:memoryunits card)]
     (if (< memory 3)
       [:div.anr-icon {:class (str "mu" memory)} ""]
       [:div.heading (str "Memory: " memory) [:span.anr-icon.mu]]))
   (when-let [cost (:cost card)]
     [:div.heading (str "Cost: " cost)])
   (when-let [trash-cost (:trash card)]
     [:div.heading (str "Trash cost: " trash-cost)])
   (when-let [strength (:strength card)]
     [:div.heading (str "Strength: " strength)])
   (when-let [requirement (:advancementcost card)]
     [:div.heading (str "Advancement requirement: " requirement)])
   (when-let [agenda-point (:agendapoints card)]
     [:div.heading (str "Agenda points: " agenda-point)])
   (when-let [min-deck-size (:minimumdecksize card)]
     [:div.heading (str "Minimum deck size: " min-deck-size)])
   (when-let [influence-limit (:influencelimit card)]
     [:div.heading (str "Influence limit: " influence-limit)])
   (when-let [influence (:factioncost card)]
     (when-let [faction (:faction card)]
       [:div.heading "Influence "
        [:span.influence
         {:dangerouslySetInnerHTML #js {:__html (apply str (for [i (range influence)] "&#8226;"))}
          :class                   (-> faction str/strip-accents .toLowerCase (.replace " " "-"))}]]))
   [:div.text
    [:p [:span.type (str (:type card))] (if (empty? (:subtype card))
                                          "" (str ": " (:subtype card)))]
    [:pre {:dangerouslySetInnerHTML #js {:__html (add-symbols (:Text card))}}]]
   ])

(defn card-view [card owner]
  (reify
    om/IInitState
    (init-state [_] {:showText false})
    om/IRenderState
    (render-state [_ state]
      (sab/html
        [:div.card-preview.blue-shade
         (if (:showText state)
           (card-text card)
           (when-let [url (image-url card)]
             [:img {:src url
                    :onClick #(do (.preventDefault %)
                                  (put! (:pub-chan (om/get-shared owner))
                                        {:topic :card-selected :data card})
                                  nil)
                    :onError #(-> (om/set-state! owner {:showText true}))
                    :onLoad #(-> % .-target js/$ .show)}]))]))))

(defn card-info-view [card owner]
  (reify
    om/IInitState
    (init-state [_] {:selected-card nil})
    om/IDidMount
    (did-mount [_]
      (let [events (sub (:notif-chan (om/get-shared owner)) :card-selected (chan))]
        (go
          (loop [e (<! events)]
            (om/set-state! owner :selected-card (:data e))
            (recur (<! events))))))
    om/IRenderState
    (render-state [_ {:keys [selected-card]}]
      (sab/html
        (if (nil? selected-card)
          [:div]
          (card-text selected-card))))))

(def primary-order ["Character" "Resource" "Hazard" "Site" "Region"])
(def resource-secondaries ["Ally" "Faction" "Greater Item" "Major Item" "Minor Item" "Special Item"])
(def shared-secondaries ["Long-event" "Permanent-event" "Permanent-event/Short-event" "Short-event"])
(def hazard-secondaries ["Creature" "Creature/Permanent-event" "Creature/Short-event"])
(def general-alignments ["Hero" "Minion" "Balrog" "Fallen-wizard" "Elf-lord" "Dwarf-lord" "Dual"])
(def set-order ["METW" "METD" "MEDM" "MELE" "MEAS" "MEWH" "MEBA" "MEFB" "MEDF" "MENE"])

(defn secondaries [primary]
    (case primary
      "All" (concat ["character"] hazard-secondaries shared-secondaries resource-secondaries ["site"] ["region"])
      "Character" ["character" "Avatar" "Agent"]
      "Resource" (concat resource-secondaries shared-secondaries)
      "Hazard" (concat hazard-secondaries shared-secondaries)
      "Site" ["site"]
      "Region" ["region"]))

(defn alignments [primary]
    (case primary
      "All" (concat general-alignments ["Neutral"])
      "Character" (concat general-alignments ["Neutral"])
      "Resource" general-alignments
      "Hazard" ["Neutral"]
      "Site" general-alignments
      "Region" ["Neutral"]))

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
    (filter #(if (= (.indexOf (str/strip-accents (.toLowerCase (:NameEN %))) query) -1) false true) cards)))

(defn sort-field [fieldname]
  (case fieldname
    "Set" #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:Set %))
    "Name" (juxt :NameEN #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:Set %)))
    "Primary" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:Set %))
                    #((into {} (map-indexed (fn [i e] [e i]) primary-order)) (:Primary %)))
    "Alignment" (juxt #((into {} (map-indexed (fn [i e] [e i]) set-order)) (:Set %))
                      #((into {} (map-indexed (fn [i e] [e i]) (concat general-alignments ["Neutral"]))) (:Alignment %)))))

(defn handle-scroll [e owner {:keys [page]}]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.innerHeight $cardlist))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (om/update-state! owner :page inc))))

(defn handle-search [e owner]
  (doseq [filter [:set-filter :secondary-filter :sort-filter :alignment-filter]]
    (om/set-state! owner filter "All"))
  (om/set-state! owner :search-query (.. e -target -value)))

(defn card-browser [{:keys [sets] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:search-query ""
       :sort-field "Set"
       :set-filter "All"
       :primary-filter "All"
       :alignment-filter "All"
       :secondary-filter "All"
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
        [:div.filters#left-bar
         [:div.blue-shade.panel.filters
          (let [query (:search-query state)]
            [:div.search-box
             [:span.e.search-icon {:dangerouslySetInnerHTML #js {:__html "&#xe822;"}}]
             (when-not (empty? query)
               [:span.e.search-clear {:dangerouslySetInnerHTML #js {:__html "&#xe819;"}
                                      :on-click #(om/set-state! owner :search-query "")}])
             [:input.search {:on-change #(handle-search % owner)
                             :NameEN "text" :placeholder "Search cards" :value query}]])
          [:div
           [:h4 "Sort by"]
           [:select {:value (:sort-filter state)
                     :on-change #(om/set-state! owner :sort-field (.trim (.. % -target -value)))}
            (for [field ["Set" "Name" "Primary" "Alignment"]]
              [:option {:value field} field])]]

          (let [sets-list-all (map #(assoc % :code (str (:code %))
                                             :position %
                                             :position 0)
                                   sets)
                sets-list (filter #(not (= (:size %) 1)) sets-list-all)]

            (for [filter [["Set" :set-filter (map :code
                                                  (sort-by (juxt :position)
                                                           sets-list-all))]
                          ["Primary" :primary-filter ["Character" "Resource" "Hazard" "Site" "Region"]]
                          ["Alignment" :alignment-filter (alignments (:primary-filter state))]
                          ["Secondary" :secondary-filter (secondaries (:primary-filter state))]]]
              [:div
               [:h4 (first filter)]
               [:select {:value ((second filter) state)
                         :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
                (options (last filter))]]))]

         [:div.blue-shade.panel.filters
          (om/build card-info-view nil)
          ]]

        [:div.card-list {:on-scroll #(handle-scroll % owner state)}
         (om/build-all card-view
                       (let [s (-> (:set-filter state)
                                     (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" ""))
                             list-sets (set (for [x sets :when (= (:code x) s)] (:code x)))
                             cards (if (= s "All")
                                     (:cards cursor)
                                     (if (= (.indexOf (:set-filter state) "Set") -1)
                                       (filter #(= (:Set %) s) (:cards cursor))
                                       (filter #(list-sets (:position %)) (:cards cursor))))]
                         (->> cards
                              (filter-cards (:primary-filter state) :Primary)
                              (filter-cards (:alignment-filter state) :Alignment)
                              (filter-cards (:secondary-filter state) :Secondary)
                              (match (str/strip-accents (.toLowerCase (:search-query state))))
                              ;;(match (:search-query state))
                              (sort-by (sort-field (:sort-field state)))
                              (take (* (:page state) 28))))
                       {:key :code})]]))))

(om/root card-browser
         app-state
         {:shared {:notif-chan notif-chan
                   :pub-chan   pub-chan}
          :target (. js/document (getElementById "cardbrowser"))})
