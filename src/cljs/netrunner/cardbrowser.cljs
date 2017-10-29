(ns netrunner.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [netrunner.appstate :refer [app-state]]
            [netrunner.ajax :refer [GET]]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

;; Load in sets and mwl lists
(go (let [sets (:json (<! (GET "/data/sets")))
          cycles (:json (<! (GET "/data/cycles")))
          mwl (:json (<! (GET "/data/mwl")))
          latest_mwl (->> mwl
                       (map (fn [e] (update e :date_start #(js/Date %))))
                       (sort-by :date_start)
                       (last))]
      (swap! app-state assoc :sets sets :mwl latest_mwl :cycles cycles)))

(go (let [cards (sort-by :code (:json (<! (GET "/data/cards"))))]
      (swap! app-state assoc :cards cards)
      (swap! app-state assoc :cards-loaded true)
      (put! cards-channel cards)))

(defn make-span [text symbol class]
  (.replace text (js/RegExp. symbol "gi") (str "<span class='anr-icon " class "'></span>")))

(defn show-alt-art?
  "Is the current user allowed to use alternate art cards and do they want to see them?"
  []
  (and
    (get-in @app-state [:options :show-alt-art] true)
    (get-in @app-state [:user :special] false)))

(defn image-url [card]
  (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                (get-in @app-state [:options :alt-arts (keyword (:code card))]))
        alt-card (get (:alt-arts @app-state) (:code card))
        has-art (and (show-alt-art?)
                     art
                     (contains? (:alt_art alt-card) (keyword art)))
        version-path (if has-art
                       (get (:alt_art alt-card) (keyword art) (:code card))
                       (:code card))]
    (str "/img/cards/" version-path ".png")))

(defn insert-alt-arts
  "Add copies of all alt art cards to the list of cards"
  [cards]
  (reduce netrunner.deckbuilder/expand-alts () (reverse cards)))

(defn add-symbols [card-text]
  (-> (if (nil? card-text) "" card-text)
      (make-span "\\[Credits\\]" "credit")
      (make-span "\\[Credit\\]" "credit")
      (make-span "\\[Click\\]" "click")
      (make-span "\\[Subroutine\\]" "subroutine")
      (make-span "\\[Recurring Credits\\]" "recurring-credit")
      (make-span "\\[recurring-credit\\]" "recurring-credit")
      (make-span "1\\[Memory Unit\\]" "mu1")
      (make-span "2\\[Memory Unit\\]" "mu2")
      (make-span "3\\[Memory Unit\\]" "mu3")
      (make-span "\\[Memory Unit\\]" "mu")
      (make-span "1\\[mu\\]" "mu1")
      (make-span "2\\[mu\\]" "mu2")
      (make-span "3\\[mu\\]" "mu3")
      (make-span "\\[mu\\]" "mu")
      (make-span "\\[Link\\]" "link")
      (make-span "\\[Trash\\]" "trash")
      (make-span "\\[adam\\]" "adam")
      (make-span "\\[anarch\\]" "anarch")
      (make-span "\\[apex\\]" "apex")
      (make-span "\\[criminal\\]" "criminal")
      (make-span "\\[hb\\]" "haas-bioroid")
      (make-span "\\[haas-bioroid\\]" "haas-bioroid")
      (make-span "\\[jinteki\\]" "jinteki")
      (make-span "\\[nbn\\]" "nbn")
      (make-span "\\[shaper\\]" "shaper")
      (make-span "\\[sunny\\]" "sunny")
      (make-span "\\[weyland\\]" "weyland-consortium")
      (make-span "\\[weyland-consortium\\]" "weyland-consortium")))

(defn- post-response [cursor response]
  (if (= 200 (:status response))
    (let [new-alts (get-in response [:json :altarts] {})]
      (swap! app-state assoc-in [:user :options :alt-arts] new-alts)
      (netrunner.gameboard/toast "Updated Art" "success" nil))
    (netrunner.gameboard/toast "Failed to Update Art" "error" nil)))

(defn selected-alt-art [card cursor]
  (let [code (keyword (:code card))
        alt-card (get (:alt-arts @app-state) (name code) nil)
        selected-alts (:alt-arts (:options cursor))
        selected-art (keyword (get selected-alts code nil))
        card-art (:art card)]
  (and alt-card
       (cond
         (= card-art selected-art) true
         (and (nil? selected-art)
              (not (keyword? card-art))) true
         (and (= :default selected-art)
              (not (keyword? card-art))) true
         :else false))))

(defn select-alt-art [card cursor]
  (when-let [art (:art card)]
    (let [code (keyword (:code card))
          alts (:alt-arts (:options cursor))
          new-alts (if (keyword? art)
                     (assoc alts code (name art))
                     (dissoc alts code))]
      (om/update! cursor [:options :alt-arts] new-alts)
      (netrunner.account/post-options "/update-profile" (partial post-response cursor)))))

(defn- card-text
  "Generate text html representation a card"
  [card cursor]
  [:div
   [:h4 (str (:title card) " ")
    [:span.influence
     {:class (if-let [faction (:faction card)]
               (-> faction .toLowerCase (.replace " " "-"))
               "neutral")}
     (when (netrunner.deckbuilder/banned? card) netrunner.deckbuilder/banned-span)
     (when (netrunner.deckbuilder/restricted? card) netrunner.deckbuilder/restricted-span)
     (when (:rotated card) netrunner.deckbuilder/rotated-span)]]
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
         {:dangerouslySetInnerHTML #js {:__html (netrunner.deckbuilder/influence-dots influence)}
          :class                   (-> faction .toLowerCase (.replace " " "-"))}]]))
   [:div.text
    [:p [:span.type (str (:type card))] (if (empty? (:subtype card))
                                                   "" (str ": " (:subtype card)))]
    [:pre {:dangerouslySetInnerHTML #js {:__html (add-symbols (:text card))}}]
    [:div.pack
     (when-let [pack (:setname card)]
       (when-let [number (:number card)]
         (str pack " " number
              (when-let [art (:art card)]
                (str " [" (netrunner.account/alt-art-name art) "]")))))]
     (if (selected-alt-art card cursor)
      [:div.selected-alt "Selected Alt Art"]
      (when (:art card)
        [:button.alt-art-selector
         {:on-click #(select-alt-art card cursor)}
         "Select Art"]))
    ]])

(defn card-view [card owner]
  (reify
    om/IInitState
    (init-state [_] {:showText false})
    om/IRenderState
    (render-state [_ state]
      (let [cursor (om/get-state owner :cursor)]
      (sab/html
        [:div.card-preview.blue-shade
         (when (om/get-state owner :decorate-card)
           {:class (cond (:selected card) "selected"
                         (selected-alt-art card cursor) "selected-alt")})
         (if (:showText state)
           (card-text card cursor)
           (when-let [url (image-url card)]
             [:img {:src url
                    :onClick #(do (.preventDefault %)
                                (put! (:pub-chan (om/get-shared owner))
                                      {:topic :card-selected :data card})
                                nil)
                    :onError #(-> (om/set-state! owner {:showText true}))
                    :onLoad #(-> % .-target js/$ .show)}]))])))))

(defn card-info-view [cursor owner]
  (reify
    om/IRenderState
    (render-state [_ state]
      (sab/html
        (let [selected-card (om/get-state owner :selected-card)]
        (if (nil? selected-card)
          [:div {:display "none"}]
          [:div
           [:h4 "Card text"]
           [:div.blue-shade.panel
            (card-text selected-card cursor)]]))))))

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

(defn filter-alt-art-cards [cards]
  (let [alt-arts (:alt-arts @app-state)]
    (filter #(contains? alt-arts (:code %)) cards)))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (field %) filter-value) cards)))

(defn filter-rotated [should-filter cards]
  (if should-filter
    (filter-cards false :rotated cards)
    cards))

(defn filter-title [query cards]
  (if (empty? query)
    cards
    (let [lcquery (.toLowerCase query)]
      (filter #(or (not= (.indexOf (.toLowerCase (:title %)) lcquery) -1)
                   (not= (.indexOf (:normalizedtitle %) lcquery) -1))
              cards))))

(defn sort-field [fieldname]
  (case fieldname
    "Name" :title
    "Influence" :factioncost
    "Cost" :cost
    "Faction" (juxt :side :faction :code)
    "Type" (juxt :side :type)
    "Set number" :number))

(defn selected-set-name [state]
  (-> (:set-filter state)
    (.replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
    (.replace " Cycle" "")))

(defn selected-set-rotated? [{:keys [sets cycles]} state]
  (let [s (selected-set-name state)
        combined (concat sets cycles)]
    (if (= s "All")
      false
      (->> combined
        (filter #(= s (:name %)))
        (first)
        (:rotated)))))

(defn handle-scroll [e owner {:keys [page]}]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.innerHeight $cardlist))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (om/update-state! owner :page inc))))

(defn handle-search [e owner]
  (doseq [filter [:set-filter :type-filter :sort-filter :faction-filter]]
    (om/set-state! owner filter "All"))
  (om/set-state! owner :search-query (.. e -target -value)))

(defn card-browser [{:keys [sets cycles] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:search-query ""
       :sort-field "Faction"
       :set-filter "All"
       :type-filter "All"
       :side-filter "All"
       :faction-filter "All"
       :hide-rotated true
       :page 1
       :filter-ch (chan)
       :selected-card nil})

    om/IWillMount
    (will-mount [this]
      (go (while true
            (let [f (<! (om/get-state owner :filter-ch))]
              (om/set-state! owner (:filter f) (:value f))))))

    om/IDidMount
    (did-mount [_]
      (let [events (sub (:notif-chan (om/get-shared owner)) :card-selected (chan))]
        (go
          (loop [e (<! events)]
            (om/set-state! owner :selected-card (:data e))
            (recur (<! events))))))

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

         (let [hide-rotated (:hide-rotated state)
               cycles-filtered (filter-rotated hide-rotated cycles)
               cycles-list-all (map #(assoc % :name (str (:name %) " Cycle")
                                            :cycle_position (:position %)
                                            :position 0)
                                    cycles-filtered)
               cycles-list (filter #(not (= (:size %) 1)) cycles-list-all)
               sets-filtered (filter-rotated hide-rotated sets)
               ;; Draft is specified as a cycle, but contains no set, nor is it marked as a bigbox
               ;; so we handled it specifically here for formatting purposes
               sets-list (map #(if (not (or (:bigbox %) (= (:name %) "Draft")))
                                  (update-in % [:name] (fn [name] (str "&nbsp;&nbsp;&nbsp;&nbsp;" name)))
                                  %)
                               sets-filtered)
               set-names (map :name
                              (sort-by (juxt :cycle_position :position)
                                       (concat cycles-list sets-list)))]
           (for [filter [["Set" :set-filter (if (show-alt-art?)
                                              (concat set-names (list "Alt Art"))
                                              set-names)]
                         ["Side" :side-filter ["Corp" "Runner"]]
                         ["Faction" :faction-filter (factions (:side-filter state))]
                         ["Type" :type-filter (types (:side-filter state))]]]
             [:div
              [:h4 (first filter)]
              [:select {:value ((second filter) state)
                        :on-change #(om/set-state! owner (second filter) (.. % -target -value))}
               (options (last filter))]]))

         [:div.hide-rotated-div
          [:label [:input.hide-rotated {:type "checkbox"
                           :value true
                           :checked (om/get-state owner :hide-rotated)
                           :on-change #(let [hide (.. % -target -checked)]
                                         (om/set-state! owner :hide-rotated hide)
                                         (when (and hide (selected-set-rotated? cursor state))
                                           (om/set-state! owner :set-filter "All"))
                                         )}]
           "Hide rotated cards"]]

         (om/build card-info-view cursor {:state {:selected-card (:selected-card state)}})
         ]

        [:div.card-list {:on-scroll #(handle-scroll % owner state)}
         (om/build-all card-view
                       (let [s (selected-set-name state)
                             cycle-sets (set (for [x sets :when (= (:cycle x) s)] (:name x)))
                             cards (cond
                                     (= s "All") (:cards cursor)
                                     (= s "Alt Art") (filter-alt-art-cards (:cards cursor))
                                     :else
                                     (if (= (.indexOf (:set-filter state) "Cycle") -1)
                                       (filter #(= (:setname %) s) (:cards cursor))
                                       (filter #(cycle-sets (:setname %)) (:cards cursor))))]
                         (->> cards
                              (filter-cards (:side-filter state) :side)
                              (filter-cards (:faction-filter state) :faction)
                              (filter-cards (:type-filter state) :type)
                              (filter-rotated (:hide-rotated state))
                              (filter-title (:search-query state))
                              (insert-alt-arts)
                              (sort-by (sort-field (:sort-field state)))
                              (take (* (:page state) 28))))
                       {:key-fn #(str (:setname %) (:code %) (:art %))
                        :fn #(assoc % :selected (and (= (:setname %) (:setname (:selected-card state)))
                                                     (= (:code %) (:code (:selected-card state)))
                                                     (= (:art %) (:art (:selected-card state)))))
                        :state {:cursor cursor :decorate-card true}
                        })]]))))

(om/root card-browser
         app-state
         {:shared {:notif-chan notif-chan
                   :pub-chan   pub-chan}
          :target (. js/document (getElementById "cardbrowser"))})
