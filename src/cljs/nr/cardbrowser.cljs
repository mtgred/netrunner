(ns nr.cardbrowser
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! >! sub pub] :as async]
            [clojure.string :as s]
            [jinteki.cards :refer [all-cards] :as cards]
            [nr.appstate :refer [app-state]]
            [nr.account :refer [alt-art-name]]
            [nr.ajax :refer [GET]]
            [nr.utils :refer [toastr-options banned-span restricted-span rotated-span
                              influence-dots slug->format format->slug render-icons]]
            [reagent.core :as r]))

(def cards-channel (chan))
(def pub-chan (chan))
(def notif-chan (pub pub-chan :topic))

(def browser-state (atom {}))

(declare generate-previous-cards)

(go (let [server-version (get-in (<! (GET "/data/cards/version")) [:json :version])
          local-cards (js->clj (.parse js/JSON (.getItem js/localStorage "cards")) :keywordize-keys true)
          need-update? (or (not local-cards) (not= server-version (:version local-cards)))
          cards (sort-by :code
                         (if need-update?
                           (:json (<! (GET "/data/cards")))
                           (:cards local-cards)))
          sets (:json (<! (GET "/data/sets")))
          cycles (:json (<! (GET "/data/cycles")))
          mwls (:json (<! (GET "/data/mwl")))
          latest-mwl (->> mwls
                          (filter #(= "standard" (:format %)))
                          (map (fn [e] (update e :date-start #(js/Date.parse %))))
                          (sort-by :date-start)
                          last)
            alt-cards (->> cards
                       (map #(select-keys % [:title :code :alt_art]))
                       (filter :alt_art)
                       (map (juxt :code identity))
                       (into {}))
          alt-info (->> (<! (GET "/data/cards/altarts"))
                        (:json)
                        (map #(select-keys % [:version :name :description :position])))]
      (reset! cards/mwl latest-mwl)
      (reset! cards/sets sets)
      (reset! cards/cycles cycles)
      (swap! app-state assoc :sets sets :cycles cycles)
      (when need-update?
        (.setItem js/localStorage "cards" (.stringify js/JSON (clj->js {:cards cards :version server-version}))))
      (reset! all-cards cards)
      (swap! app-state assoc
             :cards-loaded true :previous-cards (generate-previous-cards cards)
             :alt-info alt-info :alt-cards alt-cards)
      (put! cards-channel cards)))

(defn- expand-one
  "Reducer function to create a previous card from a newer card definition."
  [acc version c]
  (let [number (js/parseInt (subs version 3))
        cycle-pos (js/parseInt (subs version 0 2))
        prev-set (first (filter #(= cycle-pos (:cycle_position %1)) @cards/sets))
        prev (assoc c
                    :code version
                    :rotated true
                    :cycle_code (:cycle_code prev-set)
                    :setname (:name prev-set)
                    :set_code (:id prev-set)
                    :number number
                    :future-version (:code c))
        prev (dissoc prev :previous-versions :alt_art)]
    (conj acc prev)))

(defn- expand-previous
  "Reducer function to expand a card with :previous-cards defined"
  [acc c]
  (reduce #(expand-one %1 %2 c) acc (:previous-versions c)))

(defn generate-previous-cards
  "The cards database only has the latest version of a card. Create stubs for previous versions of a card for display purposes."
  [cards]
  (let [c (filter #(contains? % :previous-versions) cards)]
    (reduce expand-previous `() c)))

(defn make-span [text sym icon-class]
  (s/replace text (js/RegExp. sym "gi") (str "<span class='anr-icon " icon-class "'></span>")))

(defn show-alt-art?
  "Is the current user allowed to use alternate art cards and do they want to see them?"
  ([] (show-alt-art? false))
  ([allow-all-users]
   (and (get-in @app-state [:options :show-alt-art] true)
        (or allow-all-users
            (get-in @app-state [:user :special] false)))))

(defn image-url
  ([card] (image-url card false))
  ([card allow-all-users]
   (let [art (or (:art card) ; use the art set on the card itself, or fall back to the user's preferences.
                 (get-in @app-state [:options :alt-arts (keyword (:code card))]))
         alt-card (get (:alt-cards @app-state) (:code card))
         has-art (and (show-alt-art? allow-all-users) art)
         version-path (if has-art
                        (get (:alt_art alt-card) (keyword art) art)
                        (:code card))]
     (str "/img/cards/" version-path ".png"))))

(defn- base-image-url
  "The default card image. Displays an alternate image if the card is specified as one."
  [card]
  (let [path (if (keyword? (:art card))
                 (get-in card [:alt_art (:art card)] (:code card))
                 (:code card))]
    (str "/img/cards/" path ".png")))

(defn- alt-version-from-string
  "Given a string name, get the keyword version or nil"
  [setname]
  (when-let [alt (some #(when (= setname (:name %)) %) (:alt-info @app-state))]
    (keyword (:version alt))))

(defn- expand-alts
  [only-version acc card]
  (let [alt-card (get (:alt-cards @app-state) (:code card))
        alt-only (alt-version-from-string only-version)
        alt-keys (keys (:alt_art alt-card))
        alt-arts (if alt-only
                   (filter #(= alt-only %) alt-keys)
                   alt-keys)]
    (if (and alt-arts
             (show-alt-art? true))
      (->> alt-arts
           (concat [""])
           (map #(if % (assoc card :art %) card))
           (map #(if (not= "" (:art %)) (dissoc % :previous-versions) %))
           (concat acc))
      (conj acc card))))

(defn- insert-alt-arts
  "Add copies of alt art cards to the list of cards. If `only-version` is nil, all alt versions will be added."
  [only-version cards]
  (reduce (partial expand-alts only-version) () (reverse cards)))

(defn non-game-toast
  "Display a toast warning with the specified message."
  [msg type options]
  (set! (.-options js/toastr) (toastr-options options))
  (let [f (aget js/toastr type)]
    (f msg)))

(defn- post-response [response]
  (if (= 200 (:status response))
    (let [new-alts (get-in response [:json :altarts] {})]
      (swap! app-state assoc-in [:user :options :alt-arts] new-alts)
      (non-game-toast "Updated Art" "success" nil))
    (non-game-toast "Failed to Update Art" "error" nil)))

(defn- future-selected-alt-art [card]
  (let [future-code (keyword (:future-version card))
        selected-alts (:alt-arts (:options @app-state))
        selected-art (get selected-alts future-code)]
    (= (:code card) selected-art)))

(defn- previous-selected-alt-art [card]
  (let [selected-alts (:alt-arts (:options @app-state))
        selected-art (get selected-alts (keyword (:code card)))]
    (nil? selected-art)))

(defn- selected-alt-art [card]
  (cond (contains? card :future-version) (future-selected-alt-art card)
        (contains? card :previous-versions) (previous-selected-alt-art card)
        :else
        (let [code (keyword (:code card))
              alt-card (get (:alt-cards @app-state) (:code card))
              selected-alts (:alt-arts (:options @app-state))
              selected-art (keyword (get selected-alts code))
              card-art (:art card)]
          (or (and card-art (nil? selected-art) (= "" card-art))
              (and alt-card selected-art (= card-art selected-art))))))

;; Alts can only be set on th most recent version of a card
;; So if the card has a :future-version key, we apply the alt to
;; that card, setting the alt to the code of the old card.
(defn- select-alt-art [card]
  (let [is-old-card (contains? card :future-version)
        art (:art card)
        code-kw (keyword (:future-version card (:code card)))
        alts (:alt-arts (:options @app-state))
        new-alts (cond
                   (keyword? art) (assoc alts code-kw (name art))
                   is-old-card (assoc alts code-kw (:code card))
                   true (dissoc alts code-kw))] ; remove the key entirely if the newest card is selected
    (swap! app-state assoc-in [:options :alt-arts] new-alts)
    (nr.account/post-options "/profile" (partial post-response))))

(defn- text-class-for-status
  [status]
  (case (keyword status)
    (:legal :restricted) "legal"
    :rotated "casual"
    :banned "invalid"
    "casual"))

(defn- card-as-text
  "Generate text html representation a card"
  [card]
  [:div
   [:h4 (:title card)]
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
         {:class (-> faction s/lower-case (s/replace " " "-"))}
         (influence-dots influence)]]))
   [:div.text
    [:p [:span.type (str (:type card))]
     (if (empty? (:subtype card)) "" (str ": " (:subtype card)))]
    [:pre (render-icons (:text (first (filter #(= (:title %) (:title card)) @all-cards))))]

    [:div.formats
     (doall (for [[k name] (-> slug->format butlast)]
              (let [status (keyword (get-in card [:format (keyword k)] "unknown"))
                    c (text-class-for-status status)]
                ^{:key k}
                [:div {:class c} name
                 (case status
                   :banned banned-span
                   :restricted restricted-span
                   :rotated rotated-span
                   nil)])))]

    [:div.pack
     (when-let [pack (:setname card)]
       (when-let [number (:number card)]
         (str pack " " number
              (when-let [art (:art card)]
                (str " [" (alt-art-name art) "]")))))]
    (when (show-alt-art?)
      (if (selected-alt-art card)
        [:div.selected-alt "Selected Alt Art"]
        (when (or (:art card) (:previous-versions card) (:future-version card))
          [:button.alt-art-selector
           {:on-click #(select-alt-art card)}
           "Select Art"])))]])

(defn types [side]
  (let [runner-types ["Identity" "Program" "Hardware" "Resource" "Event"]
        corp-types ["Agenda" "Asset" "ICE" "Operation" "Upgrade"]]
    (case side
      "All" (concat runner-types corp-types)
      "Runner" runner-types
      "Corp" (cons "Identity" corp-types))))

(defn factions [side]
  (let [runner-factions ["Anarch" "Criminal" "Shaper" "Adam" "Apex" "Sunny Lebeau"]
        corp-factions ["Jinteki" "Haas-Bioroid" "NBN" "Weyland Consortium" "Neutral"]]
    (case side
      "All" (concat runner-factions corp-factions)
      "Runner" (conj runner-factions "Neutral")
      "Corp" corp-factions)))

(defn- filter-alt-art-cards [cards]
  (let [alt-arts (:alt-cards @app-state)]
    (filter #(or (contains? alt-arts (:code %))
                 (contains? % :future-version)
                 (contains? % :previous-versions))
            cards)))

(defn- filter-alt-art-set [setname cards]
  (when-let [alt-key (alt-version-from-string setname)]
    (if (= alt-key :prev)
      (filter #(or (contains? % :future-version) (contains? % :previous-versions)) cards)
      (let [sa (map first
                    (filter (fn [[k v]] (contains? (:alt_art v) alt-key)) (:alt-cards @app-state)))]
        (filter (fn [c] (some #(= (:code c) %) sa)) cards)))))

(defn filter-cards [filter-value field cards]
  (if (= filter-value "All")
    cards
    (filter #(= (get % field) filter-value) cards)))

(defn filter-format [fmt cards]
  (if (= "All" fmt)
    cards
    (let [fmt (keyword (get format->slug fmt))]
      (filter #(= "legal" (get-in % [:format fmt])) cards))))

(defn filter-title [query cards]
  (if (empty? query)
    cards
    (let [lcquery (s/lower-case query)]
      (filter #(or (s/includes? (s/lower-case (:title %)) lcquery)
                   (s/includes? (:normalizedtitle %) lcquery))
              cards))))

(defn sort-field [fieldname]
  (case fieldname
    "Name" :title
    "Influence" (juxt :factioncost :side :faction :title)
    "Cost" (juxt :cost :title)
    "Faction" (juxt :side :faction :title)
    "Type" (juxt :side :type :faction :title)
    "Set number" :number))

(defn selected-set-name [state]
  (-> (:set-filter @state)
      (s/replace "&nbsp;&nbsp;&nbsp;&nbsp;" "")
      (s/replace " Cycle" "")))

(defn handle-scroll [e state]
  (let [$cardlist (js/$ ".card-list")
        height (- (.prop $cardlist "scrollHeight") (.prop $cardlist "clientHeight"))]
    (when (> (.scrollTop $cardlist) (- height 600))
      (swap! state update-in [:page] (fnil inc 0)))))

(defn- card-view [card state]
  (let [cv (r/atom {:show-text false})]
    (fn [card state]
      [:div.card-preview.blue-shade
       {:on-click #(do (.preventDefault %)
                       (if (= card (:selected-card @state))
                         (swap! state dissoc :selected-card)
                         (swap! state assoc :selected-card card)))
        :class (if (:decorate-card @state)
                 (cond (= (:selected-card @state) card) "selected"
                       (and (show-alt-art?) (selected-alt-art card)) "selected-alt")
                 nil)}
       (if (or (= card (:selected-card @state))
               (:show-text @cv))
         [card-as-text card]
         (when-let [url (base-image-url card)]
           [:img {:src url
                  :alt (:title card)
                  :onError #(-> (swap! cv assoc :show-text true))
                  :onLoad #(-> % .-target js/$ .show)}]))])))

(defn card-list-view [state]
  (let [selected (selected-set-name state)
        selected-cycle (-> selected s/lower-case (s/replace " " "-"))
        combined-cards (concat @all-cards (:previous-cards @app-state))
        [alt-filter cards] (cond
                             (= selected "All") [nil combined-cards]
                             (= selected "Alt Art") [nil (filter-alt-art-cards combined-cards)]
                             (s/ends-with? (:set-filter @state) " Cycle") [nil (filter #(= (:cycle_code %) selected-cycle) combined-cards)]
                             (not (some #(= selected (:name %)) (:sets @app-state))) [selected (filter-alt-art-set selected combined-cards)]
                             :else
                             [nil (filter #(= (:setname %) selected) combined-cards)])
        cards (->> cards
                   (filter-cards (:side-filter @state) :side)
                   (filter-cards (:faction-filter @state) :faction)
                   (filter-cards (:type-filter @state) :type)
                   (filter-format (:format-filter @state))
                   (filter-title (:search-query @state))
                   (insert-alt-arts alt-filter)
                   (sort-by (sort-field (:sort-field @state)))
                   (take (* (:page @state) 28)))]
    [:div.card-list {:on-scroll #(handle-scroll % state)}
     (doall
       (for [card cards]
         ^{:key (str (image-url card true) "-" (:code card))}
         [card-view card state]))]))

(defn handle-search [e state]
  (doseq [filter [:set-filter :type-filter :faction-filter]]
    (swap! state assoc filter "All"))
  (swap! state assoc :sort-field "Faction")
  (swap! state assoc :search-query (.. e -target -value)))

(defn query-builder [state]
  (let [query (:search-query @state)]
    [:div.search-box
     [:span.e.search-icon {:dangerouslySetInnerHTML #js {:__html "&#xe822;"}}]
     (when-not (empty? query)
       [:span.e.search-clear {:dangerouslySetInnerHTML #js {:__html "&#xe819;"}
                              :on-click #(swap! state assoc :search-query "")}])
     [:input.search {:on-change #(handle-search % state)
                     :type "text"
                     :placeholder "Search cards"
                     :value query}]]))

(defn sort-by-builder [state]
  [:div
   [:h4 "Sort by"]
   [:select {:value (:sort-field @state)
             :on-change #(swap! state assoc :sort-field (.. % -target -value))}
    (for [field ["Faction" "Name" "Type" "Influence" "Cost" "Set number"]]
      [:option {:value field
                :key field
                :dangerouslySetInnerHTML #js {:__html field}}])]])

(defn simple-filter-builder
  [title state state-key options]
  [:div
   [:h4 title]
   [:select {:value (get @state state-key)
             :on-change #(swap! state assoc state-key (.. % -target -value))}
    (for [option (cons "All" options)]
      ^{:key option}
      [:option {:value option
                :key option
                :dangerouslySetInnerHTML #js {:__html option}}])]])


(defn format-set-name [pack-name]
  (str "&nbsp;&nbsp;&nbsp;&nbsp;" pack-name))


(defn dropdown-builder
  [state]
  (let [sets (r/cursor app-state [:sets])
        cycles (r/cursor app-state [:cycles])
        cycles-list-all (map #(assoc % :name (str (:name %) " Cycle")
                                     :cycle_position (:position %)
                                     :position 0)
                             @cycles)
        cycles-list (filter #(not (= (:size %) 1)) cycles-list-all)
        sets-list (map #(if (not (or (:bigbox %)
                                     (= (:id %) (:cycle_code %))))
                          (update-in % [:name] format-set-name)
                          %)
                       @sets)
        set-names (map :name
                       (sort-by (juxt :cycle_position :position)
                                (concat cycles-list sets-list)))
        alt-art-sets (cons "Alt Art"
                           (map #(format-set-name (:name %))
                                (sort-by :position (:alt-info @app-state))))
        sets-to-display (if (show-alt-art? true)
                          (concat set-names alt-art-sets)
                          set-names)
        formats (-> format->slug keys butlast)]
    [:div
     (doall
       (for [[title state-key options]
             [["Format" :format-filter formats]
              ["Set" :set-filter sets-to-display]
              ["Side" :side-filter ["Corp" "Runner"]]
              ["Faction" :faction-filter (factions (:side-filter @state))]
              ["Type" :type-filter (types (:side-filter @state))]]]
         ^{:key title}
         [simple-filter-builder title state state-key options]))]))

(defn clear-filters [state]
  [:p [:button
       {:key "clear-filters"
        :on-click #(swap! state assoc
                          :search-query ""
                          :sort-field "Faction"
                          :format-filter "All"
                          :set-filter "All"
                          :type-filter "All"
                          :side-filter "All"
                          :faction-filter "All")}
       "Clear"]])

(defn card-browser []
  (r/with-let [active (r/cursor app-state [:active-page])]
    (when (= "/cards" (first @active))
      (let [state (r/atom {:search-query ""
                           :sort-field "Faction"
                           :format-filter "All"
                           :set-filter "All"
                           :type-filter "All"
                           :side-filter "All"
                           :faction-filter "All"
                           :page 1
                           :decorate-card true
                           :selected-card nil})]
        (fn []
          (.focus (js/$ ".search"))
          [:div.cardbrowser
           [:div.blue-shade.panel.filters
            [query-builder state]
            [sort-by-builder state]
            [dropdown-builder state]
            [clear-filters state]]
           [card-list-view state]])))))
