(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :refer [split split-lines join escape] :as s]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.cardbrowser :refer [cards-channel image-url card-view show-alt-art? filter-title expand-alts] :as cb]
            [netrunner.account :refer [load-alt-arts]]
            [netrunner.ajax :refer [POST GET DELETE PUT]]
            [goog.string :as gstring]
            [goog.string.format]
            [jinteki.utils :refer [str->int INFINITY] :as utils]
            [jinteki.cards :refer [all-cards]]
            [jinteki.decks :as decks]
            [jinteki.cards :as cards]))

(def select-channel (chan))
(def zoom-channel (chan))

(defn num->percent
  "Converts an input number to a percent of the second input number for display"
  [num1 num2]
  (if (zero? num2)
    "0"
    (gstring/format "%.0f" (* 100 (float (/ num1 num2))))))

(defn identical-cards? [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))




(defn noinfcost? [identity card]
  (or (= (:faction card) (:faction identity))
      (= 0 (:factioncost card)) (= INFINITY (decks/id-inf-limit identity))))

(defn take-best-card
  "Returns a non-rotated card from the list of cards or a random rotated card from the list"
  [cards]
  (let [non-rotated (filter #(not (:rotated %)) cards)]
    (if (not-empty non-rotated)
      (first non-rotated)
      (first cards))))

(defn filter-exact-title [query cards]
  (let [lcquery (.toLowerCase query)]
    (filter #(or (= (.toLowerCase (:title %)) lcquery)
                 (= (:normalizedtitle %) lcquery))
            cards)))

(defn lookup
  "Lookup the card title (query) looking at all cards on specified side"
  [side card]
  (let [q (.toLowerCase (:title card))
        id (:id card)
        cards (filter #(= (:side %) side) @all-cards)
        exact-matches (filter-exact-title q cards)]
    (cond (and id
               (first (filter #(= id (:code %)) cards)))
          (first (filter #(= id (:code %)) cards))
          (not-empty exact-matches) (take-best-card exact-matches)
          :else
          (loop [i 2 matches cards]
            (let [subquery (subs q 0 i)]
              (cond (zero? (count matches)) card
                    (or (= (count matches) 1) (identical-cards? matches)) (take-best-card matches)
                    (<= i (count (:title card))) (recur (inc i) (filter-title subquery matches))
                    :else card))))))

(defn- build-identity-name
  [title setname art]
  (let [set-title (if setname (str title " (" setname ")") title)]
    (if art
      (str set-title " [" art "]")
      set-title)))

(defn parse-identity
  "Parse an id to the corresponding card map"
  [{:keys [side title art setname]}]
  (if (nil? title)
    {:display-name "Missing Identity"}
    (let [card (lookup side {:title title})]
      (assoc card :art art :display-name (build-identity-name title setname art)))))

(defn add-params-to-card
  "Add art and id parameters to a card hash"
  [card id art]
  (-> card
    (assoc :art art)
    (assoc :id id)))

(defn- clean-param
  "Parse card parameter key value pairs from a string"
  [param]
  (if (and param
           (= 2 (count param)))
    (let [[k v] (map s/trim param)
          allowed-keys '("id" "art")]
      (if (some #{k} allowed-keys)
        [(keyword k) v]
        nil))
    nil))

(defn- param-reducer
  [acc param]
  (if param
    (assoc acc (first param) (second param))
    acc))

(defn- add-params
  "Parse a string of parameters and add them to a map"
  [result params-str]
  (if params-str
    (let [params-groups (split params-str #"\,")
          params-all (map #(split % #":") params-groups)
          params-clean (map #(clean-param %) params-all)]
      (reduce param-reducer result params-clean))
    result))

(defn parse-line
  "Parse a single line of a deck string"
  [line]
  (let [clean (s/trim line)
        [_ qty-str card-name _ card-params] (re-matches #"(\d+)[^\s]*\s+([^\[]+)(\[(.*)\])?" clean)]
    (if (and qty-str
             (not (js/isNaN (str->int qty-str)))
             card-name)
      (let [result (assoc {} :qty (str->int qty-str) :card (s/trim card-name))]
        (add-params result card-params))
      nil)))

(defn- line-reducer
  "Reducer function to parse lines in a deck string"
  [acc line]
  (if-let [card (parse-line line)]
    (conj acc card)
    acc))

(defn deck-string->list
  "Turn a raw deck string into a list of {:qty :title}"
  [deck-string]
  (reduce line-reducer [] (split-lines deck-string)))

(defn collate-deck
  "Takes a list of {:qty n :card title} and returns list of unique titles and summed n for same title"
  [card-list]
  ;; create a backing map of title to {:qty n :card title} and update the
  (letfn [(duphelper [currmap line]
            (let [title (:card line)
                  curr-qty (get-in currmap [title :qty] 0)
                  line (update line :qty #(+ % curr-qty))]
              (assoc currmap title line)))]
          (vals (reduce duphelper {} card-list))))

(defn lookup-deck
  "Takes a list of {:qty n :card title} and looks up each title and replaces it with the corresponding cardmap"
  [side card-list]
  (let [card-list (collate-deck card-list)]
    ;; lookup each card and replace title with cardmap
    (map #(assoc % :card (lookup side (assoc % :title (:card %)))) card-list)))

(defn parse-deck-string
  "Parses a string containing the decklist and returns a list of lines {:qty :card}"
  [side deck-string]
  (let [raw-deck-list (deck-string->list deck-string)]
    (lookup-deck side raw-deck-list)))



(defn load-decks [decks]
  (swap! app-state assoc :decks decks)
  (put! select-channel (first (sort-by :date > decks)))
  (swap! app-state assoc :decks-loaded true))

(defn process-decks
  "Process the raw deck from the database into a more useful format"
  [decks]
  (for [deck decks]
    (let [identity (parse-identity (:identity deck))
          cards (lookup-deck (:side identity) (:cards deck))]
      (assoc deck :identity identity :cards cards))))

(defn distinct-by [f coll]
  (letfn [(step [xs seen]
            (lazy-seq (when-let [[x & more] (seq xs)]
                        (let [k (f x)]
                          (if (seen k)
                            (step more seen)
                            (cons x (step more (conj seen k))))))))]
    (step coll #{})))

(defn- add-deck-name
  [all-titles card]
  (let [card-title (:title card)
        indexes (keep-indexed #(if (= %2 card-title) %1 nil) all-titles)
        dups (> (count indexes) 1)]
    (if dups
      (assoc card :display-name (str (:title card) " (" (:setname card) ")"))
      (assoc card :display-name (:title card)))))


(defn side-identities [side]
  (let [cards
        (->> @all-cards
          (filter #(and (= (:side %) side)
                        (= (:type %) "Identity")))
          (filter #(not (contains? %1 :replaced_by))))
        all-titles (map :title cards)
        add-deck (partial add-deck-name all-titles)]
    (->> cards
      (map add-deck)
      (reduce expand-alts []))))

(defn- insert-params
  "Add card parameters into the string representation"
  [card]
  (let [id (:id card)
        art (:art card)]
    (if (or id art)
      (str " ["
           (when id (str "id: " id))
           (when (and id art) ", ")
           (when art (str "art: " art))
           "]")
      "")))

(defn deck->str [owner]
  (let [cards (om/get-state owner [:deck :cards])
        str (reduce #(str %1 (:qty %2) " " (get-in %2 [:card :title]) (insert-params %2) "\n") "" cards)]
    (om/set-state! owner :deck-edit str)))














(defn edit-deck [owner]
  (let [deck (om/get-state owner :deck)]
    (om/set-state! owner :old-deck deck)
    (om/set-state! owner :edit true)
    (deck->str owner)
    (-> owner (om/get-node "viewport") js/$ (.addClass "edit"))
    (try (js/ga "send" "event" "deckbuilder" "edit") (catch js/Error e))
    (go (<! (timeout 500))
        (-> owner (om/get-node "deckname") js/$ .select))))

(defn end-edit [owner]
  (om/set-state! owner :edit false)
  (om/set-state! owner :query "")
  (-> owner (om/get-node "viewport") js/$ (.removeClass "edit")))

(defn handle-edit [owner]
  (let [text (.-value (om/get-node owner "deck-edit"))
        side (om/get-state owner [:deck :identity :side])
        cards (parse-deck-string side text)]
    (om/set-state! owner :deck-edit text)
    (om/set-state! owner [:deck :cards] cards)))

(defn cancel-edit [owner]
  (end-edit owner)
  (go (let [deck (om/get-state owner :old-deck)
            all-decks (process-decks (:json (<! (GET (str "/data/decks")))))]
        (load-decks all-decks)
        (put! select-channel deck))))

(defn delete-deck [owner]
  (om/set-state! owner :delete true)
  (deck->str owner)
  (-> owner (om/get-node "viewport") js/$ (.addClass "delete"))
  (try (js/ga "send" "event" "deckbuilder" "delete") (catch js/Error e)))

(defn end-delete [owner]
  (om/set-state! owner :delete false)
  (-> owner (om/get-node "viewport") js/$ (.removeClass "delete")))

(defn handle-delete [cursor owner]
  (authenticated
   (fn [user]
     (let [deck (om/get-state owner :deck)]
       (try (js/ga "send" "event" "deckbuilder" "delete") (catch js/Error e))
       (go (let [response (<! (DELETE (str "/data/decks/" (:_id deck))))]))
       (do
         (om/transact! cursor :decks (fn [ds] (remove #(= deck %) ds)))
         (om/set-state! owner :deck (first (sort-by :date > (:decks @cursor))))
         (end-delete owner))))))

(defn new-deck [side owner]
  (om/set-state! owner :deck {:name "New deck" :cards [] :identity (-> side side-identities first)})
  (try (js/ga "send" "event" "deckbuilder" "new" side) (catch js/Error e))
  (edit-deck owner))

(defn save-deck [cursor owner]
  (authenticated
   (fn [user]
     (end-edit owner)
     (let [deck (assoc (om/get-state owner :deck) :date (.toJSON (js/Date.)))
           deck (dissoc deck :stats)
           decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))
           cards (for [card (:cards deck) :when (get-in card [:card :title])]
                   (let [card-map {:qty (:qty card) :card (get-in card [:card :title])}
                         card-id (if (contains? card :id) (conj card-map {:id (:id card)}) card-map)]
                     (if (contains? card :art)
                       (conj card-id {:art (:art card)})
                       card-id)))
           ;; only include keys that are relevant
           identity (select-keys (:identity deck) [:title :side :code])
           identity-art (if (contains? (:identity deck) :art)
                          (do
                            (conj identity {:art (:art (:identity deck))}))
                          identity)
           data (assoc deck :cards cards :identity identity-art)]
       (try (js/ga "send" "event" "deckbuilder" "save") (catch js/Error e))
       (go (let [new-id (get-in (<! (if (:_id deck)
                                      (PUT "/data/decks" data :json)
                                      (POST "/data/decks" data :json)))
                                [:json :_id])
                 new-deck (if (:_id deck) deck (assoc deck :_id new-id))
                 all-decks (process-decks (:json (<! (GET (str "/data/decks")))))]
             (om/update! cursor :decks (conj decks new-deck))
             (om/set-state! owner :deck new-deck)
             (load-decks all-decks)))))))

(defn clear-deck-stats [cursor owner]
  (authenticated
    (fn [user]
      (let [deck (dissoc (om/get-state owner :deck) :stats)
            decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))]
        (try (js/ga "send" "event" "deckbuilder" "cleardeckstats") (catch js/Error e))
        (go (let [result (<! (DELETE (str "/profile/stats/deck/" (:_id deck))))]
              (om/update! cursor :decks (conj decks deck))
              (om/set-state! owner :deck deck)
              (.focus deck)))))))

(defn html-escape [st]
  (escape st {\< "&lt;" \> "&gt;" \& "&amp;" \" "#034;"}))

;; Dot definitions
(def zws "\u200B")                                          ; zero-width space for wrapping dots
(def influence-dot (str "●" zws))                           ; normal influence dot
(def banned-dot (str "✘" zws))                              ; on the banned list
(def restricted-dot (str "🦄" zws))                         ; on the restricted list
(def alliance-dot (str "○" zws))                            ; alliance free-inf dot
(def rotated-dot (str "↻" zws))                             ; on the rotation list

(def banned-span
  [:span.invalid {:title "Removed"} " " banned-dot])

(def restricted-span
  [:span {:title "Restricted"} " " restricted-dot])

(def rotated-span
  [:span.casual {:title "Rotated"} " " rotated-dot])

(defn- make-dots
  "Returns string of specified dots and number. Uses number for n > 20"
  [dot n]
  (if (<= 20 n)
    (str n dot)
    (join (conj (repeat n dot) ""))))

(defn influence-dots
  "Returns a string with UTF-8 full circles representing influence."
  [num]
  (make-dots influence-dot num))

(defn alliance-dots
  [num]
  (make-dots alliance-dot num))

(defn- dots-html
  "Make a hiccup-ready vector for the specified dot and cost-map (influence or mwl)"
  [dot cost-map]
  (for [factionkey (sort (keys cost-map))]
    [:span.influence {:class (name factionkey)} (make-dots dot (factionkey cost-map))]))

(defn card-influence-html
  "Returns hiccup-ready vector with dots for influence as well as restricted / rotated / banned symbols"
  [card qty in-faction allied?]
  (let [influence (* (:factioncost card) qty)
        banned (decks/banned? card)
        restricted (decks/restricted? card)
        rotated (:rotated card)]
    (list " "
          (when (and (not banned) (not in-faction))
            [:span.influence {:class (utils/faction-label card)}
             (if allied?
               (alliance-dots influence)
               (influence-dots influence))])
          (if banned
            banned-span
            [:span
             (when restricted restricted-span)
             (when rotated rotated-span)]))))

(defn deck-influence-html
  "Returns hiccup-ready vector with dots colored appropriately to deck's influence."
  [deck]
  (dots-html influence-dot (decks/influence-map deck)))

(defn- build-deck-status-label [valid mwl rotation cache-refresh onesies onesies-details?]
  (let [status (decks/deck-status mwl valid rotation)
        message (case status
                  "legal" "Tournament legal"
                  "casual" "Casual play only"
                  "invalid" "Invalid"
                  "")]
    [:div.status-tooltip.blue-shade
     [:div {:class (if valid "legal" "invalid")}
      [:span.tick (if valid "✔" "✘")] "Basic deckbuilding rules"]
     [:div {:class (if mwl "legal" "invalid")}
      [:span.tick (if mwl "✔" "✘")] (:name @cards/mwl)]
     [:div {:class (if rotation "legal" "invalid")}
      [:span.tick (if rotation "✔" "✘")] "Only released cards"]
     [:div {:class (if (:legal cache-refresh) "legal" "invalid") :title (if onesies-details? (:reason cache-refresh)) }
      [:span.tick (if (:legal cache-refresh) "✔" "✘")] "Cache Refresh compliant"]
     [:div {:class (if (:legal onesies) "legal" "invalid") :title (if onesies-details? (:reason onesies))}
      [:span.tick (if (:legal onesies) "✔" "✘") ] "1.1.1.1 format compliant"]]))

(defn- deck-status-details
  [deck use-trusted-info]
  (if use-trusted-info
    (decks/trusted-deck-status deck)
    (decks/calculate-deck-status deck)))

(defn format-deck-status-span
  [deck-status tooltip? onesies-details?]
  (let [{:keys [valid mwl rotation cache-refresh onesies status]} deck-status
        message (case status
                  "legal" "Tournament legal"
                  "casual" "Casual play only"
                  "invalid" "Invalid"
                  "")]
    [:span.deck-status.shift-tooltip {:class status} message
     (when tooltip?
       (build-deck-status-label valid mwl rotation cache-refresh onesies onesies-details?))]))

(defn deck-status-span-impl [sets deck tooltip? onesies-details? use-trusted-info]
  (format-deck-status-span (deck-status-details deck use-trusted-info) tooltip? onesies-details?))

(def deck-status-span-memoize (memoize deck-status-span-impl))

(defn deck-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity."
  ([sets deck] (deck-status-span sets deck false false true))
  ([sets deck tooltip? onesies-details? use-trusted-info]
   (deck-status-span-memoize sets deck tooltip? onesies-details? use-trusted-info)))

(defn match [identity query]
  (->> @all-cards
    (filter #(decks/allowed? % identity))
    (distinct-by :title)
    (filter-title query)
    (take 10)))

(defn handle-keydown [owner event]
  (let [selected (om/get-state owner :selected)
        matches (om/get-state owner :matches)]
    (case (.-keyCode event)
      38 (when (pos? selected)
           (om/update-state! owner :selected dec))
      40 (when (< selected (dec (count matches)))
           (om/update-state! owner :selected inc))
      (9 13) (when-not (= (om/get-state owner :query) (:title (first matches)))
               (.preventDefault event)
               (-> ".deckedit .qty" js/$ .select)
               (om/set-state! owner :query (:title (nth matches selected))))
      (om/set-state! owner :selected 0))))

(defn handle-add [owner event]
  (.preventDefault event)
  (let [qty (str->int (om/get-state owner :quantity))
        card (nth (om/get-state owner :matches) (om/get-state owner :selected))
        best-card (lookup (:side card) card)]
    (if (js/isNaN qty)
      (om/set-state! owner :quantity 3)
      (let [max-qty (or (:limited best-card) 3)
            limit-qty (if (> qty max-qty) max-qty qty)]
        (put! (om/get-state owner :edit-channel)
                {:qty limit-qty
                 :card best-card})
          (om/set-state! owner :quantity 3)
          (om/set-state! owner :query "")
          (-> ".deckedit .lookup" js/$ .select)))))

(defn card-lookup [{:keys [cards]} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:query ""
       :matches []
       :quantity 3
       :selected 0})

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:p
        [:h3 "Add cards"]
        [:form.card-search {:on-submit #(handle-add owner %)}
         [:input.lookup {:type "text" :placeholder "Card name" :value (:query state)
                         :on-change #(om/set-state! owner :query (.. % -target -value))
                         :on-key-down #(handle-keydown owner %)}]
         " x "
         [:input.qty {:type "text" :value (:quantity state)
                      :on-change #(om/set-state! owner :quantity (.. % -target -value))}]
         [:button "Add to deck"]
         (let [query (:query state)
               matches (match (get-in state [:deck :identity]) query)
               exact-match (= (:title (first matches)) query)]
           (cond
             exact-match
             (do
               (om/set-state! owner :matches matches)
               (om/set-state! owner :selected 0))

             (not (or (empty? query) exact-match))
             (do
               (om/set-state! owner :matches matches)
               [:div.typeahead
                (for [i (range (count matches))]
                  [:div {:class (if (= i (:selected state)) "selected" "")
                         :on-click (fn [e] (-> ".deckedit .qty" js/$ .select)
                                     (om/set-state! owner :query (.. e -target -textContent))
                                     (om/set-state! owner :selected i))}
                   (:title (nth matches i))])])))]]))))

(defn deck-collection
  [{:keys [sets decks decks-loaded active-deck]} owner]
  (reify
    om/IRenderState
    (render-state [this state]
      (sab/html
        (cond
          (not decks-loaded) [:h4 "Loading deck collection..."]
          (empty? decks) [:h4 "No decks"]
          :else [:div
                 (for [deck (sort-by :date > decks)]
                   [:div.deckline {:class (when (= active-deck deck) "active")
                                   :on-click #(put! select-channel deck)}
                    [:img {:src (image-url (:identity deck))
                           :alt (get-in deck [:identity :title] "")}]
                    [:div.float-right (deck-status-span sets deck)]
                    [:h4 (:name deck)]
                    [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
                    [:p (get-in deck [:identity :title]) [:br]
                     (when (and (:stats deck) (not= "none" (get-in @app-state [:options :deckstats])))
                       (let [stats (:stats deck)
                             games (or (:games stats) 0)
                             started (or (:games-started stats) 0)
                             completed (or (:games-completed stats) 0)
                             wins (or (:wins stats) 0)
                             losses (or (:loses stats) 0)]
                         ; adding key :games to handle legacy stats before adding started vs completed
                         [:span "  Games: " (+ started games)
                          " - Completed: " (+ completed games)
                          " - Won: " wins " (" (num->percent wins (+ wins losses)) "%)"
                          " - Lost: " losses]))]])])))))

(defn line-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span qty "  "
   (if-let [name (:title card)]
     (let [infaction (noinfcost? identity card)
           banned (decks/banned? card)
           allied (decks/alliance-is-free? cards line)
           valid (and (decks/allowed? card identity)
                      (decks/legal-num-copies? identity line))
           released (decks/released? sets card)
           modqty (if (decks/is-prof-prog? deck card) (- qty 1) qty)]
       [:span
        [:span {:class (cond
                         (and valid released (not banned)) "fake-link"
                         valid "casual"
                         :else "invalid")
                :on-mouse-enter #(put! zoom-channel line)
                :on-mouse-leave #(put! zoom-channel false)} name]
        (card-influence-html card modqty infaction allied)])
     card)])

 (defn line-qty-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span qty "  "])
 
(defn line-name-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span (if-let [name (:title card)]
           (let [infaction (noinfcost? identity card)
                 banned (decks/banned? card)
                 allied (decks/alliance-is-free? cards line)
                 valid (and (decks/allowed? card identity)
                            (decks/legal-num-copies? identity line))
                 released (decks/released? sets card)
                 modqty (if (decks/is-prof-prog? deck card) (- qty 1) qty)]
             [:span
              [:span {:class (cond
                               (and valid released (not banned)) "fake-link"
                               valid "casual"
                               :else "invalid")
                      :on-mouse-enter #(put! zoom-channel line)
                      :on-mouse-leave #(put! zoom-channel false)} name]
              (card-influence-html card modqty infaction allied)])
           card)])

(defn- create-identity
  [state target-value]
  (let [side (get-in state [:deck :identity :side])
        json-map (.parse js/JSON (.. target-value -target -value))
        id-map (js->clj json-map :keywordize-keys true)
        card (lookup side id-map)]
    (if-let [art (:art id-map)]
      (assoc card :art art)
      card)))

(defn- identity-option-string
  [card]
  (.stringify js/JSON (clj->js {:title (:title card) :id (:code card) :art (:art card)})))

(defn deck-builder
  "Make the deckbuilder view"
  [{:keys [decks decks-loaded sets] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
       :old-deck nil
       :edit-channel (chan)
       :deck nil})

    om/IWillMount
    (will-mount [this]
      (let [edit-channel (om/get-state owner :edit-channel)]
        (go (while true
            (let [card (<! zoom-channel)]
              (om/set-state! owner :zoom card))))
        (go (while true
              (let [edit (<! edit-channel)
                    card (:card edit)
                    max-qty (or (:limited card) 3)
                    cards (om/get-state owner [:deck :cards])
                    match? #(when (= (get-in % [:card :title]) (:title card)) %)
                    existing-line (some match? cards)]
                (let [new-qty (+ (or (:qty existing-line) 0) (:qty edit))
                      rest (remove match? cards)
                      draft-id (decks/is-draft-id? (om/get-state owner [:deck :identity]))
                      new-cards (cond (and (not draft-id) (> new-qty max-qty))
                                        (conj rest (assoc existing-line :qty max-qty))
                                      (<= new-qty 0) rest
                                      (empty? existing-line) (conj rest {:qty new-qty :card card})
                                      :else (conj rest (assoc existing-line :qty new-qty)))]
                  (om/set-state! owner [:deck :cards] new-cards))
                (deck->str owner)))))
      (go (while true
            (om/set-state! owner :deck (<! select-channel)))))

    om/IRenderState
    (render-state [this state]
      (sab/html
       [:div
        [:div.deckbuilder.blue-shade.panel
         [:div.viewport {:ref "viewport"}
          [:div.decks
           [:div.button-bar
            [:button {:on-click #(new-deck "Corp" owner)} "New Corp deck"]
            [:button {:on-click #(new-deck "Runner" owner)} "New Runner deck"]]
           [:div.deck-collection
            (when-not (:edit state)
              (om/build deck-collection {:sets sets :decks decks :decks-loaded decks-loaded :active-deck (om/get-state owner :deck)}))
            ]
           [:div {:class (when (:edit state) "edit")}
            (when-let [line (om/get-state owner :zoom)]
              (let [art (:art line)
                    id (:id line)
                    updated-card (add-params-to-card (:card line) id art)]
              (om/build card-view updated-card {:state {:cursor cursor}})))]]

          [:div.decklist
           (when-let [deck (:deck state)]
             (let [identity (:identity deck)
                   cards (:cards deck)
                   edit? (:edit state)
                   delete? (:delete state)]
               [:div
                (cond
                  edit? [:div.button-bar
                         [:button {:on-click #(save-deck cursor owner)} "Save"]
                         [:button {:on-click #(cancel-edit owner)} "Cancel"]]
                  delete? [:div.button-bar
                           [:button {:on-click #(handle-delete cursor owner)} "Confirm Delete"]
                           [:button {:on-click #(end-delete owner)} "Cancel"]]
                  :else [:div.button-bar
                         [:button {:on-click #(edit-deck owner)} "Edit"]
                         [:button {:on-click #(delete-deck owner)} "Delete"]
                         (when (and (:stats deck) (not= "none" (get-in @app-state [:options :deckstats])))
                           [:button {:on-click #(clear-deck-stats cursor owner)} "Clear Stats"])])
                [:h3 (:name deck)]
                [:div.header
                 [:img {:src (image-url identity)
                        :alt (:title identity)}]
                 [:h4 {:class (if (decks/released? (:sets @app-state) identity) "fake-link" "casual")
                       :on-mouse-enter #(put! zoom-channel {:card identity :art (:art identity) :id (:id identity)})
                       :on-mouse-leave #(put! zoom-channel false)}
                  (:title identity)
                  (if (decks/banned? identity)
                    banned-span
                    (when (:rotated identity) rotated-span))]
                 (let [count (decks/card-count cards)
                       min-count (decks/min-deck-size identity)]
                   [:div count " cards"
                    (when (< count min-count)
                      [:span.invalid (str " (minimum " min-count ")")])])
                 (let [inf (decks/influence-count deck)
                       id-limit (decks/id-inf-limit identity)]
                   [:div "Influence: "
                    ;; we don't use valid? and mwl-legal? functions here, since it concerns influence only
                    [:span {:class (if (> inf id-limit) (if (> inf id-limit) "invalid" "casual") "legal")} inf]
                    "/" (if (= INFINITY id-limit) "∞" id-limit)
                    (if (pos? inf)
                      (list " " (deck-influence-html deck)))])
                 (when (= (:side identity) "Corp")
                   (let [min-point (decks/min-agenda-points deck)
                         points (decks/agenda-points deck)]
                     [:div "Agenda points: " points
                      (when (< points min-point)
                        [:span.invalid " (minimum " min-point ")"])
                      (when (> points (inc min-point))
                        [:span.invalid " (maximum " (inc min-point) ")"])]))
                 [:div (deck-status-span sets deck true true false)]]
                [:div.cards
                 (for [group (sort-by first (group-by #(get-in % [:card :type]) cards))]
                   [:div.group
                    [:h4 (str (or (first group) "Unknown") " (" (decks/card-count (last group)) ")") ]
                    (for [line (sort-by #(get-in % [:card :title]) (last group))]
                      [:div.line
                       (if (:edit state)
                         (let [ch (om/get-state owner :edit-channel)]
                           [:span
                            [:button.small {:on-click #(put! ch {:qty -1 :card (:card line)})
                                            :type "button"} "-"]
                            (line-qty-span sets deck line)
                            [:button.small {:on-click #(put! ch {:qty 1 :card (:card line)})
                                            :type "button"} "+"]
                            (line-name-span sets deck line)])
                        (line-span sets deck line))])])]]))]

          [:div.deckedit
           [:div
            [:p
             [:h3 "Deck name"]
             [:input.deckname {:type "text" :placeholder "Deck name"
                               :ref "deckname" :value (get-in state [:deck :name])
                               :on-change #(om/set-state! owner [:deck :name] (.. % -target -value))}]]
            [:p
             [:h3 "Identity"]
             [:select.identity {:value (identity-option-string (get-in state [:deck :identity]))
                                :on-change #(om/set-state! owner [:deck :identity] (create-identity state %))}
              (let [idents (side-identities (get-in state [:deck :identity :side]))]
                (for [card (sort-by :display-name idents)]
                  [:option
                   {:value (identity-option-string card)}
                   (:display-name card)]))]]
            (om/build card-lookup cursor {:state state})
            [:h3 "Decklist"
             [:span.small "(Type or paste a decklist, it will be parsed)" ]]]
           [:textarea {:ref "deck-edit" :value (:deck-edit state)
                       :on-change #(handle-edit owner)}]]]]]))))

(go (let [cards (<! cards-channel)
          decks (process-decks (:json (<! (GET (str "/data/decks")))))]
      (load-decks decks)
      (load-alt-arts)
      (>! cards-channel cards)))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})
