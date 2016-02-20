(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :refer [split split-lines join escape]]
            [netrunner.main :refer [app-state]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.cardbrowser :refer [cards-channel image-url card-view] :as cb]
            [netrunner.ajax :refer [POST GET]]))

(def select-channel (chan))
(def zoom-channel (chan))
(def INFINITY 2147483647)

(defn identical-cards? [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))

(defn found? [query cards]
  (some #(if (= (.toLowerCase (:title %)) query) %) cards))

(defn id-inf-limit
  "Returns influence limit of an identity or INFINITY in case of draft IDs."
  [identity]
  (if (= (:setname identity) "Draft") INFINITY (:influencelimit identity)))

(defn mostwanted?
  "Returns true if card is on Most Wanted NAPD list."
  [card]
  (let [napdmwl '("Cerberus \"Lady\" H1" "Clone Chip" "Desperado" "Parasite" "Prepaid VoicePAD" "Yog.0"
                   "Architect" "AstroScript Pilot Program" "Eli 1.0" "NAPD Contract" "SanSan City Grid")]
    (some #(= (:title card) %) napdmwl)))

(defn card-count [cards]
  (reduce #(+ %1 (:qty %2)) 0 cards))

(defn noinfcost? [identity card]
  (or (= (:faction card) (:faction identity))
      (= 0 (:factioncost card)) (= INFINITY (id-inf-limit identity))))

(defn search [query cards]
  (filter #(if (= (.indexOf (.toLowerCase (:title %)) query) -1) false true) cards))

(defn alt-art? [card]
  (or (get-in @app-state [:user :special])
      (not= "Alternates" (:setname card))))

(defn lookup [side query]
  (let [q (.toLowerCase query)
        cards (filter #(and (= (:side %) side) (alt-art? %))
                      (:cards @app-state))]
    (if-let [card (some #(when (= (-> % :title .toLowerCase) q) %) cards)]
      card
      (loop [i 2 matches cards]
        (let [subquery (subs q 0 i)]
         (cond (zero? (count matches)) query
               (or (= (count matches) 1) (identical-cards? matches)) (first matches)
               (found? subquery matches) (found? subquery matches)
               (<= i (count query)) (recur (inc i) (search subquery matches))
               :else query))))))

(defn parse-line [side line]
  (let [tokens (split line " ")
        qty (js/parseInt (first tokens))
        cardname (join " " (rest tokens))]
    (when-not (js/isNaN qty)
      {:qty (min qty 6) :card (lookup side cardname)})))

(defn parse-deck
  "Parses a string containing cardlist and returns a list of line card maps {:qty num :card cardmap}"
  [side deck]
  (let [base-list (reduce #(if-let [card (parse-line side %2)] (conj %1 card) %1) [] (split-lines deck))
        ;; in case there were e.g. 2 lines with Sure Gambles, we need to sum them up in deduplicate
        duphelper (fn [currmap line]
                    (let [title (:title (:card line))
                          qty (:qty line)]
                      (if (contains? currmap title)
                        (assoc-in currmap [title :qty] (+ (get-in currmap [title :qty]) qty))
                        (assoc currmap title line))))]
    (vals (reduce duphelper {} base-list))))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction .toLowerCase (.replace " " "-"))))

(defn allowed?
  "Checks if a card is allowed in deck of a given identity - not accounting for influence"
  [card {:keys [side faction title] :as identity}]
  (and (not= (:type card) "Identity")
       (= (:side card) side)
       (or (not= (:type card) "Agenda")
           (= (:faction card) "Neutral")
           (= (:faction card) faction)
           (= title "The Shadow: Pulling the Strings"))
       (or (not= title "Custom Biotics: Engineered for Success")
           (not= (:faction card) "Jinteki"))))

(defn load-decks [decks]
  (swap! app-state assoc :decks decks)
  (put! select-channel (first (sort-by :date > decks)))
  (swap! app-state assoc :decks-loaded true))

(defn process-decks [decks]
  (for [deck decks]
    (let [cards (map #(str (:qty %) " " (:card %)) (:cards deck))]
      (assoc deck :cards (parse-deck (get-in deck [:identity :side]) (join "\n" cards))))))

(defn distinct-by [f coll]
  (letfn [(step [xs seen]
            (lazy-seq (when-let [[x & more] (seq xs)]
                        (let [k (f x)]
                          (if (seen k)
                            (step more seen)
                            (cons x (step more (conj seen k))))))))]
    (step coll #{})))

(defn side-identities [side]
  (->> (:cards @app-state)
       (filter #(and (= (:side %) side)
                     (= (:type %) "Identity")
                     (alt-art? %)))
       (distinct-by :title)))

(defn get-card [title]
  (some #(when (and (= (:title %) title) (alt-art? %)) %)
        (:cards @app-state)))

(defn deck->str [owner]
  (let [cards (om/get-state owner [:deck :cards])
        str (reduce #(str %1 (:qty %2) " " (get-in %2 [:card :title]) "\n") "" cards)]
    (om/set-state! owner :deck-edit str)))

(defn mostwanted
  "Returns a map of faction keywords to number of MWL restricted cards from the faction's cards."
  [deck]
  (let [cards (:cards deck)
        mwlhelper (fn [currmap line]
                    (let [card (:card line)]
                      (if (mostwanted? card)
                        (update-in currmap [(keyword (faction-label card))]
                                   (fnil (fn [curmwl] (+ curmwl (:qty line))) 0))
                        currmap)))]
    (reduce mwlhelper {} cards)))

(defn influence
  "Returns a map of faction keywords to influence values from the faction's cards."
  [deck]
  (let [identity (:identity deck)
        cards (:cards deck)
        ;; sums up influence of a cardlist by faction to a map
        infhelper (fn [currmap line]
                    (let [card (:card line)]
                      (if (= (:faction card) (:faction identity))
                        currmap
                        (update-in currmap [(keyword (faction-label card))]
                                   (fnil (fn [curinf] (+ curinf (* (:qty line) (:factioncost card))))
                                         0)))))
        infmap (reduce infhelper {} cards)
        ;; sums up influence of one of each imported programs, to resolve Professor's ability
        profhelper (fn [arg-infmap]
                     (let [progs (filter #(= "Program" (:type (:card %))) cards)
                           ;; list with single programs
                           singled-progs (reduce #(conj %1 (assoc-in %2 [:qty] 1)) '() progs)
                           singled-infmap (reduce infhelper {} singled-progs)]
                       (merge-with - arg-infmap singled-infmap)))
        infmap (if (= (:code identity) "03029") ; The Professor: Keeper of Knowledge
                 (profhelper infmap)
                 infmap)
        ;; checks card ID against list of currently known alliance cards
        has-alliance-subtype? (fn [card]
                                (case (:code (:card card))
                                  (list "10013" "10018" "10019" "10067" "10068" "10071" "10072" "10076" "10109")
                                  true
                                  false))
        ;; alliance helper, subtracts influence of free ally cards from given influence map
        allyhelper (fn [arg-infmap]
                     (let [ally-cards (filter has-alliance-subtype? cards)
                           ;; Implements the standard alliance check, 6 or more non-alliance faction cards
                           default-alliance-free? (fn [card]
                                                    (<= 6 (card-count (filter #(and (= (:faction (:card card))
                                                                                       (:faction (:card %)))
                                                                                    (not (has-alliance-subtype? %)))
                                                                              cards))))
                           ;; checks card for alliance conditions and returns true if they are met
                           is-ally-free? (fn [card]
                                           (case (:code (:card card))
                                             (list
                                               "10013" ; Heritage Committee
                                               "10067" ; Jeeves Model Bioroids
                                               "10068" ; Raman Rai
                                               "10071" ; Salem's Hospitality
                                               "10072" ; Executive Search Firm
                                               "10109") ; Ibrahim Salem
                                             (default-alliance-free? card)
                                             "10018" ; Mumba Temple
                                             (>= 15 (card-count (filter #(= "ICE" (:type (:card %))) cards)))
                                             "10019" ; Museum of History
                                             (<= 50 (card-count cards))
                                             "10076" ; Mumbad Virtual Tour
                                             (<= 7 (card-count (filter #(= "Asset" (:type (:card %))) cards)))
                                             false))
                           free-ally-cards (filter is-ally-free? ally-cards)
                           free-ally-infmap (reduce infhelper {} free-ally-cards)]
                       (merge-with - arg-infmap free-ally-infmap)))
        infmap (if (some has-alliance-subtype? cards)
                 (allyhelper infmap)
                 infmap)]
    infmap
    ))

(defn mostwanted-count
  "Returns total number of MWL restricted cards in a deck."
  [deck]
  (apply + (vals (mostwanted deck))))

(defn influence-count
  "Returns sum of influence count used by a deck."
  [deck]
  (apply + (vals (influence deck))))

(defn deck-inf-limit [deck]
  (let [originf (id-inf-limit (:identity deck))
        postmwlinf (- originf (mostwanted-count deck))]
    (if (= originf INFINITY) ; FIXME this ugly 'if' could get cut when we get a proper nonreducible infinity in CLJS
      INFINITY (if (> 1 postmwlinf) 1 postmwlinf))))

(defn min-deck-size
  "Contains implementation-specific decksize adjustments, if they need to be different from printed ones."
  [identity]
  (cond
    (= "09037" (:code identity)) 48 ;; Adam needs to have his 3 directives in deck
    :else (:minimumdecksize identity)))

(defn min-agenda-points [deck]
  (let [size (max (card-count (:cards deck)) (min-deck-size (:identity deck)))]
    (+ 2 (* 2 (quot size 5)))))

(defn agenda-points [{:keys [cards]}]
  (reduce #(if-let [point (get-in %2 [:card :agendapoints])]
             (+ (* point (:qty %2)) %1) %1) 0 cards))

(defn legal-num-copies?
  "Returns true if there is a legal number of copies of a particular card."
  [{:keys [qty card] :as line}]
  (<= qty (or (:limited card) 3)))

(defn adam-valid?
  "Checks for Adam decks specific to current implementation of his identity."
  [{:keys [identity cards] :as deck}]
  (or (not= "09037" (:code identity))
      (and (<= 1 (card-count (filter #(= "09041" (:code (:card %))) cards))) ;; Always Be Running
           (<= 1 (card-count (filter #(= "09043" (:code (:card %))) cards))) ;; Neutralize All Threats
           (<= 1 (card-count (filter #(= "09044" (:code (:card %))) cards)))))) ;; Safety First

(defn valid? [{:keys [identity cards] :as deck}]
  (and (>= (card-count cards) (min-deck-size identity))
       (<= (influence-count deck) (id-inf-limit identity))
       (every? #(and (allowed? (:card %) identity)
                     (legal-num-copies? %)) cards)
       (or (= (:side identity) "Runner")
           (let [min (min-agenda-points deck)]
             (<= min (agenda-points deck) (inc min))))
       (adam-valid? deck)))

(defn released?
  "Returns false if the card comes from a spoiled set or is out of competitive rotation."
  [card]
  (let [cid (js/parseInt (:code card))]
    ;; Cards up to Kala Ghoda are currently released
    (and cid (<= cid 10019))))

(defn mwl-legal?
  "Returns true if the deck's influence fits within NAPD MWL restrictions."
  [deck]
  (<= (influence-count deck) (deck-inf-limit deck)))

(defn only-in-rotation?
  "Returns true if the deck doesn't contain any cards outside of current rotation."
  [deck]
  (and (every? #(released? (:card %)) (:cards deck))
       (released? (:identity deck))))

(defn edit-deck [owner]
  (om/set-state! owner :edit true)
  (deck->str owner)
  (-> owner (om/get-node "viewport") js/$ (.addClass "edit"))
  (try (js/ga "send" "event" "deckbuilder" "edit") (catch js/Error e))
  (go (<! (timeout 500))
      (-> owner (om/get-node "deckname") js/$ .select)))

(defn new-deck [side owner]
  (om/set-state! owner :deck {:name "New deck" :cards [] :identity (-> side side-identities first)})
  (try (js/ga "send" "event" "deckbuilder" "new" side) (catch js/Error e))
  (edit-deck owner))

(defn end-edit [owner]
  (om/set-state! owner :edit false)
  (om/set-state! owner :query "")
  (-> owner (om/get-node "viewport") js/$ (.removeClass "edit")))

(defn save-deck [cursor owner]
  (authenticated
   (fn [user]
     (end-edit owner)
     (let [deck (assoc (om/get-state owner :deck) :date (.toJSON (js/Date.)))
           decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))
           cards (for [card (:cards deck) :when (get-in card [:card :title])]
                   {:qty (:qty card) :card (get-in card [:card :title])})
           data (assoc deck :cards cards)]
       (try (js/ga "send" "event" "deckbuilder" "save") (catch js/Error e))
       (go (let [new-id (get-in (<! (POST "/data/decks/" data :json)) [:json :_id])
                 new-deck (if (:_id deck) deck (assoc deck :_id new-id))]
             (om/update! cursor :decks (conj decks new-deck))
             (om/set-state! owner :deck new-deck)))))))

(defn match [identity query]
  (if (empty? query)
    []
    (let [cards (->> (:cards @app-state)
                     (filter #(and (allowed? % identity)
                                   (not= "Special" (:setname %))
                                   (alt-art? %)))
                     (distinct-by :title))]
      (take 10 (filter #(not= (.indexOf (.toLowerCase (:title %)) (.toLowerCase query)) -1) cards)))))

(defn handle-edit [owner]
  (let [text (.-value (om/get-node owner "deck-edit"))]
    (om/set-state! owner :deck-edit text)
    (om/set-state! owner [:deck :cards] (parse-deck (om/get-state owner [:deck :identity :side]) text))))

(defn handle-delete [cursor owner]
  (authenticated
   (fn [user]
     (let [deck (om/get-state owner :deck)]
       (try (js/ga "send" "event" "deckbuilder" "delete") (catch js/Error e))
       (go (let [response (<! (POST "/data/decks/delete" deck :json))]))
       (om/transact! cursor :decks (fn [ds] (remove #(= deck %) ds)))
       (om/set-state! owner :deck (first (sort-by :date > (:decks @cursor))))))))

(defn html-escape [st]
  (escape st {\< "&lt;" \> "&gt;" \& "&amp;" \" "#034;"}))

(defn not-alternate [card]
  (if (= (:setname card) "Alternates")
    (some #(when (and (not= (:setname %) "Alternates") (= (:title %) (:title card))) %)
          (:cards @app-state))
    card))

(defn influence-dots
  "Returns a string with UTF-8 full circles representing influence."
  [num]
  (join (conj (repeat num "&#9679;&#8203;") ""))) ; &#8203; is a zero-width space to allow wrapping

(defn restricted-dots
  "Returns a string with UTF-8 empty circles representing MWL restricted cards."
  [num]
  (join (conj (repeat num "&#9675;&#8203;") "")))

(defn influence-html
  "Returns hiccup-ready vector with dots colored appropriately to deck's influence."
  [deck]
  (let [infmap (influence deck)]
    (for [factionkey (sort (keys infmap))] [:span.influence
                                            {:class (name factionkey)
                                             :dangerouslySetInnerHTML
                                                    #js {:__html (influence-dots (factionkey infmap))}}])))

(defn restricted-html
  "Returns hiccup-ready vector with dots colored appropriately to deck's MWL restricted cards."
  [deck]
  (let [mwlmap (mostwanted deck)]
    (for [factionkey (sort (keys mwlmap))] [:span.influence
                                         {:class (name factionkey)
                                          :dangerouslySetInnerHTML
                                          #js {:__html (restricted-dots (factionkey mwlmap))}}])))

(defn deck-status-label
  [deck]
  (cond
    (and (mwl-legal? deck) (valid? deck) (only-in-rotation? deck)) "legal"
    (valid? deck) "casual"
    :else "invalid"))

(defn deck-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity."
  ([deck] (deck-status-span deck false))
  ([deck tooltip?]
   (let [status (deck-status-label deck)
         valid (valid? deck)
         mwl (mwl-legal? deck)
         rotation (only-in-rotation? deck)
         message (case status
                   "legal" "Tournament legal"
                   "casual" "Casual play only"
                   "invalid" "Invalid")]
     [:span.deck-status {:class status} message
      (when tooltip?
        [:div.status-tooltip.blue-shade
         [:div {:class (if valid "legal" "invalid")}
          [:span.tick (if valid "✔" "✘")] "Basic deckbuilding rules"]
         [:div {:class (if mwl "legal" "invalid")}
          [:span.tick (if mwl "✔" "✘")] "NAPD Most Wanted List"]
         [:div {:class (if rotation "legal" "invalid")}
          [:span.tick (if rotation "✔" "✘")] "Only released cards"]])])))

(defn octgn-link [owner]
  (let [deck (om/get-state owner :deck)
        identity (not-alternate (:identity deck))
        id "bc0f047c-01b1-427f-a439-d451eda"
        xml (str
             "<deck game=\"0f38e453-26df-4c04-9d67-6d43de939c77\"><section name=\"Identity\"><card qty=\"1\" id=\""
             id (:code identity) "\">" (:title identity) "</card></section>"
             "<section name=\"R&amp;D / Stack\">"
             (join (for [c (:cards deck) :when (get-in c [:card :title])]
                     (str "<card qty=\"" (:qty c) "\" id=\"" id (get-in c [:card :code]) "\">"
                          (html-escape (get-in c [:card :title])) "</card>")))
             "</section></deck>")
        blob (js/Blob. (clj->js [xml]) #js {:type "application/download"})]
    (.createObjectURL js/URL blob)))

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
  (let [qty (js/parseInt (om/get-state owner :quantity))]
    (if (js/isNaN qty)
      (om/set-state! owner :quantity 3)
      (do (put! (om/get-state owner :edit-channel)
                {:qty qty
                 :card (nth (om/get-state owner :matches) (om/get-state owner :selected))})
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
               matches (match (get-in state [:deck :identity]) query)]
           (when-not (or (empty? query)
                         (= (:title (first matches)) query))
             (om/set-state! owner :matches matches)
             [:div.typeahead
              (for [i (range (count matches))]
                [:div {:class (if (= i (:selected state)) "selected" "")
                       :on-click (fn [e] (-> ".deckedit .qty" js/$ .select)
                                         (om/set-state! owner :query (.. e -target -innerHTML))
                                         (om/set-state! owner :selected i))}
                 (:title (nth matches i))])]))]]))))

(defn deck-collection
  [decks active-deck]
  (for [deck (sort-by :date > decks)]
    [:div.deckline {:class (when (= active-deck deck) "active")
                    :on-click #(put! select-channel deck)}
     [:img {:src (image-url (:identity deck))}]
     [:div.float-right (deck-status-span deck)]
     [:h4 (:name deck)]
     [:div.float-right (-> (:date deck) js/Date. js/moment (.format "MMM Do YYYY"))]
     [:p (get-in deck [:identity :title])]]))

(defn deck-builder [{:keys [decks decks-loaded] :as cursor} owner]
  (reify
    om/IInitState
    (init-state [this]
      {:edit false
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
                      new-cards (cond (> new-qty max-qty) (conj rest {:qty max-qty :card card})
                                      (<= new-qty 0) rest
                                      :else (conj rest {:qty new-qty :card card}))]
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
            (cond
              (not decks-loaded) [:h4 "Loading deck collection..."]
              (empty? decks) [:h4 "No decks"]
              :else (deck-collection decks (om/get-state owner :deck)))]
           [:div {:class (when (:edit state) "edit")}
            (when-let [card (om/get-state owner :zoom)]
              (om/build card-view card))]]

          [:div.decklist
           (when-let [deck (:deck state)]
             (let [identity (:identity deck)
                   cards (:cards deck)]
               [:div
                (if (:edit state)
                  [:div.button-bar
                   [:button {:on-click #(save-deck cursor owner)} "Save"]
                   [:button {:on-click #(end-edit owner)} "Cancel"]]
                  [:div.button-bar
                   [:button {:on-click #(edit-deck owner)} "Edit"]
                   [:button {:on-click #(handle-delete cursor owner)} "Delete"]
                   [:a.button {:href (octgn-link owner) :download (str (:name deck) ".o8d")} "OCTGN Export"]])
                [:h3 (:name deck)]
                [:div.header
                 [:img {:src (image-url identity)}]
                 [:h4.fake-link {:on-mouse-enter #(put! zoom-channel identity)
                                 :on-mouse-leave #(put! zoom-channel false)} (:title identity)]
                 (let [count (card-count cards)
                       min-count (min-deck-size identity)]
                   [:div count " cards"
                    (when (< count min-count)
                      [:span.invalid (str " (minimum " min-count ")")])
                    (when-not (adam-valid? deck)
                      [:span.invalid " (not enough directives)"])])
                 (let [inf (influence-count deck)
                       limit (deck-inf-limit deck)
                       id-limit (id-inf-limit identity)
                       mwl (mostwanted-count deck)]
                   [:div "Influence: "
                    ; we don't use valid? and mwl-legal? functions here, since it concerns influence only
                    [:span {:class (if (> inf limit) (if (> inf id-limit) "invalid" "casual") "legal")} inf]
                    "/" (if (= INFINITY id-limit) "∞" limit)
                    (if (< 0 (+ inf mwl))
                      (list " " (influence-html deck) (restricted-html deck)))])
                 (when (= (:side identity) "Corp")
                   (let [min-point (min-agenda-points deck)
                         points (agenda-points deck)]
                     [:div "Agenda points: " points
                      (when (< points min-point)
                        [:span.invalid " (minimum " min-point ")"])
                      (when (> points (inc min-point))
                        [:span.invalid " (maximum" (inc min-point) ")"])]))
                 [:div (deck-status-span deck true)]]
                [:div.cards
                 (for [group (sort-by first (group-by #(get-in % [:card :type]) cards))]
                   [:div.group
                    [:h4 (str (or (first group) "Unknown") " (" (card-count (last group)) ")") ]
                    (for [line (sort-by #(get-in % [:card :title]) (last group))]
                      [:div.line
                       (when (:edit state)
                         (let [ch (om/get-state owner :edit-channel)]
                           [:span
                            [:button.small {:on-click #(put! ch {:qty 1 :card (:card line)})
                                            :type "button"} "+"]
                            [:button.small {:on-click #(put! ch {:qty -1 :card (:card line)})
                                            :type "button"} "-"]]))
                       (:qty line) " "
                       (if-let [name (get-in line [:card :title])]
                         (let [card (:card line)
                               infaction (noinfcost? identity card)
                               wanted (mostwanted? card)
                               valid (and (allowed? card identity)
                                          (legal-num-copies? line))
                               released (released? card)]
                           [:span
                            [:span {:class (cond
                                             (and valid released) "fake-link"
                                             valid "casual"
                                             :else "invalid")
                                    :on-mouse-enter #(put! zoom-channel card)
                                    :on-mouse-leave #(put! zoom-channel false)} name]
                            (when (or wanted (not infaction))
                              (let [influence (* (:factioncost card) (:qty line))]
                                (list " " [:span.influence
                                 {:class (faction-label card)
                                  :dangerouslySetInnerHTML
                                  #js {:__html (str (if-not infaction (influence-dots influence))
                                                    (if wanted (restricted-dots (:qty line))))}}])))])
                         (:card line))])])]]))]

          [:div.deckedit
           [:div
            [:p
             [:h3 "Deck name"]
             [:input.deckname {:type "text" :placeholder "Deck name"
                               :ref "deckname" :value (get-in state [:deck :name])
                               :on-change #(om/set-state! owner [:deck :name] (.. % -target -value))}]]
            [:p
             [:h3 "Identity"]
             [:select.identity {:value (get-in state [:deck :identity :title])
                                :on-change #(om/set-state! owner [:deck :identity] (get-card (.. % -target -value)))}
              (for [card (sort-by :title (side-identities (get-in state [:deck :identity :side])))]
                [:option (:title card)])]]
            (om/build card-lookup cursor {:state state})
            [:h3 "Decklist"
             [:span.small "(Type or paste a decklist, it will be parsed)" ]]]
           [:textarea {:ref "deck-edit" :value (:deck-edit state)
                       :on-change #(handle-edit owner)}]]]]]))))

(go (let [cards (<! cards-channel)
          decks (process-decks (:json (<! (GET (str "/data/decks")))))]
      (load-decks decks)
      (>! cards-channel cards)))

(om/root deck-builder app-state {:target (. js/document (getElementById "deckbuilder"))})
