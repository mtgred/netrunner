(ns nr.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
   [cljs.core.async :refer [<! >! chan put! timeout close! go-loop] :as async]
   [clojure.string :refer [join lower-case split split-lines] :as s]
   [jinteki.cards :refer [all-cards] :as cards]
   [jinteki.utils :refer [INFINITY str->int] :as utils]
   [jinteki.validator :as validator]
   [nr.ajax :refer [DELETE GET POST PUT]]
   [nr.appstate :refer [app-state]]
   [nr.auth :refer [authenticated] :as auth]
   [nr.cardbrowser :refer [cards-channel factions filter-title image-url] :as cb]
   [nr.deck-status :refer [deck-status-span]]
   [nr.translations :refer [tr tr-faction tr-format tr-side tr-type]]
   [nr.utils :refer [alliance-dots banned-span cond-button
                     deck-points-card-span dots-html format->slug format-date-time
                     influence-dot influence-dots mdy-formatter non-game-toast num->percent
                     restricted-span rotated-span set-scroll-top slug->format store-scroll-top]]
   [nr.ws :as ws]
   [reagent-modals.modals :as reagent-modals]
   [reagent.core :as r]))

(def select-channel (chan))
(def zoom-channel (chan))

(defonce db-dom (atom {}))

(defn- format-status-impl
  [format card]
  (get-in card [:format (keyword format)] "unknown"))

(def format-status (fnil format-status-impl :standard {}))

(defn identical-cards?
  [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))

(defn no-inf-cost?
  [identity card]
  (or (= (:faction card) (:faction identity))
      (= 0 (:factioncost card))
      (= INFINITY (validator/id-inf-limit identity))))

(defn take-best-card
  "Returns a non-rotated card from the list of cards or a random rotated card from the list"
  [cards]
  (let [non-rotated (filter #(not (:rotated %)) cards)]
    (if (not-empty non-rotated)
      (first non-rotated)
      (first cards))))

(defn filter-exact-title [query cards]
  (filter #(or (= (lower-case (:title % "")) query)
               (= (:normalizedtitle %) query))
          cards))

(defn lookup
  "Lookup the card title (query) looking at all cards on specified side"
  [side card]
  (let [id (:id card)
        cards (filter #(= (:side %) side) (vals @all-cards))
        first-id (first (filter #(= id (:code %)) cards))]
    (if (and id first-id)
      first-id
      (let [q (lower-case (:title card ""))
            exact-matches (filter-exact-title q cards)]
        (if (not-empty exact-matches)
          (take-best-card exact-matches)
          (loop [i 2
                 matches cards]
            (let [subquery (subs q 0 i)]
              (cond
                (zero? (count matches))
                card

                (or (= (count matches) 1) (identical-cards? matches))
                (take-best-card matches)

                (<= i (count (:title card)))
                (recur (inc i) (filter-title subquery matches))

                :else
                card))))))))

(defn- build-identity-name
  [title setname]
  (if setname
    (str title " (" setname ")")
    title))

(defn parse-identity
  "Parse an id to the corresponding card map"
  [{:keys [side title setname]}]
  (if (empty? title)
    {:display-name "Missing Identity"}
    (let [card (lookup side {:title title})]
      (assoc card :display-name (build-identity-name title setname)))))

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
          allowed-keys ["id" "art"]]
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
  ;; create a backing map of title to {:qty n :card title}
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

(defn process-cards-in-deck
  "Process the raw deck from the database into a more useful format"
  [deck]
  (if (:parsed? deck)
    deck
    (let [cards (lookup-deck (:side (:identity deck)) (:cards deck))]
      (assoc deck :cards cards :parsed? true))))

(defn load-decks [decks]
  (let [decks (sort-by :date > decks)]
    (swap! app-state assoc :decks decks)
    (swap! app-state assoc :decks-loaded true)))

(defn- add-deck-name
  [all-titles card]
  (let [card-title (:title card)
        indexes (keep-indexed #(if (= %2 card-title) %1 nil) all-titles)
        dups (> (count indexes) 1)]
    (if dups
      (assoc card :display-name (str (:title card) " (" (:setname card) ")"))
      (assoc card :display-name (:title card)))))

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

(defn deck->str [s]
  (let [cards (get-in @s [:deck :cards])
        deck-string (join "\n"
                          (for [line cards]
                            (str (:qty line) " "
                                 (get-in line [:card :title])
                                 (insert-params line))))]
    (swap! s assoc :deck-edit deck-string)))

(defn edit-deck [s]
  (let [deck (:deck @s)]
    (swap! s assoc :old-deck deck)
    (swap! s assoc :edit true)
    (deck->str s)
    (-> (:viewport @db-dom) js/$ (.addClass "edit"))
    (go (<! (timeout 500))
        (-> (:deckname @db-dom) js/$ .select))))

(defn end-edit [s]
  (swap! s assoc :edit false)
  (swap! s assoc :query "")
  (-> (:viewport @db-dom) js/$ (.removeClass "edit")))

(defn cancel-edit [s]
  (let [deck (:deck @s)
        decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))
        selected (or (:old-deck @s) (first decks))]
    (end-edit s)
    (load-decks decks)
    (put! select-channel selected)))

(defn delete-deck [s]
  (swap! s assoc :delete true)
  (deck->str s)
  (-> (:viewport @db-dom) js/$ (.addClass "delete")))

(defn end-delete [s]
  (swap! s assoc :delete false)
  (-> (:viewport @db-dom) js/$ (.removeClass "delete")))

(defn set-deck-on-state
  [s deck]
  (let [deck (process-cards-in-deck deck)
        decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))]
    (load-decks (conj decks deck))
    (swap! s assoc :deck deck)))

(defn handle-delete [s]
  (authenticated
    (fn [_]
      (let [deck (:deck @s)]
        (go (let [response (<! (DELETE (str "/data/decks/" (:_id deck))))]
              (when (= 200 (:status response))
                (load-decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state)))
                (swap! s assoc :deck nil)
                (end-delete s))))))))

(defn side-identities [side]
  (let [cards (->> (vals @all-cards)
                   (filter #(and (= (:side %) side)
                                 (= (:type %) "Identity"))))
        all-titles (map :title cards)
        add-deck (partial add-deck-name all-titles)]
    (map add-deck cards)))

(defn new-deck [s side]
  (let [old-deck (:deck @s)
        id (->> (side-identities side)
                (sort-by :title)
                first)]
    (set-deck-on-state s {:name "New deck"
                          :cards []
                          :identity id
                          :format "standard"
                          :_id (.getTime (js/Date.))
                          :new true})
    (edit-deck s)
    (swap! s assoc :old-deck old-deck)))

(defn- send-import [s]
  (ws/ws-send! [:decks/import {:input (:msg @s)}])
  (reagent-modals/close-modal!))

(defn import-deck-modal []
  (r/with-let [s (r/atom {})]
    [:div
     [:h3 (tr [:deck-builder.import-title "Enter a Public NRDB Deck ID or URL"])]
     [:p [:input.url {:type "text"
                      :id "nrdb-input"
                      :placeholder (tr [:deck-builder.import-placeholder "NRDB ID"])
                      :value (:msg @s)
                      :on-key-press #(when (= 13 (.. % -charCode))
                                       (send-import s))
                      :on-change #(swap! s assoc :msg (-> % .-target .-value))}]]
     [:p.float-right
      (let [disabled (empty? (:msg @s))]
        [:button
         {:disabled disabled
          :class (when disabled "disabled")
          :on-click #(send-import s)}
         (tr [:deck-builder.import "Import"])])
      [:button {:on-click #(reagent-modals/close-modal!)} (tr [:deck-builder.cancel "Cancel"])]]]))

(defn load-decks-from-json
  [json]
  (when-not (= {:message "Not authorized"} json)
    (for [deck json]
      (assoc deck
             :identity (parse-identity (:identity deck))
             :cards (:cards deck)
             :parsed? false))))

(defn save-deck [s]
  (authenticated
    (fn [_]
      (end-edit s)
      (let [deck (assoc (:deck @s) :date (.toJSON (js/Date.)))
            new? (:new deck)
            deck (dissoc deck :stats :new)
            cards (for [card (:cards deck)
                        :when (get-in card [:card :title])
                        :let [card-map {:qty (:qty card)
                                        :card (get-in card [:card :title])}
                              card-id (if (contains? card :id)
                                        (conj card-map {:id (:id card)})
                                        card-map)]]
                    (if (contains? card :art)
                      (conj card-id {:art (:art card)})
                      card-id))
            ;; only include keys that are relevant
            id (select-keys (:identity deck) [:title :side :code])
            deck (if new? (dissoc deck :_id) deck)
            data (assoc deck :cards cards :identity id)]
        (go (let [response (<! (if new?
                                 (POST "/data/decks" data :json)
                                 (PUT "/data/decks" data :json)))
                  new-id (get-in response [:json :_id])
                  new-deck (if new? (assoc deck :_id new-id) deck)
                  json (:json (<! (GET "/data/decks")))
                  all-decks (load-decks-from-json json)]
              (when (= 200 (:status response))
                (set-deck-on-state s new-deck)
                (load-decks all-decks))))))))

(defn clear-deck-stats [s]
  (authenticated
    (fn [_]
      (let [deck (dissoc (:deck @s) :stats)
            decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))]
        (go (<! (DELETE (str "/profile/stats/deck/" (:_id deck))))
            (swap! app-state assoc :decks (conj decks deck))
            (set-deck-on-state s deck)
            (put! select-channel (:deck @s)))))))

(defn card-influence-html
  "Returns hiccup-ready vector with dots for influence as well as rotated / restricted / banned symbols"
  [format card qty in-faction allied?]
  (let [influence (* (:factioncost card) qty)
        card-status (format-status format card)
        banned (:banned card-status)
        restricted (:restricted card-status)
        rotated (:rotated card-status)
        points (:points card-status)]
    [:span " "
     (when (and (not banned) (not in-faction))
       [:span.influence {:key "influence"
                         :class (utils/faction-label card)}
        (if allied?
          (alliance-dots influence)
          (influence-dots influence))])
     (if banned
       banned-span
       [:span {:key "restricted"}
        (when restricted restricted-span)
        (when rotated rotated-span)
        (when points (deck-points-card-span points))])]))

(defn deck-influence-html
  "Returns hiccup-ready vector with dots colored appropriately to deck's influence."
  [deck]
  (dots-html influence-dot (validator/influence-map deck)))

(defn distinct-by [f coll]
  (letfn [(step [xs seen]
            (lazy-seq (when-let [[x & more] (seq xs)]
                        (let [k (f x)]
                          (if (seen k)
                            (step more seen)
                            (cons x (step more (conj seen k))))))))]
    (step coll #{})))

(defn match [identity query]
  (->> (vals @all-cards)
       (filter #(validator/allowed? % identity))
       (distinct-by :title)
       (filter-title query)
       (take 10)))

(defn handle-keydown [s event]
  (let [selected (:selected @s)
        matches (:matches @s)]
    (case (.-keyCode event)
      38 (when (pos? selected)
           (swap! s update :selected dec))
      40 (when (< selected (dec (count matches)))
           (swap! s update :selected inc))
      (9 13) (when-not (= (:query @s) (:title (first matches)))
               (.preventDefault event)
               (-> ".deckedit .qty" js/$ .select)
               (swap! s assoc :query (:title (nth matches selected))))
      (swap! s assoc :selected 0))))

(defn update-decklist-cards
  [s edit]
  (let [card (:card edit)
        max-qty (:deck-limit card 3)
        cards (vec (get-in @s [:deck :cards]))
        match? (fn [idx item]
                 (when (= (lower-case (get-in item [:card :title] ""))
                          (lower-case (:title card "")))
                   idx))
        existing-line-idx (first (keep-indexed match? cards))
        existing-line (when existing-line-idx
                        (nth cards existing-line-idx))
        new-qty (+ (or (:qty existing-line) 0) (:qty edit))
        draft-id (validator/draft-id? (get-in @s [:deck :identity]))
        new-cards (cond
                    (and existing-line-idx
                         (not draft-id)
                         (> new-qty max-qty))
                    (update cards existing-line-idx assoc :qty max-qty)

                    (and existing-line-idx
                         (not (pos? new-qty)))
                    (concat (subvec cards 0 existing-line-idx)
                            (subvec cards (inc existing-line-idx)))

                    existing-line-idx
                    (update cards existing-line-idx assoc :qty new-qty)

                    :else
                    (concat cards [{:qty new-qty
                                    :card card}]))]
    (swap! s assoc-in [:deck :cards] new-cards)
    (deck->str s)))

(defn handle-add [s card-state event]
  (.preventDefault event)
  (let [qty (str->int (:quantity @card-state))
        card (nth (:matches @card-state) (:selected @card-state) nil)
        best-card (lookup (:side card) card)]
    (if (js/isNaN qty)
      (swap! card-state assoc :quantity 3)
      (let [max-qty (:deck-limit best-card 3)
            limit-qty (if (> qty max-qty) max-qty qty)]
        (update-decklist-cards s {:qty limit-qty
                                  :card best-card})
        (reset! card-state {:query ""
                            :matches []
                            :quantity 3
                            :selected 0})
        (-> ".deckedit .lookup" js/$ .select)))))

(defn card-lookup []
  (let [card-state (r/atom {:query ""
                            :matches []
                            :quantity 3
                            :selected 0})]
    (fn [s]
      [:div
       [:h3 (tr [:deck-builder.add-cards "Add cards"])]
       [:form.card-search {:on-submit #(handle-add s card-state %)}
        [:input.lookup {:type "text"
                        :placeholder (tr [:deck-builder.card-name "Card name"])
                        :value (:query @card-state)
                        :on-change #(swap! card-state assoc :query (.. % -target -value))
                        :on-key-down #(handle-keydown card-state %)}]
        " x "
        [:input.qty {:type "text"
                     :value (:quantity @card-state)
                     :on-change #(swap! card-state assoc :quantity (.. % -target -value))}]
        [:button (let [disabled (empty? (:matches @card-state))]
                   {:disabled disabled
                    :class (when disabled "disabled")})
         (tr [:deck-builder.add-to-deck "Add to deck"])]
        (let [query (:query @card-state)
              matches (match (get-in @s [:deck :identity]) query)
              exact-match (= (:title (first matches)) query)]
          (cond
            (empty? query) nil

            exact-match
            (do
              (swap! card-state assoc :matches matches)
              (swap! card-state assoc :selected 0)
              "")

            (not (or (empty? query) exact-match))
            (do
              (swap! card-state assoc :matches matches)
              [:div.typeahead
               (doall (for [i (range (count matches))]
                        [:div {:class (if (= i (:selected @card-state)) "selected" "")
                               :on-click (fn [e] (-> ".deckedit .qty" js/$ .select)
                                           (swap! card-state assoc :query (.. e -target -textContent))
                                           (swap! card-state assoc :selected i)
                                           nil)
                               :key (:title (nth matches i))}
                         (:title (nth matches i))]))])))]])))

(defn deck-name
  ([deck] (deck-name deck 40))
  ([deck limit]
   (let [deck-name (:name deck)]
     (str (s/trim (subs deck-name 0 limit))
          (when (< limit (count deck-name)) "...")))))

(defn deck-date [deck]
  (when-let [date (:date deck)]
    (format-date-time mdy-formatter date)))

(defn deck-stats-line [deck]
  (r/with-let [deckstats (r/cursor app-state [:options :deckstats])]
    (when (and (:stats deck) (not= "none" @deckstats))
      (let [stats (:stats deck)
            games (or (:games stats) 0)
            started (or (:games-started stats) 0)
            completed (or (:games-completed stats) 0)
            wins (or (:wins stats) 0)
            losses (or (:loses stats) 0)]
        [:p
         ; adding key :games to handle legacy stats before adding started vs completed
         "  " (tr [:deck-builder.games "Games"]) ": " (+ started games)
         " - " (tr [:deck-builder.completed "Completed"]) ": " (+ completed games)
         " - " (tr [:deck-builder.won "Won"]) ": " wins " (" (num->percent wins (+ wins losses)) "%)"
         " - " (tr [:deck-builder.lost "Lost"]) ": " losses]))))

(defn deck-entry [s deck]
  (r/with-let [state-deck (r/cursor s [:deck])]
    [:div.deckline {:class (when (= (:_id deck) (:_id @state-deck)) "active")
                    :on-click #(put! select-channel deck)}
     [:img {:src (image-url (:identity deck))
            :alt (get-in deck [:identity :title] "")}]
     [:span.float-right
      [deck-status-span deck]
      [:p (deck-date deck)]]
     [:h4 (deck-name deck)]
     [:span (get-in deck [:identity :title])]
     [deck-stats-line deck]]))

(def all-sides-filter "Any Side")
(def all-factions-filter "Any Faction")
(def all-formats-filter "Any Format")

(defn- filter-side [side-filter decks]
  (if (= all-sides-filter @side-filter)
    decks
    (filter #(= (get-in % [:identity :side]) @side-filter) decks)))

(defn- filter-faction [faction-filter decks]
  (if (= all-factions-filter @faction-filter)
    decks
    (filter #(= (get-in % [:identity :faction]) @faction-filter) decks)))

(defn- filter-format [fmt-filter decks]
  (if (= all-formats-filter @fmt-filter)
    decks
    (let [fmt-slug (format->slug @fmt-filter)]
      (filter #(= (:format %) fmt-slug) decks))))

(defn- filter-selected [side-filter faction-filter fmt-filter]
  (not (and (= all-sides-filter @side-filter)
            (= all-factions-filter @faction-filter)
            (= all-formats-filter @fmt-filter))))

(defn decks-list [_ _ scroll-top]
  (r/create-class
    {:display-name "deck-collection"
     :component-did-mount #(set-scroll-top % @scroll-top)
     :component-will-unmount #(store-scroll-top % scroll-top)
     :reagent-render
     (fn [filtered-decks s _]
       (into [:div.deck-collection]
             (for [deck (sort-by (juxt :date :_id) > filtered-decks)]
               ^{:key (:_id deck)}
               [deck-entry s deck])))}))

(defn deck-collection
  [state decks decks-loaded scroll-top]
  (r/with-let [side-filter (r/cursor state [:side-filter])
               faction-filter (r/cursor state [:faction-filter])
               fmt-filter (r/cursor state [:format-filter])]
    (when-not (:edit @state)
      (cond
        (not @decks-loaded)
        [:div.deck-collection
         [:h4 (tr [:deck-builder.loading-msg "Loading deck collection..."])]]
        (empty? @decks)
        [:div.deck-collection
         [:h4 (tr [:deck-builder.no-decks "No decks"])]]
        :else
        (let [filtered-decks (->> @decks
                                  (filter-side side-filter)
                                  (filter-faction faction-filter)
                                  (filter-format fmt-filter))
              n (count filtered-decks)
              deck-str (tr [:deck-builder.deck-count] n)]
          [:<>
           [:div.deck-count
            [:h4 (str deck-str (when (filter-selected side-filter faction-filter fmt-filter)
                                 (str "  " (tr [:deck-builder.filtered "(filtered)"]))))]]
           [decks-list filtered-decks state scroll-top]])))))

(defn line-span
  "Make the view of a single line in the deck - returns a span"
  [{:keys [identity cards format] :as deck} {:keys [qty card] :as line}]
  [:span qty "  "
   (if-let [title (:title card)]
     (let [infaction (no-inf-cost? identity card)
           card-status (format-status format card)
           banned (:banned card-status)
           rotated (:rotated card-status)
           allied (validator/alliance-is-free? cards line)
           valid (and (validator/allowed? card identity)
                      (validator/legal-num-copies? identity line))
           modqty (if (validator/is-prof-prog? deck card) (- qty 1) qty)]
       [:span
        [:span {:class (str "fake-link"
                            (cond rotated " casual"
                                  banned " invalid"
                                  (not valid) " invalid"))
                :on-mouse-enter #(when (:setname card) (put! zoom-channel line))
                :on-mouse-leave #(put! zoom-channel false)} title]
        [card-influence-html format card modqty infaction allied]])
     card)])

(defn line-qty-span
  "Make the view of a single line in the deck - returns a span"
  [{:keys [qty card]}]
  [:span {:key (:code card)} qty "  "])

(defn line-name-span
  "Make the view of a single line in the deck - returns a span"
  [{:keys [identity cards format] :as deck} {:keys [qty card] :as line}]
  [:span (if-let [name (:title card)]
           (let [infaction (no-inf-cost? identity card)
                 card-status (format-status format card)
                 banned (:banned card-status)
                 rotated (:rotated card-status)
                 allied (validator/alliance-is-free? cards line)
                 valid (and (validator/allowed? card identity)
                            (validator/legal-num-copies? identity line))
                 modqty (if (validator/is-prof-prog? deck card)
                          (- qty 1)
                          qty)]
             [:span
              [:span {:class (str "fake-link"
                                  (cond rotated " casual"
                                        banned " invalid"
                                        (not valid) " invalid"))
                      :on-mouse-enter #(when (:setname card) (put! zoom-channel line))
                      :on-mouse-leave #(put! zoom-channel false)} name]
              (card-influence-html format card modqty infaction allied)])
           card)])

(defn- build-deck-points-tooltip [deck]
  (let [fmt (keyword (:format deck))
        pointed-cards (->> (validator/filter-cards-by-legal-status deck :points)
                           (map #(assoc {} :title (get-in % [:card :title])
                                           :points (get-in % [:card :format fmt :points]))))]
    [:div.status-tooltip.blue-shade
     (doall (for [{:keys [title points]} (sort-by :title pointed-cards)]
              ^{:key title}
              [:div
               [:span.tick.fake-link title ": " points [deck-points-card-span]]]))]))

(defn deck-points-span [deck]
  (let [deck-points (validator/deck-point-count deck)
        point-limit (validator/format-point-limit (:format deck))]
    [:span.deck-status.shift-tooltip
     [:span (str (tr [:deck-builder.deck-points "Deck points"]) ": ")]
     [:span {:class (if (> deck-points point-limit)
                      "invalid"
                      "legal")}
      deck-points]
     [:span "/" point-limit [deck-points-card-span]]
     (when (pos? deck-points)
       (build-deck-points-tooltip deck))]))

(defn decklist-header
  [deck cards]
  (let [id (:identity deck)]
    [:div.header
     [:img {:src (image-url id)
            :alt (:title id)}]
     [:div.header-text
      [:h4 {:class (str "fake-link"
                        (let [status (format-status (:format deck) id)]
                          (cond (:rotated status) " casual"
                                (:banned status) " invalid")))
            :on-mouse-enter #(put! zoom-channel {:card id
                                                 :art (:art id)
                                                 :id (:id id)})
            :on-mouse-leave #(put! zoom-channel false) }
       (:title id)
       (let [status (format-status (:format deck) id)]
         (cond (:banned status) banned-span
               (:restricted status) restricted-span
               (:rotated status) rotated-span
               (:points status) (deck-points-card-span (:points status))))]
      (let [count (validator/card-count cards)
            min-count (validator/min-deck-size id)]
        [:div count (str " " (tr [:deck-builder.cards "cards"]))
         (when (< count min-count)
           [:span.invalid (str " (" (tr [:deck-builder.min "minimum"]) " " min-count ")")])])
      (let [inf (validator/influence-count deck)
            id-limit (validator/id-inf-limit id)]
        [:div (str (tr [:deck-builder.influence "Influence"]) ": ")
         ;; we don't use valid? and mwl-legal? functions here, since it concerns influence only
         [:span {:class (if (> inf id-limit)
                          (if (> inf id-limit)
                            "invalid"
                            "casual")
                          "legal")}
          inf]
         "/" (if (= INFINITY id-limit) "∞" id-limit)
         " "
         (when (pos? inf)
           (deck-influence-html deck))])
      (when (= (:side id) "Corp")
        (let [min-point (validator/min-agenda-points deck)
              points (validator/agenda-points deck)]
          [:div (str (tr [:deck-builder.agenda-points "Agenda points"]) ": " points)
           (when (< points min-point)
             [:span.invalid " (" (tr [:deck-builder.min "minimum"]) " " min-point ")"])
           (when (> points (inc min-point))
             [:span.invalid " (" (tr [:deck-builder.max "maximum"]) " " (inc min-point) ")"])]))
      (when (validator/format-point-limit (:format deck))
        [:div [deck-points-span deck]])
      [:div [deck-status-span deck true true false]]
      (when (:hash deck) [:div (tr [:deck-builder.hash "Tournament hash"]) ": " (:hash deck)])]]))

(defn decklist-contents
  [s deck cards]
  [:div.cards
   (doall
     (for [group (sort-by first (group-by #(get-in % [:card :type]) cards))]
       ^{:key (or (first group) "Unknown")}
       [:div.group
        [:h4 (str (tr-type (or (first group) "Unknown")) " (" (validator/card-count (last group)) ")") ]
        (doall
          (for [line (sort-by #(get-in % [:card :title]) (last group))]
            ^{:key (or (get-in line [:card :code]) line)}
            [:div.line
             (if (:edit @s)
               [:span
                [:button.small {:on-click #(update-decklist-cards
                                             s {:qty -1
                                                :card (:card line)})
                                :type "button"} "-"]
                [line-qty-span line]
                [:button.small {:on-click #(update-decklist-cards
                                             s {:qty 1
                                                :card (:card line)})
                                :type "button"} "+"]
                [line-name-span deck line]]
               [line-span deck line])]))]))])

(defn decklist-notes
  [deck]
  [:div.notes (:notes deck "")])

(defn edit-buttons
  [s]
  [:div.button-bar
   [:button {:on-click #(save-deck s)} (tr [:deck-builder.save "Save"])]
   [:button {:on-click #(cancel-edit s)} (tr [:deck-builder.cancel "Cancel"])]])

(defn delete-buttons
  [s]
  [:div.button-bar
   [:button {:on-click #(handle-delete s)} (tr [:deck-builder.confirm-delete "Confirm Delete"])]
   [:button {:on-click #(end-delete s)} (tr [:deck-builder.cancel "Cancel"])]])

(defn view-buttons
  [s deck]
  [:div.button-bar
   [cond-button (tr [:deck-builder.edit "Edit"])
    (not (:locked deck))
    #(edit-deck s)]
   [:button {:on-click #(delete-deck s)} (tr [:deck-builder.delete "Delete"])]
   (when (and (:stats deck)
              (not= "none" (get-in @app-state [:options :deckstats])))
     [:button {:on-click #(clear-deck-stats s)}
      (tr [:deck-builder.clear-stats "Clear Stats"])])
   ;; (let [disabled (or (:editing-game @app-state false)
   ;;                    (:gameid @app-state false)
   ;;                    (and (not= (:format deck) "casual")
   ;;                         (not (validator/legal-deck? deck))))]
   ;;   [:button.float-right {:href "/play"
   ;;                         :on-click #(do
   ;;                                      (swap! app-state assoc :create-game-deck (:deck @s))
   ;;                                      ; (.setToken history "/play")
   ;;                                      )
   ;;                         :disabled disabled
   ;;                         :class (when disabled "disabled")}
   ;;    (tr [:deck-builder.create-game "Create Game"])])
   ])

(defn selected-panel
  [s]
  [:div.decklist
   (let [deck (:deck @s)
         cards (:cards deck)]
     (when deck
       [:div
        (cond
          (:edit @s) [edit-buttons s]
          (:delete @s) [delete-buttons s]
          :else [view-buttons s deck])
        [:h3 (:name deck)]
        [decklist-header deck cards]
        [decklist-contents s deck cards]
        (when-not (:edit @s)
          [decklist-notes deck])]))])

(defn deck-name-editor
  [s]
  [:div
   [:h3 (tr [:deck-builder.deck-name "Deck name"])]
   [:input.deckname
    {:type "text"
     :placeholder (tr [:deck-builder.deck-name "Deck name"])
     :ref #(swap! db-dom assoc :deckname %)
     :value (get-in @s [:deck :name])
     :on-change #(swap! s assoc-in [:deck :name] (.. % -target -value))}]])

(defn format-editor
  [s]
  [:div
   [:h3 (tr [:deck-builder.format "Format"])]
   [:select.format {:value (get-in @s [:deck :format] "standard")
                    :on-change #(swap! s assoc-in [:deck :format] (.. % -target -value))}
    (doall
      (for [[k v] slug->format]
        ^{:key k}
        [:option {:value k} (tr-format v)]))]])

(defn- identity-option-string
  [card]
  (.stringify js/JSON (clj->js {:title (:title card)
                                :id (:code card)})))

(defn- create-identity
  [s target-value]
  (let [side (get-in @s [:deck :identity :side])
        json-map (.parse js/JSON (.. target-value -target -value))
        id-map (js->clj json-map :keywordize-keys true)]
    (lookup side id-map)))

(defn identity-editor
  [s]
  [:div
   [:h3 (tr [:deck-builder.identity "Identity"])]
   [:select.identity {:value (identity-option-string (get-in @s [:deck :identity]))
                      :on-change #(swap! s assoc-in [:deck :identity] (create-identity s %))}
    (let [idents (side-identities (get-in @s [:deck :identity :side]))]
      (for [card (sort-by :display-name idents)]
        ^{:key (:display-name card)}
        [:option
         {:value (identity-option-string card)}
         (:display-name card)]))]])

(defn parse-deck-string
  "Parses a string containing the decklist and returns a list of lines {:qty :card}"
  [side deck-string]
  (let [raw-deck-list (deck-string->list deck-string)]
    (lookup-deck side raw-deck-list)))

(defn handle-edit [s]
  (let [text (.-value (:deckedit @db-dom))
        side (get-in @s [:deck :identity :side])
        cards (parse-deck-string side text)]
    (swap! s assoc :deck-edit text)
    (swap! s assoc-in [:deck :cards] cards)))

(defn edit-textbox
  [s]
  [:textarea {:ref #(swap! db-dom assoc :deckedit %)
              :value (:deck-edit @s)
              :on-change #(handle-edit s)}])

(defn notes-textbox
  [s]
  [:textarea.notes-edit
   {:placeholder (tr [:deck-builder.deck-notes "Deck notes"])
    :ref #(swap! db-dom assoc :deck-notes %)
    :value (get-in @s [:deck :notes])
    :on-change #(swap! s assoc-in [:deck :notes] (.. % -target -value))}])

(defn edit-panel
  [s]
  [:div.deckedit
   [deck-name-editor s]
   [format-editor s]
   [identity-editor s]
   [card-lookup s]
   [:div
    [:h3 (tr [:deck-builder.decklist "Decklist"])
     [:span.small (tr [:deck-builder.decklist-inst "(Type or paste a decklist, it will be parsed)"])]]]
   [edit-textbox s]
   [:div
    [:h3 (tr [:deck-builder.notes "Notes"])]]
   [notes-textbox s]])

(defn- reset-deck-filters [state]
  (swap! state assoc
         :side-filter all-sides-filter
         :faction-filter all-factions-filter
         :format-filter all-formats-filter))

(defn collection-buttons [s user decks-loaded]
  [:div.button-bar
   [cond-button (tr [:deck-builder.new-corp "New Corp deck"])
   (and @user @decks-loaded) #(do
                                (reset-deck-filters s)
                                (new-deck s "Corp"))]
   [cond-button (tr [:deck-builder.new-runner "New Runner deck"])
    (and @user @decks-loaded) #(do
                                 (reset-deck-filters s)
                                 (new-deck s "Runner"))]
   [cond-button (tr [:deck-builder.import-button "Import deck"]) (and @user @decks-loaded)
    #(reagent-modals/modal! [import-deck-modal]
                            {:shown (fn [] (.focus (.getElementById js/document "nrdb-input")))})]])

(defn- simple-filter-builder
  [state state-key options decks-loaded callback scroll-top translator]
  [:select.deckfilter-select {:class (if-not @decks-loaded "disabled" state-key)
                              :value (get @state state-key)
                              :on-change #(do (swap! state assoc state-key (.. % -target -value))
                                              (reset! scroll-top 0)
                                              (when callback
                                                (callback state)))}
   (doall
     (for [option options]
       ^{:key option}
       [:option.deckfilter-option {:value option
                                   :key option
                                   :dangerouslySetInnerHTML #js {:__html (translator option)}}]))])

(defn- handle-side-changed [state]
  (swap! state assoc :faction-filter all-factions-filter))

(defn- filter-builder
  [state decks-loaded scroll-top]
  (let [formats (-> format->slug keys butlast)]
    [:div.deckfilter
     (doall
       (for [[state-key options callback translator]
             [[:side-filter [all-sides-filter "Corp" "Runner"] handle-side-changed tr-side]
              [:faction-filter (cons all-factions-filter (factions (:side-filter @state))) nil tr-faction]
              [:format-filter (cons all-formats-filter formats) nil tr-format]]]
         ^{:key state-key}
         [simple-filter-builder state state-key options decks-loaded callback scroll-top translator]))

     [:button {:class (if-not @decks-loaded "disabled" "")
               :on-click #(reset-deck-filters state)}
      (tr [:deck-builder.reset "Reset"])]]))

(defn- zoom-card-view [card state]
  [card state]
  (when-let [url (image-url card)]
    [:div.card-preview.blue-shade
     [:img {:src url
            :alt (:title card)}]]))

(defn list-panel
  [s user decks decks-loaded scroll-top]
  (r/with-let [zoom-card (r/cursor s [:zoom])]
    [:div.decks
     [collection-buttons s user decks-loaded]
     [filter-builder s decks-loaded scroll-top]
     [deck-collection s decks decks-loaded scroll-top]
     [:div {:class (when (:edit @s) "edit")}
      (when-let [line @zoom-card]
        (let [art (:art line)
              id (:id line)
              updated-card (add-params-to-card (:card line) id art)]
          [zoom-card-view updated-card s]))]]))

(defn- class-for-state [s]
  (r/with-let [edit (r/cursor s [:edit])
               delete (r/cursor s [:delete])]
    (cond
      @edit "edit"
      @delete "delete"
      :else "")))

(defn deck-builder
  "Make the deckbuilder view"
  []
  (let [s (r/atom {:edit false
                   :old-deck nil
                   :deck nil
                   :side-filter all-sides-filter
                   :faction-filter all-factions-filter
                   :format-filter all-formats-filter})
        decks (r/cursor app-state [:decks])
        user (r/cursor app-state [:user])
        decks-loaded (r/cursor app-state [:decks-loaded])
        scroll-top (atom 0)]
    (r/create-class
      {:display-name "deck-builder"
       :component-did-mount
       (fn [_comp]
         (go-loop [card (<! zoom-channel)]
                  (when-not (= :exit card)
                    (swap! s assoc :zoom card)
                    (recur (<! zoom-channel))))

         (go-loop [deck (<! select-channel)]
                  (when-not (= :exit deck)
                    (end-delete s)
                    (set-deck-on-state s deck)
                    (recur (<! select-channel)))))

       :component-will-unmount
       (fn [_comp]
         (put! zoom-channel :exit)
         (put! select-channel :exit))

       :reagent-render
       (fn []
         [:div.container
          [:div.deckbuilder-bg]
          [:div.deckbuilder.blue-shade.panel
           [:div.viewport {:ref #(swap! db-dom assoc :viewport %)
                           :class (class-for-state s)}
            [list-panel s user decks decks-loaded scroll-top]
            [selected-panel s]
            [edit-panel s]]]])})))

(go (let [cards (<! cards-channel)
          json (:json (<! (GET (str "/data/decks"))))
          decks (load-decks-from-json json)]
      (load-decks decks)
      (>! cards-channel cards)))

(defmethod ws/event-msg-handler :decks/import-failure [{data :?data}]
  (non-game-toast data "error" nil))

(defmethod ws/event-msg-handler :decks/import-success [{data :?data}]
  (non-game-toast data "success" nil)
  (go (let [json (:json (<! (GET (str "/data/decks"))))
            decks (load-decks-from-json json)]
        (load-decks decks))))
