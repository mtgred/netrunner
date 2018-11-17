(ns nr.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :refer [split split-lines join escape lower-case] :as s]
            [goog.string :as gstring]
            [goog.string.format]
            [jinteki.cards :refer [all-cards] :as cards]
            [jinteki.decks :as decks]
            [jinteki.utils :refer [str->int INFINITY slugify] :as utils]
            [nr.account :refer [load-alt-arts]]
            [nr.ajax :refer [DELETE GET POST PUT]]
            [nr.appstate :refer [app-state]]
            [nr.auth :refer [authenticated] :as auth]
            [nr.cardbrowser :refer [card-view cards-channel expand-alts filter-title image-url show-alt-art? ] :as cb]
            [nr.utils :refer [alliance-dots banned-span dots-html influence-dot influence-dots make-dots restricted-span rotated-span]]
            [reagent.core :as r]))


(def select-channel (chan))
(def zoom-channel (chan))

(defonce db-dom (atom {}))

(defn num->percent
  "Converts an input number to a percent of the second input number for display"
  [num1 num2]
  (if (zero? num2)
    "0"
    (gstring/format "%.0f" (* 100 (float (/ num1 num2))))))

(defn identical-cards?
  [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))

(defn no-inf-cost?
  [identity card]
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
  (let [lcquery (lower-case query)]
    (filter #(or (= (lower-case (:title %)) lcquery)
                 (= (:normalizedtitle %) lcquery))
            cards)))

(defn lookup
  "Lookup the card title (query) looking at all cards on specified side"
  [side card]
  (let [q (lower-case (:title card))
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
  [title setname]
  (if setname
    (str title " (" setname ")")
    title))

(defn parse-identity
  "Parse an id to the corresponding card map"
  [{:keys [side title setname]}]
  (if (nil? title)
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

(defn parse-deck-string
  "Parses a string containing the decklist and returns a list of lines {:qty :card}"
  [side deck-string]
  (let [raw-deck-list (deck-string->list deck-string)]
    (lookup-deck side raw-deck-list)))

(defn load-decks [decks]
  (swap! app-state assoc :decks decks)
  (when-let [selected-deck (first (sort-by :date > decks))]
    (put! select-channel selected-deck))
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
  (let [cards (->> @all-cards
                   (filter #(and (= (:side %) side)
                                 (= (:type %) "Identity")))
                   (sort-by :code)
                   (group-by :title)
                   vals
                   (map last))
        all-titles (map :title cards)
        add-deck (partial add-deck-name all-titles)]
    (map add-deck cards)))

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
        str (reduce #(str %1 (:qty %2) " " (get-in %2 [:card :title]) (insert-params %2) "\n") "" cards)]
    (swap! s assoc :deck-edit str)))

(defn edit-deck [s]
  (let [deck (:deck @s)]
    (swap! s assoc :old-deck deck)
    (swap! s assoc :edit true)
    (deck->str s)
    (-> (:viewport @db-dom) js/$ (.addClass "edit"))
    (try (js/ga "send" "event" "deckbuilder" "edit") (catch js/Error e))
    (go (<! (timeout 500))
        (-> (:deckname @db-dom) js/$ .select))))

(defn end-edit [s]
  (swap! s assoc :edit false)
  (swap! s assoc :query "")
  (-> (:viewport @db-dom) js/$ (.removeClass "edit")))

(defn handle-edit [s]
  (let [text (.-value (:deckedit @db-dom))
        side (get-in @s [:deck :identity :side])
        cards (parse-deck-string side text)]
    (swap! s assoc :deck-edit text)
    (swap! s assoc-in [:deck :cards] cards)))

(defn cancel-edit [s]
  (end-edit s)
  (go (let [deck (:old-deck @s)
            all-decks (process-decks (:json (<! (GET (str "/data/decks")))))]
        (load-decks all-decks)
        (put! select-channel deck))))

(defn delete-deck [s]
  (swap! s assoc :delete true)
  (deck->str s)
  (-> (:viewport @db-dom) js/$ (.addClass "delete"))
  (try (js/ga "send" "event" "deckbuilder" "delete") (catch js/Error e)))

(defn end-delete [s]
  (swap! s assoc :delete false)
  (-> (:viewport @db-dom) js/$ (.removeClass "delete")))

(defn handle-delete [s]
  (authenticated
    (fn [user]
      (let [deck (:deck @s)]
        (try (js/ga "send" "event" "deckbuilder" "delete") (catch js/Error e))
        (go (let [response (<! (DELETE (str "/data/decks/" (:_id deck))))]))
        (do
          (swap! app-state update :decks (fn [ds] (remove #(= deck %) ds)))
          (swap! s assoc :deck (first (sort-by :date > (:decks @app-state))))
          (end-delete s))))))

(defn new-deck [side s]
  (let [old-deck (:deck @s)
        id (->> side
                side-identities
                (sort-by :title)
                first)]
    (swap! s assoc :deck {:name "New deck" :cards [] :identity id})
    (try (js/ga "send" "event" "deckbuilder" "new" side) (catch js/Error e))
    (edit-deck s)
    (swap! s assoc :old-deck old-deck)))

(defn save-deck [s]
  (authenticated
    (fn [user]
      (end-edit s)
      (let [deck (assoc (:deck @s) :date (.toJSON (js/Date.)))
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
            data (assoc deck :cards cards :identity identity)]
        (try (js/ga "send" "event" "deckbuilder" "save") (catch js/Error e))
        (go (let [new-id (get-in (<! (if (:_id deck)
                                       (PUT "/data/decks" data :json)
                                       (POST "/data/decks" data :json)))
                                 [:json :_id])
                  new-deck (if (:_id deck) deck (assoc deck :_id new-id))
                  all-decks (process-decks (:json (<! (GET (str "/data/decks")))))]
              (swap! app-state assoc :decks (conj decks new-deck))
              (swap! s assoc :deck new-deck)
              (load-decks all-decks)))))))

(defn clear-deck-stats [s]
  (authenticated
    (fn [user]
      (let [deck (dissoc (:deck @s) :stats)
            decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))]
        (try (js/ga "send" "event" "deckbuilder" "cleardeckstats") (catch js/Error e))
        (go (let [result (<! (DELETE (str "/profile/stats/deck/" (:_id deck))))]
              (swap! app-state assoc :decks (conj decks deck))
              (swap! s assoc :deck deck)
              (put! select-channel deck)))))))

(defn html-escape [st]
  (escape st {\< "&lt;" \> "&gt;" \& "&amp;" \" "#034;"}))

(defn card-influence-html
  "Returns hiccup-ready vector with dots for influence as well as rotated / restricted / banned symbols"
  [card qty in-faction allied?]
  (let [influence (* (:factioncost card) qty)
        banned (decks/legal? "banned" card)
        restricted (decks/legal? "restricted" card)
        rotated (:rotated card)]
    (list " "
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
             (when rotated rotated-span)]))))

(defn deck-influence-html
  "Returns hiccup-ready vector with dots colored appropriately to deck's influence."
  [deck]
  (dots-html influence-dot (decks/influence-map deck)))

(defn- build-deck-status-label [deck-status violation-details?]
  [:div.status-tooltip.blue-shade
   (doall (for [[status-key {:keys [legal reason description]}] deck-status
                :when description]
            ^{:key status-key}
            [:div {:class (if legal "legal" "invalid")
                   :title (when violation-details? reason)}
             [:span.tick (if legal "✔" "✘")]
             description]))])

(defn- deck-status-details
  [deck use-trusted-info]
  (if use-trusted-info
    (decks/trusted-deck-status deck)
    (decks/calculate-deck-status deck)))

(defn check-deck-status
  [deck-status]
  (let [fmt (:format deck-status)]
    (if (get-in deck-status [(keyword fmt) :legal])
      fmt "invalid")))

(def slug->format
  {"standard" "Standard"
   "eternal" "Eternal"
   "core-experience" "Core Experience"
   "snapshot" "Snapshot"
   "socr8" "Stimhack Online Cache Refresh 8"
   "casual" "Casual"})

(defn format-deck-status-span
  [{:keys [format] :as deck-status} tooltip? violation-details?]
  (let [status (check-deck-status deck-status)
        message (str (get slug->format (:format deck-status) "Standard")
                     " "
                     (if-not (= "invalid" status) "legal" "illegal"))]
    [:span.deck-status.shift-tooltip {:class status} message
     (when tooltip?
       (build-deck-status-label deck-status violation-details?))]))

(defn deck-status-span-impl [deck tooltip? violation-details? use-trusted-info]
  (format-deck-status-span (deck-status-details deck false) tooltip? violation-details?))

(def deck-status-span-memoize (memoize deck-status-span-impl))

(defn deck-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity."
  ([deck] (deck-status-span deck false false true))
  ([deck tooltip? violation-details? use-trusted-info]
   (deck-status-span-memoize deck tooltip? violation-details? use-trusted-info)))

(defn deck-format-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity for a single format."
  [deck format use-trusted-info?]
  (format-deck-status-span (assoc
                             (deck-status-details (assoc deck :format format) use-trusted-info?)
                             :format
                             format)
                           false false))

(defn match [identity query]
  (->> @all-cards
       (filter #(decks/allowed? % identity))
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

(defn handle-add [s event]
  (.preventDefault event)
  (let [qty (str->int (:quantity @s))
        card (nth (:matches @s) (:selected @s))
        best-card (lookup (:side card) card)]
    (if (js/isNaN qty)
      (swap! s assoc :quantity 3)
      (let [max-qty (:deck-limit best-card 3)
            limit-qty (if (> qty max-qty) max-qty qty)]
        (put! (:edit-channel @s)
              {:qty limit-qty
               :card best-card})
        (swap! s assoc :quantity 3)
        (swap! s assoc :query "")
        (-> ".deckedit .lookup" js/$ .select)))))

(defn card-lookup [deck]
  (let [s (r/atom {:query ""
                   :matches []
                   :quantity 3
                   :selected 0
                   :edit-channel (:edit-channel @deck)})]
    (fn [deck]
      [:div
       [:h3 "Add cards"]
       [:form.card-search {:on-submit #(handle-add s %)}
        [:input.lookup {:type "text" :placeholder "Card name" :value (:query @s)
                        :on-change #(swap! s assoc :query (.. % -target -value))
                        :on-key-down #(handle-keydown s %)}]
        " x "
        [:input.qty {:type "text" :value (:quantity @s)
                     :on-change #(swap! s assoc :quantity (.. % -target -value))}]
        [:button "Add to deck"]
        (let [query (:query @s)
              matches (match (get-in @deck [:deck :identity]) query)
              exact-match (= (:title (first matches)) query)]
          (cond
            exact-match
            (do
              (swap! s assoc :matches matches)
              (swap! s assoc :selected 0)
              "")

            (not (or (empty? query) exact-match))
            (do
              (swap! s assoc :matches matches)
              [:div.typeahead
               (doall (for [i (range (count matches))]
                        [:div {:class (if (= i (:selected @s)) "selected" "")
                               :on-click (fn [e] (-> ".deckedit .qty" js/$ .select)
                                           (swap! s assoc :query (.. e -target -textContent))
                                           (swap! s assoc :selected i)
                                           nil)
                               :key (:title (nth matches i))}
                         (:title (nth matches i))]))])))]])))

(defn deck-collection
  [{:keys [sets decks decks-loaded active-deck]}]
  (cond

    (not @decks-loaded)
    [:h4 "Loading deck collection..."]

    (empty? @decks)
    [:h4 "No decks"]

    :else [:div
           (doall
             (for [deck (sort-by :date > @decks)]
               ^{:key (:_id deck)}
               [:div.deckline {:class (when (= active-deck deck) "active")
                               :on-click #(put! select-channel deck)}
                [:img {:src (image-url (:identity deck))
                       :alt (get-in deck [:identity :title] "")}]
                [:div.float-right [deck-status-span deck]]
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
                      " - Lost: " losses]))]]))]))

(defn line-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span qty "  "
   (if-let [name (:title card)]
     (let [infaction (no-inf-cost? identity card)
           banned (decks/legal? "banned" card)
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
                :on-mouse-enter #(when (:setname card) (put! zoom-channel line))
                :on-mouse-leave #(put! zoom-channel false)} name]
        (card-influence-html card modqty infaction allied)])
     card)])

(defn line-qty-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span {:key (:code card)} qty "  "])

(defn line-name-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span (if-let [name (:title card)]
           (let [infaction (no-inf-cost? identity card)
                 banned (decks/legal? "banned" card)
                 allied (decks/alliance-is-free? cards line)
                 valid (and (decks/allowed? card identity)
                            (decks/legal-num-copies? identity line))
                 released (decks/released? sets card)
                 modqty (if (decks/is-prof-prog? deck card)
                          (- qty 1)
                          qty)]
             [:span
              [:span {:class (cond
                               (and valid released (not banned)) "fake-link"
                               valid "casual"
                               :else "invalid")
                      :on-mouse-enter #(when (:setname card) (put! zoom-channel line))
                      :on-mouse-leave #(put! zoom-channel false)} name]
              (card-influence-html card modqty infaction allied)])
           card)])

(defn- create-identity
  [s target-value]
  (let [side (get-in @s [:deck :identity :side])
        json-map (.parse js/JSON (.. target-value -target -value))
        id-map (js->clj json-map :keywordize-keys true)]
    (lookup side id-map)))

(defn- identity-option-string
  [card]
  (.stringify js/JSON (clj->js {:title (:title card)
                                :id (:code card)})))

(defn deck-builder
  "Make the deckbuilder view"
  []
  (let [s (r/atom {:edit false
                   :old-deck nil
                   :edit-channel (chan)
                   :deck nil})
        decks (r/cursor app-state [:decks])
        user (r/cursor app-state [:user])
        decks-loaded (r/cursor app-state [:decks-loaded])
        card-sets (r/cursor app-state [:sets])]

    (r/create-class
      {:display-name "deck-builder"

       :component-will-mount
       (fn []
         (let [edit-channel (:edit-channel @s)]
           (go (while true
                 (let [card (<! zoom-channel)]
                   (swap! s assoc :zoom card))))
           (go (while true
                 (let [edit (<! edit-channel)
                       card (:card edit)
                       max-qty (:deck-limit card 3)
                       cards (get-in @s [:deck :cards])
                       match? #(when (= (get-in % [:card :title]) (:title card)) %)
                       existing-line (some match? cards)
                       new-qty (+ (or (:qty existing-line) 0) (:qty edit))
                       remaining (remove match? cards)
                       draft-id (decks/draft-id? (get-in @s [:deck :identity]))
                       new-cards (cond
                                   (and (not draft-id)
                                        (> new-qty max-qty))
                                   (conj remaining (assoc existing-line :qty max-qty))
                                   (<= new-qty 0)
                                   remaining
                                   (empty? existing-line)
                                   (conj remaining {:qty new-qty :card card})
                                   :else
                                   (conj remaining (assoc existing-line :qty new-qty)))]
                   (swap! s assoc-in [:deck :cards] new-cards)
                   (deck->str s)))))
         (go (while true
               (let [deck (<! select-channel)]
                 (end-delete s)
                 (swap! s assoc :deck deck)))))

       :reagent-render
       (fn []
         [:div
          [:div.deckbuilder.blue-shade.panel
           [:div.viewport {:ref #(swap! db-dom assoc :viewport %)}
            [:div.decks
             [:div.button-bar
              (if @user
                (list
                  [:button {:key "corp"
                            :on-click #(new-deck "Corp" s)} "New Corp deck"]
                  [:button {:key "runner"
                            :on-click #(new-deck "Runner" s)} "New Runner deck"])
                (list
                  [:button {:key "corp"
                            :class "disabled"} "New Corp deck"]
                  [:button {:key "runner"
                            :class "disabled"} "New Runner deck"]))]
             [:div.deck-collection
              (when-not (:edit @s)
                [deck-collection {:sets card-sets
                                  :decks decks
                                  :decks-loaded decks-loaded
                                  :active-deck (:deck @s)}])]
             [:div {:class (when (:edit @s) "edit")}
              (when-let [line (:zoom @s)]
                (let [art (:art line)
                      id (:id line)
                      updated-card (add-params-to-card (:card line) id art)]
                  [card-view updated-card s]))]]

            [:div.decklist
             (when-let [deck (:deck @s)]
               (let [identity (:identity deck)
                     cards (:cards deck)
                     edit? (:edit @s)
                     delete? (:delete @s)]
                 [:div
                  (cond
                    edit? [:div.button-bar
                           [:button {:on-click #(save-deck s)} "Save"]
                           [:button {:on-click #(cancel-edit s)} "Cancel"]]
                    delete? [:div.button-bar
                             [:button {:on-click #(handle-delete s)} "Confirm Delete"]
                             [:button {:on-click #(end-delete s)} "Cancel"]]
                    :else [:div.button-bar
                           [:button {:on-click #(edit-deck s)} "Edit"]
                           [:button {:on-click #(delete-deck s)} "Delete"]
                           (when (and (:stats deck)
                                      (not= "none" (get-in @app-state [:options :deckstats])))
                             [:button {:on-click #(clear-deck-stats s)} "Clear Stats"])])
                  [:h3 (:name deck)]
                  [:div.header
                   [:img {:src (image-url identity)
                          :alt (:title identity)}]
                   [:h4 {:class (if (decks/released? @card-sets identity) "fake-link" "casual")
                         :on-mouse-enter #(put! zoom-channel {:card identity
                                                              :art (:art identity)
                                                              :id (:id identity)})
                         :on-mouse-leave #(put! zoom-channel false)}
                    (:title identity)
                    (when (decks/legal? "banned" identity) banned-span)
                    (when (decks/legal? "restricted" identity) restricted-span)
                    (when (:rotated identity) rotated-span)]
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
                      " "
                      (if (pos? inf)
                        (deck-influence-html deck))])
                   (when (= (:side identity) "Corp")
                     (let [min-point (decks/min-agenda-points deck)
                           points (decks/agenda-points deck)]
                       [:div "Agenda points: " points
                        (when (< points min-point)
                          [:span.invalid " (minimum " min-point ")"])
                        (when (> points (inc min-point))
                          [:span.invalid " (maximum " (inc min-point) ")"])]))
                   [:div [deck-status-span deck true true false]]]
                  [:div.cards
                   (doall
                     (for [group (sort-by first (group-by #(get-in % [:card :type]) cards))]
                       ^{:key (or (first group) "Unknown")}
                       [:div.group
                        [:h4 (str (or (first group) "Unknown") " (" (decks/card-count (last group)) ")") ]
                        (doall
                          (for [line (sort-by #(get-in % [:card :title]) (last group))]
                            ^{:key (or (get-in line [:card :code]) line)}
                            [:div.line
                             (if (:edit @s)
                               (let [ch (:edit-channel @s)]
                                 [:span
                                  [:button.small {:on-click #(put! ch {:qty -1 :card (:card line)})
                                                  :type "button"} "-"]
                                  [line-qty-span @card-sets deck line]
                                  [:button.small {:on-click #(put! ch {:qty 1 :card (:card line)})
                                                  :type "button"} "+"]
                                  [line-name-span @card-sets deck line]])
                               [line-span @card-sets deck line])]))]))]]))]
            [:div.deckedit
             [:div
              [:div
               [:h3 "Deck name"]
               [:input.deckname {:type "text"
                                 :placeholder "Deck name"
                                 :ref #(swap! db-dom assoc :deckname %)
                                 :value (get-in @s [:deck :name])
                                 :on-change #(swap! s assoc-in [:deck :name] (.. % -target -value))}]]
              [:div
               [:h3 "Format"]
               [:select.format {:value (get-in @s [:deck :format] "standard")
                                :on-change #(swap! s assoc-in [:deck :format] (.. % -target -value))}
                (for [[k v] slug->format]
                  ^{:key k}
                  [:option {:value k} v])]]
              [:div
               [:h3 "Identity"]
               [:select.identity {:value (identity-option-string (get-in @s [:deck :identity]))
                                  :on-change #(swap! s assoc-in [:deck :identity] (create-identity s %))}
                (let [idents (side-identities (get-in @s [:deck :identity :side]))]
                  (for [card (sort-by :display-name idents)]
                    ^{:key (:display-name card)}
                    [:option
                     {:value (identity-option-string card)}
                     (:display-name card)]))]]
              [card-lookup s]
              [:h3 "Decklist"
               [:span.small "(Type or paste a decklist, it will be parsed)"]]]
             [:textarea {:ref #(swap! db-dom assoc :deckedit %)
                         :value (:deck-edit @s)
                         :on-change #(handle-edit s)}]]]]])})))

(go (let [cards (<! cards-channel)
          decks (process-decks (:json (<! (GET (str "/data/decks")))))]
      (load-decks decks)
      (load-alt-arts)
      (>! cards-channel cards)))
