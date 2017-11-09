(ns netrunner.deckbuilder
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as sab :include-macros true]
            [cljs.core.async :refer [chan put! <! timeout] :as async]
            [clojure.string :refer [split split-lines join escape] :as s]
            [netrunner.appstate :refer [app-state]]
            [netrunner.auth :refer [authenticated] :as auth]
            [netrunner.cardbrowser :refer [cards-channel image-url card-view show-alt-art? filter-title] :as cb]
            [netrunner.account :refer [load-alt-arts alt-art-name]]
            [netrunner.ajax :refer [POST GET]]
            [goog.string :as gstring]
            [goog.string.format]))

(def select-channel (chan))
(def zoom-channel (chan))
(def INFINITY 2147483647)

(defn num->percent
  "Converts an input number to a percent of the second input number for display"
  [num1 num2]
  (if (zero? num2)
    "0"
    (gstring/format "%.0f" (* 100 (float (/ num1 num2))))))

(defn identical-cards? [cards]
  (let [name (:title (first cards))]
    (every? #(= (:title %) name) cards)))

(defn is-draft-id?
  "Check if the specified id is a draft identity"
  [identity]
  (= "Draft" (:setname identity)))

(defn is-prof-prog?
  "Check if ID is The Professor and card is a Program"
  [deck card]
  (and (= "03029" (get-in deck [:identity :code]))
       (= "Program" (:type card))))

(defn id-inf-limit
  "Returns influence limit of an identity or INFINITY in case of draft IDs."
  [identity]
  (if (is-draft-id? identity) INFINITY (:influencelimit identity)))

(defn card-count [cards]
  (reduce #(+ %1 (:qty %2)) 0 cards))

(defn noinfcost? [identity card]
  (or (= (:faction card) (:faction identity))
      (= 0 (:factioncost card)) (= INFINITY (id-inf-limit identity))))

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
        cards (filter #(= (:side %) side)
                      (:cards @app-state))
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
  (let [card (lookup side {:title title})]
    (assoc card :art art :display-name (build-identity-name title setname art))))

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
             (not (js/isNaN (js/parseInt qty-str)))
             card-name)
      (let [result (assoc {} :qty (js/parseInt qty-str) :card (s/trim card-name))]
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

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction .toLowerCase (.replace " " "-"))))

(defn allowed?
  "Checks if a card is allowed in deck of a given identity - not accounting for influence"
  [card {:keys [side faction code] :as identity}]
  (and (not= (:type card) "Identity")
       (= (:side card) side)
       (or (not= (:type card) "Agenda")
           (= (:faction card) "Neutral")
           (= (:faction card) faction)
           (is-draft-id? identity))
       (or (not= code "03002") ; Custom Biotics: Engineered for Success
           (not= (:faction card) "Jinteki"))))

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

(defn expand-alts
  [acc card]
  (let [alt-card (get (:alt-arts @app-state) (:code card))
        alt-arts (keys (:alt_art alt-card))]
    (if (and alt-arts
             (show-alt-art?))
    (->> alt-arts
      (concat [""])
      (map (fn [art] (if art
                       (assoc card :art art)
                       card)))
      (map (fn [c] (if (:art c)
                     (assoc c :display-name (str (:display-name c) " [" (alt-art-name (:art c)) "]"))
                     c)))
      (concat acc))
    (conj acc card))))

(defn side-identities [side]
  (let [cards
        (->> (:cards @app-state)
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

;;; Helpers for Alliance cards
(defn is-alliance?
  "Checks if the card is an alliance card"
  [card]
  ;; All alliance cards
  (let [ally-cards #{"10013" "10018" "10019" "10029" "10038" "10067" "10068" "10071" "10072" "10076" "10094" "10109"}
        card-code (:code (:card card))]
    (ally-cards card-code)))

(defn default-alliance-is-free?
  "Default check if an alliance card is free - 6 non-alliance cards of same faction."
  [cards line]
  (<= 6 (card-count (filter #(and (= (get-in line [:card :faction])
                                     (get-in % [:card :faction]))
                                  (not (is-alliance? %)))
                            cards))))

(defn alliance-is-free?
  "Checks if an alliance card is free"
  [cards {:keys [card] :as line}]
  (case (:code card)
    (list
      "10013"                                               ; Heritage Committee
      "10029"                                               ; Product Recall
      "10067"                                               ; Jeeves Model Bioroids
      "10068"                                               ; Raman Rai
      "10071"                                               ; Salem's Hospitality
      "10072"                                               ; Executive Search Firm
      "10094"                                               ; Consulting Visit
      "10109")                                              ; Ibrahim Salem
    (default-alliance-is-free? cards line)
    "10018"                                                 ; Mumba Temple
    (>= 15 (card-count (filter #(= "ICE" (:type (:card %))) cards)))
    "10019"                                                 ; Museum of History
    (<= 50 (card-count cards))
    "10038"                                                 ; PAD Factory
    (= 3 (card-count (filter #(= "PAD Campaign" (:title (:card %))) cards)))
    "10076"                                                 ; Mumbad Virtual Tour
    (<= 7 (card-count (filter #(= "Asset" (:type (:card %))) cards)))
    ;; Not an alliance card
    false))

;;; Influence map helpers
;; Note: line is a map with a :card and a :qty
(defn line-base-cost
  "Returns the basic influence cost of a deck-line"
  [identity-faction {:keys [card qty]}]
  (let [card-faction (:faction card)]
    (if (= identity-faction card-faction)
      0
      (* qty (:factioncost card)))))

(defn line-influence-cost
  "Returns the influence cost of the specified card"
  [deck line]
  (let [identity-faction (get-in deck [:identity :faction])
        base-cost (line-base-cost identity-faction line)]
    ;; Do not care about discounts if the base cost is 0 (in faction or free neutral)
    (if (zero? base-cost)
      0
      (cond
        ;; The Professor: Keeper of Knowledge - discount influence cost of first copy of each program
        (is-prof-prog? deck (:card line))
        (- base-cost (get-in line [:card :factioncost]))
        ;; Check if the card is Alliance and fulfills its requirement
        (alliance-is-free? (:cards deck) line)
        0
        :else
        base-cost))))

(defn influence-map
  "Returns a map of faction keywords to influence values from the faction's cards."
  [deck]
  (letfn [(infhelper [infmap line]
            (let [inf-cost (line-influence-cost deck line)
                  faction (keyword (faction-label (:card line)))]
              (update infmap faction #(+ (or % 0) inf-cost))))]
    (reduce infhelper {} (:cards deck))))

(defn influence-count
  "Returns sum of influence count used by a deck."
  [deck]
  (apply + (vals (influence-map deck))))

(defn min-deck-size
  "Contains implementation-specific decksize adjustments, if they need to be different from printed ones."
  [identity]
  (:minimumdecksize identity))

(defn min-agenda-points [deck]
  (let [size (max (card-count (:cards deck)) (min-deck-size (:identity deck)))]
    (+ 2 (* 2 (quot size 5)))))

(defn agenda-points [{:keys [cards]}]
  (reduce #(if-let [point (get-in %2 [:card :agendapoints])]
             (+ (* point (:qty %2)) %1) %1) 0 cards))

(defn legal-num-copies?
  "Returns true if there is a legal number of copies of a particular card."
  [identity {:keys [qty card]}]
  (or (is-draft-id? identity)
      (<= qty (or (:limited card) 3))))

(defn valid? [{:keys [identity cards] :as deck}]
  (and (>= (card-count cards) (min-deck-size identity))
       (<= (influence-count deck) (id-inf-limit identity))
       (every? #(and (allowed? (:card %) identity)
                     (legal-num-copies? identity %)) cards)
       (or (= (:side identity) "Runner")
           (let [min (min-agenda-points deck)]
             (<= min (agenda-points deck) (inc min))))))

(defn released?
  "Returns false if the card comes from a spoiled set or is out of competitive rotation."
  [sets card]
  (let [card-set (:setname card)
        rotated (:rotated card)
        date (some #(when (= (:name %) card-set) (:available %)) sets)]
    (and (not rotated)
         (not= date "")
         (< date (.toJSON (js/Date.))))))

;; 1.1.1.1 and Cache Refresh validation
(defn group-cards-from-restricted-sets
  "Return map (big boxes and datapacks) of used sets that are restricted by given format"
  [sets allowed-sets deck]
  (let [restricted-cards (remove (fn [card] (some #(= (:setname (:card card)) %) allowed-sets)) (:cards deck))
        restricted-sets (group-by (fn [card] (:setname (:card card))) restricted-cards)
        sorted-restricted-sets (reverse (sort-by #(count (second %)) restricted-sets))
        [restricted-bigboxes restricted-datapacks] (split-with (fn [[setname cards]] (some #(when (= (:name %) setname) (:bigbox %)) sets)) sorted-restricted-sets)]
    { :bigboxes restricted-bigboxes :datapacks restricted-datapacks }))

(defn cards-over-one-core
  "Returns cards in deck that require more than single box."
  [deck]
  (let [one-box-num-copies? (fn [{:keys [qty card]}] (<= qty (or (:packquantity card) 3)))]
    (remove one-box-num-copies? (:cards deck))))

(defn sets-in-two-newest-cycles
  "Returns sets in two newest cycles of released datapacks - for Cache Refresh format"
  [sets]
  (let [cycles (group-by :cycle (remove :bigbox sets))
        cycle-release-date (reduce-kv (fn [result cycle sets-in-cycle] (assoc result cycle (apply min (map :available sets-in-cycle)))) {} cycles)
        valid-cycles (map first (take-last 2 (sort-by last (filter (fn [[cycle date]] (and (not= date "") (< date (.toJSON (js/Date.))))) cycle-release-date))))]
    (map :name (filter (fn [set] (some #(= (:cycle set) %) valid-cycles)) sets))))

(defn cache-refresh-legal
  "Returns true if deck is valid under Cache Refresh rules."
  [sets deck]
  (let [over-one-core (cards-over-one-core deck)
        valid-sets (concat ["Revised Core Set" "Terminal Directive"] (sets-in-two-newest-cycles sets))
        deck-with-id (assoc deck :cards (cons {:card (:identity deck) } (:cards deck))) ;identity should also be from valid sets
        restricted-sets (group-cards-from-restricted-sets sets valid-sets deck-with-id)
        restricted-bigboxes (rest (:bigboxes restricted-sets)) ;one big box is fine
        restricted-datapacks (:datapacks restricted-sets)
        example-card (fn [cardlist] (get-in (first cardlist) [:card :title]))
        reasons {
          :onecore (when (not= (count over-one-core) 0) (str "Only one Core Set permitted - check: " (example-card over-one-core)))
          :bigbox (when (not= (count restricted-bigboxes) 0) (str "Only one Deluxe Expansion permitted - check: " (example-card (second (first restricted-bigboxes)))))
          :datapack (when (not= (count restricted-datapacks) 0) (str "Only two most recent cycles permitted - check: " (example-card (second (first restricted-datapacks)))))
        }]
    { :legal (not-any? val reasons) :reason (join "\n" (filter identity (vals reasons))) }))

(defn onesies-legal
  "Returns true if deck is valid under 1.1.1.1 format rules."
  [sets deck]
  (let [over-one-core (cards-over-one-core deck)
        valid-sets ["Core Set"]
        restricted-sets (group-cards-from-restricted-sets sets valid-sets deck)
        restricted-bigboxes (rest (:bigboxes restricted-sets)) ;one big box is fine
        restricted-datapacks (rest (:datapacks restricted-sets)) ;one datapack is fine
        only-one-offence (>= 1 (apply + (map count [over-one-core restricted-bigboxes restricted-datapacks]))) ;one offence is fine
        example-card (fn [cardlist] (join ", " (map #(get-in % [:card :title]) (take 2 cardlist))))
        reasons (if only-one-offence {} {
          :onecore (when (not= (count over-one-core) 0) (str "Only one Core Set permitted - check: " (example-card over-one-core)))
          :bigbox (when (not= (count restricted-bigboxes) 0) (str "Only one Deluxe Expansion permitted - check: " (example-card (second (first restricted-bigboxes)))))
          :datapack (when (not= (count restricted-datapacks) 0) (str "Only one Datapack permitted - check: " (example-card (second (first restricted-datapacks)))))
        })]
    { :legal (not-any? val reasons) :reason (join "\n" (filter identity (vals reasons))) }))

(defn banned-cards
  "Returns a list of card codes that are on the MWL banned list"
  []
  (->> (:cards (:mwl @app-state))
    (filter (fn [[k v]] (contains? v :deck_limit)))
    (map key)))

(defn banned?
  "Returns true if the card is on the MWL banned list"
  [card]
  (let [banned (banned-cards)]
    (not= -1 (.indexOf banned (keyword (:code card))))))

(defn contains-banned-cards
  "Returns true if any of the cards are in the MWL banned list"
  [deck]
  (some #(banned? (:card %)) (:cards deck)))

(defn restricted-cards
  "Returns a list of card codes that are on the MWL restricted list"
  []
  (->> (:cards (:mwl @app-state))
    (filter (fn [[k v]] (contains? v :is_restricted)))
    (map key)))

(defn restricted?
  "Returns true if the card is on the MWL restricted list"
  [card]
  (let [restricted (restricted-cards)]
    (not= -1 (.indexOf restricted (keyword (:code card))))))

(defn restricted-card-count
  "Returns the number of *types* of restricted cards"
  [deck]
  (->> (:cards deck)
    (filter (fn [c] (restricted? (:card c))))
    (map (fn [c] (:title (:card c))))
    (distinct)
    (count)))

(defn mwl-legal?
  "Returns true if the deck does not contain banned cards or more than one type of restricted card"
  [deck]
  (and (not (contains-banned-cards deck))
       (<= (restricted-card-count deck) 1)))

(defn only-in-rotation?
  "Returns true if the deck doesn't contain any cards outside of current rotation."
  [sets deck]
  (and (every? #(released? sets (:card %)) (:cards deck))
       (released? sets (:identity deck))))

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
       (go (let [response (<! (POST "/data/decks/delete" deck :json))]))
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
       (go (let [new-id (get-in (<! (POST "/data/decks/" data :json)) [:json :_id])
                 new-deck (if (:_id deck) deck (assoc deck :_id new-id))
                 all-decks (process-decks (:json (<! (GET (str "/data/decks")))))]
             (om/update! cursor :decks (conj decks new-deck))
             (om/set-state! owner :deck new-deck)
             (load-decks all-decks)))))))

(defn clear-deck-stats [cursor owner]
  (authenticated
    (fn [user]
      (let [deck (dissoc (om/get-state owner :deck) :stats)
            decks (remove #(= (:_id deck) (:_id %)) (:decks @app-state))
            cards (for [card (:cards deck) :when (get-in card [:card :title])]
                    {:qty (:qty card) :card (get-in card [:card :title])})
            ;; only include keys that are relevant, currently title and side, includes code for future-proofing
            identity (select-keys (:identity deck) [:title :side :code])
            data (assoc deck :cards cards :identity identity)]
        (try (js/ga "send" "event" "deckbuilder" "cleardeckstats") (catch js/Error e))
        (go (let [result (<! (POST "/data/decks/clearstats" data :json))]
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
        banned (banned? card)
        restricted (restricted? card)
        rotated (:rotated card)]
    (list " "
          (when (and (not banned) (not in-faction))
            [:span.influence {:class (faction-label card)}
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
  (dots-html influence-dot (influence-map deck)))

(defn- deck-status
  [mwl-legal valid in-rotation]
  (cond
    (and mwl-legal valid in-rotation) "legal"
    valid "casual"
    :else "invalid"))

(defn deck-status-label
  [sets deck]
  (let [valid (valid? deck)
        mwl (mwl-legal? deck)
        rotation (only-in-rotation? sets deck)]
    (deck-status mwl valid rotation)))

(defn deck-status-span-impl [sets deck tooltip? onesies-details?]
   (let [valid (valid? deck)
         mwl (mwl-legal? deck)
         rotation (only-in-rotation? sets deck)
         status (deck-status mwl valid rotation)
         message (case status
                   "legal" "Tournament legal"
                   "casual" "Casual play only"
                   "invalid" "Invalid")]
     [:span.deck-status.shift-tooltip {:class status} message
      (when tooltip?
        (let [cache-refresh (cache-refresh-legal sets deck)
              onesies (onesies-legal sets deck)]
          [:div.status-tooltip.blue-shade
           [:div {:class (if valid "legal" "invalid")}
            [:span.tick (if valid "✔" "✘")] "Basic deckbuilding rules"]
           [:div {:class (if mwl "legal" "invalid")}
            [:span.tick (if mwl "✔" "✘")] (:name (:mwl @app-state))]
           [:div {:class (if rotation "legal" "invalid")}
            [:span.tick (if rotation "✔" "✘")] "Only released cards"]
           [:div {:class (if (:legal cache-refresh) "legal" "invalid") :title (if onesies-details? (:reason cache-refresh)) }
            [:span.tick (if (:legal cache-refresh) "✔" "✘")] "Cache Refresh compliant"]
           [:div {:class (if (:legal onesies) "legal" "invalid") :title (if onesies-details? (:reason onesies))}
            [:span.tick (if (:legal onesies) "✔" "✘") ] "1.1.1.1 format compliant"]]))]))

(def deck-status-span-memoize (memoize deck-status-span-impl))

(defn deck-status-span
  "Returns a [:span] with standardized message and colors depending on the deck validity."
  ([sets deck] (deck-status-span sets deck false))
  ([sets deck tooltip?] (deck-status-span sets deck tooltip? false))
  ([sets deck tooltip? onesies-details?]
   (deck-status-span-memoize sets deck tooltip? onesies-details?)))

(defn match [identity query]
  (->> (:cards @app-state)
    (filter #(allowed? % identity))
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
  (let [qty (js/parseInt (om/get-state owner :quantity))
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
               matches (match (get-in state [:deck :identity]) query)]
           (when-not (or (empty? query)
                         (= (:title (first matches)) query))
             (om/set-state! owner :matches matches)
             [:div.typeahead
              (for [i (range (count matches))]
                [:div {:class (if (= i (:selected state)) "selected" "")
                       :on-click (fn [e] (-> ".deckedit .qty" js/$ .select)
                                         (om/set-state! owner :query (.. e -target -textContent))
                                         (om/set-state! owner :selected i))}
                 (:title (nth matches i))])]))]]))))

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
                    [:img {:src (image-url (:identity deck))}]
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
                          " - Won: " wins
                          " - Lost: " losses
                          " - Percent Won: " (num->percent wins (+ wins losses)) "%"]))]])])))))

(defn line-span
  "Make the view of a single line in the deck - returns a span"
  [sets {:keys [identity cards] :as deck} {:keys [qty card] :as line}]
  [:span qty " "
   (if-let [name (:title card)]
     (let [infaction (noinfcost? identity card)
           banned (banned? card)
           allied (alliance-is-free? cards line)
           valid (and (allowed? card identity)
                      (legal-num-copies? identity line))
           released (released? sets card)
           modqty (if (is-prof-prog? deck card) (- qty 1) qty)]
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
                      draft-id (is-draft-id? (om/get-state owner [:deck :identity]))
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
                 [:img {:src (image-url identity)}]
                 [:h4 {:class (if (released? (:sets @app-state) identity) "fake-link" "casual")
                       :on-mouse-enter #(put! zoom-channel {:card identity :art (:art identity) :id (:id identity)})
                       :on-mouse-leave #(put! zoom-channel false)}
                  (:title identity)
                  (if (banned? identity)
                    banned-span
                    (when (:rotated identity) rotated-span))]
                 (let [count (card-count cards)
                       min-count (min-deck-size identity)]
                   [:div count " cards"
                    (when (< count min-count)
                      [:span.invalid (str " (minimum " min-count ")")])])
                 (let [inf (influence-count deck)
                       id-limit (id-inf-limit identity)]
                   [:div "Influence: "
                    ;; we don't use valid? and mwl-legal? functions here, since it concerns influence only
                    [:span {:class (if (> inf id-limit) (if (> inf id-limit) "invalid" "casual") "legal")} inf]
                    "/" (if (= INFINITY id-limit) "∞" id-limit)
                    (if (pos? inf)
                      (list " " (deck-influence-html deck)))])
                 (when (= (:side identity) "Corp")
                   (let [min-point (min-agenda-points deck)
                         points (agenda-points deck)]
                     [:div "Agenda points: " points
                      (when (< points min-point)
                        [:span.invalid " (minimum " min-point ")"])
                      (when (> points (inc min-point))
                        [:span.invalid " (maximum " (inc min-point) ")"])]))
                 [:div (deck-status-span sets deck true true)]]
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
                       (line-span sets deck line)])])]]))]

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
