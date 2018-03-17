(ns jinteki.decks
  (:require [clojure.string :refer [split split-lines join escape] :as s]
            [jinteki.utils :refer [faction-label INFINITY]]
            [jinteki.cards :refer [all-cards] :as cards]
            #?@(:clj [[clj-time.core :as t] [clj-time.format :as f]])))

(defn card-count [cards]
  (reduce #(+ %1 (:qty %2)) 0 cards))


;;; Helpers for Alliance cards
(defn- is-alliance?
  "Checks if the card is an alliance card"
  [card]
  ;; All alliance cards
  (let [ally-cards #{"10013" "10018" "10019" "10029" "10038" "10067" "10068" "10071" "10072" "10076" "10094" "10109"}
        card-code (:code (:card card))]
    (ally-cards card-code)))

(defn- default-alliance-is-free?
  "Default check if an alliance card is free - 6 non-alliance cards of same faction."
  [cards line]
  (<= 6 (card-count (filter #(and (= (get-in line [:card :faction])
                                     (get-in % [:card :faction]))
                                  (not (is-alliance? %)))
                            cards))))

(defn- alliance-is-free?
  "Checks if an alliance card is free"
  [cards {:keys [card] :as line}]
  (case (:code card)
    ("10013"                                               ; Heritage Committee
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


;; Basic deck rules
(defn min-deck-size
  "Contains implementation-specific decksize adjustments, if they need to be different from printed ones."
  [identity]
  (:minimumdecksize identity))

(defn min-agenda-points [deck]
  (let [size (max (card-count (:cards deck)) (min-deck-size (:identity deck)))]
    (+ 2 (* 2 (quot size 5)))))

(defn is-draft-id?
  "Check if the specified id is a draft identity"
  [identity]
  (= "Draft" (:setname identity)))

(defn id-inf-limit
  "Returns influence limit of an identity or INFINITY in case of draft IDs."
  [identity]
  (if (is-draft-id? identity) INFINITY (:influencelimit identity)))

(defn legal-num-copies?
  "Returns true if there is a legal number of copies of a particular card."
  [identity {:keys [qty card]}]
  (or (is-draft-id? identity)
      (<= qty (or (:limited card) 3))))

(defn is-prof-prog?
  "Check if ID is The Professor and card is a Program"
  [deck card]
  (and (= "03029" (get-in deck [:identity :code]))
       (= "Program" (:type card))))

(defn- before-today? [date]
  #?(:clj  (t/before?
             (if (string? date) (f/parse (f/formatters :date) date) date)
             (t/now))
     :cljs (< date (.toJSON (js/Date.)))))

(defn released?
  "Returns false if the card comes from a spoiled set or is out of competitive rotation."
  [sets card]
  (let [card-set (:setname card)
        rotated (:rotated card)
        date (some #(when (= (:name %) card-set) (:available %)) sets)]
    (and (not rotated)
         (not= date "")
         (before-today? date))))

;; Influence
;; Note: line is a map with a :card and a :qty
(defn line-base-cost
  "Returns the basic influence cost of a deck-line"
  [identity-faction {:keys [card qty]}]
  (let [card-faction (:faction card)]
    (if (= identity-faction card-faction)
      0
      (* qty (:factioncost card 0)))))

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




;; Deck attribute calculations
(defn agenda-points [{:keys [cards] :as deck}]
  (reduce #(if-let [point (get-in %2 [:card :agendapoints])]
             (+ (* point (:qty %2)) %1) %1) 0 cards))


(defn influence-count
  "Returns sum of influence count used by a deck."
  [deck]
  (apply + (vals (influence-map deck))))



;; Rotation and MWL
(defn banned-cards
  "Returns a list of card codes that are on the MWL banned list"
  []
  (->> (:cards @cards/mwl)
       (filter (fn [[k v]] (contains? v :deck_limit)))
       (map key)))

(defn banned?
  "Returns true if the card is on the MWL banned list"
  [card]
  (let [banned (banned-cards)]
    (some #(= (keyword (:code card)) %) banned)))

(defn contains-banned-cards
  "Returns true if any of the cards are in the MWL banned list"
  [deck]
  (some #(banned? (:card %)) (:cards deck)))

(defn restricted-cards
  "Returns a list of card codes that are on the MWL restricted list"
  []
  (->> (:cards @cards/mwl)
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
        parse-date #?(:clj  #(f/parse (f/formatters :date) %)
                      :cljs identity)
        cycle-release-date (reduce-kv (fn [result cycle sets-in-cycle]
                                        (assoc result
                                          cycle
                                          (first (sort (mapv #(parse-date (:available %)) sets-in-cycle)))))
                                      {} cycles)
        valid-cycles (map first (take-last 2 (sort-by last (filter (fn [[cycle date]] (before-today? date)) cycle-release-date))))]
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



;; Card and deck validity
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

(defn valid-deck? [{:keys [identity cards] :as deck}]
  ;(prn "valid?" (min-deck-size identity) (card-count cards) deck)
  (and (>= (card-count cards) (min-deck-size identity))
       (<= (influence-count deck) (id-inf-limit identity))
       (every? #(and (allowed? (:card %) identity)
                     (legal-num-copies? identity %)) cards)
       (or (= (:side identity) "Runner")
           (let [min (min-agenda-points deck)]
             (<= min (agenda-points deck) (inc min))))))

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

(defn deck-status
  [mwl-legal valid in-rotation]
  (cond
    (and mwl-legal valid in-rotation) "legal"
    valid "casual"
    :else "invalid"))

(defn calculate-deck-status [deck]
  (let [valid (valid-deck? deck)
        mwl (mwl-legal? deck)
        rotation (only-in-rotation? @cards/sets deck)
        onesies (onesies-legal @cards/sets deck)
        cache-refresh (cache-refresh-legal @cards/sets deck)
        status (deck-status mwl valid rotation)]
    {:valid         valid
     :mwl           mwl
     :rotation      rotation
     :status        status
     :onesies       onesies
     :cache-refresh cache-refresh}))

(defn trusted-deck-status [{:keys [status name cards date] :as deck}]
  (let [parse-date #?(:clj  #(f/parse (f/formatters :date-time) %)
                      :cljs #(js/Date.parse %))
        deck-date (parse-date date)
        mwl-date (:date_start @cards/mwl)]
    (if (and status
             (> deck-date mwl-date))
      status
      (calculate-deck-status deck))))
