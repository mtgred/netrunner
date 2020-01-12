(ns jinteki.decks
  (:require [clojure.string :refer [split split-lines join escape] :as s]
            [game.core.card :refer [has-subtype?]]
            [jinteki.utils :refer [faction-label INFINITY]]
            [jinteki.cards :refer [all-cards] :as cards]
            #?@(:clj [[clj-time.core :as t] [clj-time.format :as f]])))

(defn card-count
  [cards]
  (reduce (fn [sum line] (+ sum (:qty line))) 0 cards))


;;; Helpers for Alliance cards
(defn default-alliance-is-free?
  "Default check if an alliance card is free - 6 non-alliance cards of same faction."
  [cards line]
  (<= 6 (card-count (filter #(and (= (get-in line [:card :faction])
                                     (get-in % [:card :faction]))
                                  (not (has-subtype? (:card %) "Alliance")))
                            cards))))

(defn alliance-is-free?
  "Checks if an alliance card is free"
  [cards {:keys [card] :as line}]
  (case (:title card)
    ("Heritage Committee"
     "Product Recall"
     "Jeeves Model Bioroids"
     "Raman Rai"
     "Salem's Hospitality"
     "Executive Search Firm"
     "Consulting Visit"
     "Ibrahim Salem")
    (default-alliance-is-free? cards line)
    "Mumba Temple"
    (>= 15 (card-count (filter #(= "ICE" (:type (:card %))) cards)))
    "Museum of History"
    (<= 50 (card-count cards))
    "PAD Factory"
    (= 3 (card-count (filter #(= "PAD Campaign" (:title (:card %))) cards)))
    "Mumbad Virtual Tour"
    (<= 7 (card-count (filter #(= "Asset" (:type (:card %))) cards)))
    ;; Not an alliance card
    false))

;; Basic deck rules
(defn min-deck-size
  "Contains implementation-specific decksize adjustments, if they need to be different from printed ones."
  [identity]
  (:minimumdecksize identity 0))

(defn min-agenda-points
  [deck]
  (let [size (max (card-count (:cards deck))
                  (min-deck-size (:identity deck)))]
    (+ 2 (* 2 (quot size 5)))))

(defn draft-id?
  "Check if the specified id is a draft identity"
  [identity]
  (= "Draft" (:setname identity)))

(defn id-inf-limit
  "Returns influence limit of an identity or INFINITY in case of draft IDs."
  [identity]
  (if (draft-id? identity)
    INFINITY
    (:influencelimit identity)))

(defn legal-num-copies?
  "Returns true if there is a legal number of copies of a particular card."
  [identity {:keys [qty card]}]
  (or (draft-id? identity)
      (<= qty (:deck-limit card 3))))

(defn is-prof-prog?
  "Check if ID is The Professor and card is a Program"
  [deck card]
  (and (= "The Professor: Keeper of Knowledge" (get-in deck [:identity :title]))
       (= "Program" (:type card))))

(defn before-today? [date]
  #?(:clj  (let [parsed-date (if (string? date)
                               (f/parse (f/formatters :date) date)
                               date)]
             (if (nil? parsed-date)
               false
               (t/before? parsed-date (t/now))))
     :cljs (< date (.toJSON (js/Date.)))))

(defn released?
  "Returns false if the card comes from a spoiled set or is out of competitive rotation."
  [sets card]
  (let [card-set (:setname card)
        rotated (:rotated card)
        date (some #(when (= (:name %) card-set) (:available %)) sets)]
    (and (not rotated)
         (not= date "")
         (not (nil? date))
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
(defn agenda-points
  [{:keys [cards] :as deck}]
  (reduce (fn [acc card]
            (if-let [point (get-in card [:card :agendapoints])]
              (+ acc (* point (:qty card)))
              acc))
          0
          cards))

(defn influence-count
  "Returns sum of influence count used by a deck."
  [deck]
  (apply + (vals (influence-map deck))))

;; Card and deck validity
(defn allowed?
  "Checks if a card is allowed in deck of a given identity - not accounting for influence"
  [card {:keys [side faction title] :as identity}]
  (and (not= (:type card) "Identity")
       (= (:side card) side)
       (or (not= (:type card) "Agenda")
           (= (:faction card) "Neutral")
           (= (:faction card) faction)
           (draft-id? identity))
       (or (not= title "Custom Biotics: Engineered for Success")
           (not= (:faction card) "Jinteki"))))

(defn valid-deck?
  "Checks that a given deck follows deckbuilding rules"
  [{:keys [identity cards] :as deck}]
  (let [identity? (not (nil? identity))
        card-count (card-count cards)
        min-deck-size (min-deck-size identity)
        card-count? (>= card-count min-deck-size)
        influence-count (influence-count deck)
        id-inf-limit (id-inf-limit identity)
        influence-limit? (<= influence-count id-inf-limit)
        allowed-cards-fn #(allowed? (:card %) identity)
        legal-num-copies-fn #(legal-num-copies? identity %)
        allowed-cards? (every? allowed-cards-fn cards)
        legal-num-copies? (every? legal-num-copies-fn cards)
        min-agenda-points (min-agenda-points deck)
        agenda-points (agenda-points deck)
        agenda-points? (or (= (:side identity) "Runner")
                           (<= min-agenda-points agenda-points (inc min-agenda-points)))]
    {:legal (and identity? card-count? influence-limit? allowed-cards? legal-num-copies? agenda-points?)
     :reason (cond
               (not identity?) (str "Invalid identity: " (:title identity))
               (not card-count?) (str "Not enough cards in the deck: " card-count ", Min: " min-deck-size)
               (not influence-limit?) (str "Spent too much influence: " influence-count)
               (not allowed-cards?) (str "Cards aren't legal for chosen identity: "
                                         (get-in (some #(when ((complement allowed-cards-fn) %) %) cards) [:card :title]))
               (not legal-num-copies?) (str "Too many copies of a card: "
                                            (get-in (some #(when ((complement legal-num-copies-fn) %) %) cards) [:card :title]))
               (not agenda-points?) (str "Incorrect amount of agenda points: " agenda-points
                                         ", Between: " min-agenda-points " and " (inc min-agenda-points)))
     :description "Basic deckbuilding rules"}))

(defn legal?
  ([status card]
   (legal? :standard status card))
  ([fmt status card]
   (= status (get-in card [:format fmt]))))

(defn legal-line?
  ([status line]
   (legal-line? :standard status (:card line)))
  ([fmt status line]
   (legal? fmt status (:card line))))

(defn mwl-legal?
  "Returns true if the deck does not contain banned cards or more than one type of restricted card"
  [fmt cards]
  (let [allowed-cards-fn #(when-let [card (get-in % [:card :format fmt])]
                            (case (name card)
                              ("legal" "restricted" "banned") true
                              false))
        allowed-cards? (every? allowed-cards-fn cards)
        single-restricted-fn #(legal-line? fmt "restricted" %)
        single-restricted? (>= 1 (count (filter single-restricted-fn cards)))
        no-banned-fn #(legal-line? fmt "banned" %)
        no-banned? (zero? (count (filter no-banned-fn cards)))]
    {:legal (and allowed-cards? single-restricted? no-banned?)
     :reason (cond
               (not allowed-cards?) (str "Illegal card: "
                                         (get-in (some #(when ((complement allowed-cards-fn) %) %) cards) [:card :title]))
               (not single-restricted?) (str "Too many restricted cards: "
                                             (get-in (some #(when (single-restricted-fn %) %) cards) [:card :title]))
               (not no-banned?) (str "Includes a banned card: "
                                     (get-in (some #(when (no-banned-fn %) %) cards) [:card :title])))}))

(defn combine-id-and-cards
  [deck]
  (conj (:cards deck) {:qty 1 :card (:identity deck)}))

(defn legal-format?
  [fmt deck]
  (mwl-legal? fmt (combine-id-and-cards deck)))

(defn build-format-legality
  [valid fmt deck]
  (let [mwl (legal-format? fmt deck)]
    {:legal (and (:legal valid) (:legal mwl))
     :reason (or (:reason valid) (:reason mwl))
     :description (str "Legal for " (-> fmt name s/capitalize))}))

(defn build-snapshot-plus-legality
  [valid fmt deck]
  (merge (build-format-legality valid fmt deck)
         {:description "Legal for Snapshot Plus"}))

(defn cards-over-one-core
  "Returns cards in deck that require more than single box."
  [cards]
  (letfn [(one-box-num-copies? [{:keys [qty card]}] (<= qty (:quantity card 3)))]
    (remove one-box-num-copies? cards)))

(defn cards-not-in-most-recent-core
  "Returns cards in deck that aren't in latest System Core"
  [cards]
  (remove #(= "system-core-2019" (get-in % [:card :cycle_code])) cards))

(defn build-core-experience-legality
  [valid {:keys [cards] :as deck}]
  (let [mwl (legal-format? :core-experience deck)
        example-card (first (or (cards-not-in-most-recent-core cards)
                                (cards-over-one-core cards)))]
    {:legal (and (nil? example-card)
                 (:legal valid)
                 (:legal mwl))
     :reason (or (when example-card
                   (str "Only one System Core 2019 permitted - check: "
                        (get-in example-card [:card :title])))
                 (:reason valid)
                 (:reason mwl))
     :description "Legal for Core Experience"}))

(defn build-socr-legality
  [valid deck]
  (let [mwl (legal-format? :socr deck)
        big-boxes ["honor-and-profit"
                   "order-and-chaos"
                   "data-and-destiny"
                   "reign-and-reverie"]
        single-set? (as-> deck d
                      (combine-id-and-cards d)
                      (group-by #(get-in % [:card :cycle_code]) d)
                      (select-keys d big-boxes)
                      (keys d)
                      (>= 1 (count d)))]
    {:legal (and single-set?
                 (:legal valid)
                 (:legal mwl))
     :reason (or (when-not single-set? "Cards from too many Big Boxes")
                 (:reason valid)
                 (:reason mwl))
     :description "Legal for Stimhack Online Cache Refresh"}))

(defn calculate-deck-status
  "Calculates all the deck's validity for the basic deckbuilding rules, as well as various official and unofficial formats.
  Implement any new formats here."
  [deck]
  (let [valid (valid-deck? deck)]
    {:format (:format deck)
     :casual valid
     :standard (build-format-legality valid :standard deck)
     :eternal (build-format-legality valid :eternal deck)
     :classic (build-format-legality valid :classic deck)
     :snapshot (build-format-legality valid :snapshot deck)
     :snapshot-plus (build-snapshot-plus-legality valid :snapshot-plus deck)
     :core-experience (build-core-experience-legality valid deck)
     :socr (build-socr-legality valid deck)}))

(defn trusted-deck-status
  [{:keys [status] :as deck}]
  status)
