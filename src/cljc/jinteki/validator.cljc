(ns jinteki.validator
  (:require
   [game.core.card :refer [has-subtype?]]
   [jinteki.cards :as cards]
   [clojure.string :as str]
   [jinteki.utils :refer [faction-label INFINITY]]))

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
  "Returns influence limit of an identity or INFINITY in case of special IDs."
  [identity]
  (let [inf (:influencelimit identity)]
    (if (or (nil? inf) (= "∞" inf)) INFINITY inf)))

(defn singleton-id?
  "Returns true if the given id is singleton"
  [identity]
  (or (= "Nova Initiumia: Catalyst & Impetus" (:title identity))
      (= "Ampère: Cybernetics For Anyone" (:title identity))))

(defn singleton-legal
  "Returns false if the identity chosen is a singleton id and the there are multiple copies of the given card, otherwise returns true"
  [identity {:keys [qty card]}]
  (if (singleton-id? identity)
    (= 1 qty)
    true))

(defn legal-num-copies?
  "Returns true if there is a legal number of copies of a particular card."
  [identity {:keys [qty card]}]
  (and (singleton-legal identity {:qty qty :card card})
       (or (draft-id? identity)
           (<= qty (:deck-limit card 3)))))

(defn is-prof-prog?
  "Check if ID is The Professor and card is a Program"
  [deck card]
  (and (= "The Professor: Keeper of Knowledge" (get-in deck [:identity :title]))
       (= "Program" (:type card))))

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
  [{:keys [cards]}]
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

(defn invalid-singleton-agendas
  "returns invalid agendas for a singleton deck"
  [identity cards]
  (when (singleton-id? identity)
    ;; filter into only agendas
    (let [cards (map :card cards)
          relevant-agendas (filter #(and (= (:type %) "Agenda")
                                         (not= (:faction %) "Neutral")) cards)
          factions (distinct (map :faction relevant-agendas))
          relevant-by-faction (fn [fac] (filter #(= fac (:faction %)) relevant-agendas))
          faction-count (map #(relevant-by-faction %) factions)
          offending-cards (into [] cat (filter #(< 2 (count %)) faction-count))]
      (when-not (empty? offending-cards)
        offending-cards))))

(defn singleton-agenda-valid?
  "returns true if the agenda is valid for singleton, or the deck is not singleton"
  [card identity cards]
  (if-not (singleton-id? identity)
    true
    (let [gendies (invalid-singleton-agendas identity cards)]
      (not (some #(= (:title card) (:title %)) gendies)))))

;; Card and deck validity
(defn allowed?
  "Checks if a card is allowed in deck of a given identity - not accounting for influence"
  [card {:keys [side faction title] :as identity}]
  (and (not= (:type card) "Identity")
       (= (:side card) side)
       (or (not= (:type card) "Agenda")
           (= (:faction card) "Neutral")
           (= (:faction card) faction)
           (draft-id? identity)
           (singleton-id? identity))
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
        inf-limit (id-inf-limit identity)
        influence-limit? (<= influence-count inf-limit)
        allowed-cards-fn #(allowed? (:card %) identity)
        legal-num-copies-fn #(legal-num-copies? identity %)
        allowed-cards? (every? allowed-cards-fn cards)
        legal-num-copies? (every? legal-num-copies-fn cards)
        min-agenda-points (min-agenda-points deck)
        invalid-singleton-agendas? (invalid-singleton-agendas identity cards)
        agenda-points (agenda-points deck)
        agenda-points? (or (= (:side identity) "Runner")
                           (<= min-agenda-points agenda-points (inc min-agenda-points)))]
    {:legal (and identity? card-count? influence-limit? allowed-cards? legal-num-copies? agenda-points? (not invalid-singleton-agendas?))
     :reason (cond
               (not identity?) (str "Invalid identity: " (:title identity))
               (not card-count?) (str "Not enough cards in the deck: " card-count ", Min: " min-deck-size)
               (not influence-limit?) (str "Spent too much influence: " influence-count)
               (not allowed-cards?) (str "Cards aren't legal for chosen identity: "
                                         (get-in (some #(when ((complement allowed-cards-fn) %) %) cards) [:card :title]))
               (not legal-num-copies?) (str "Too many copies of a card: "
                                            (get-in (some #(when ((complement legal-num-copies-fn) %) %) cards) [:card :title]))
               invalid-singleton-agendas? (str "Too many agendas from the same factions: " (str/join ", " (map :title invalid-singleton-agendas?)))
               (not agenda-points?) (str "Incorrect amount of agenda points: " agenda-points
                                         ", Between: " min-agenda-points " and " (inc min-agenda-points)))}))

(defn combine-id-and-cards
  [deck]
  (conj (:cards deck) {:qty 1 :card (:identity deck)}))

(defn legal?
  ([status card]
   (legal? :standard status card))
  ([fmt status card]
   (contains? (get-in card [:format fmt]) status)))

(defn legal-line?
  ([status line]
   (legal-line? :standard status (:card line)))
  ([fmt status line]
   (legal? fmt status (:card line))))

(defn filter-cards-by-legal-status
  ([deck status]
   (let [combined-cards (combine-id-and-cards deck)
         fmt (keyword (:format deck))]
     (filter-cards-by-legal-status fmt status combined-cards)))
  ([fmt status cards]
   (filter #(legal-line? fmt status %) cards)))

(defn format-point-limit
  [fmt]
  (get-in @cards/mwl [(keyword fmt) :point-limit]))

(defn deck-point-count
  ([deck]
   (let [combined-cards (combine-id-and-cards deck)
         fmt (keyword (:format deck))
         pointed-cards (filter-cards-by-legal-status fmt :points combined-cards)]
    (deck-point-count fmt pointed-cards)))
  ([fmt cards]
  (reduce (fn [sum line]
            (+ sum (get-in line [:card :format fmt :points])))
          0 cards)))

(defn mwl-legal?
  "Returns true if the deck does not contain banned cards or more than one type of restricted card"
  [fmt cards]
  (let [allowed-cards-fn #(when-let [fmt-legality (get-in % [:card :format fmt])]
                            (some (fn [k] (contains? fmt-legality k)) [:legal :banned]))
        allowed-cards? (every? allowed-cards-fn cards)
        single-restricted-cards (filter-cards-by-legal-status fmt :restricted cards)
        single-restricted? (>= 1 (count single-restricted-cards))
        banned-cards (filter-cards-by-legal-status fmt :banned cards)
        no-banned? (zero? (count banned-cards))
        point-limit (format-point-limit fmt)
        point-cards (filter-cards-by-legal-status fmt :points cards)
        point-total (when point-limit
                      (deck-point-count fmt point-cards))
        under-point-limit? (if point-total
                             (>= point-limit point-total)
                             true)]
    {:legal (and allowed-cards? single-restricted? no-banned? under-point-limit?)
     :reason (cond
               (not allowed-cards?) (str "Illegal card: "
                                         (get-in (some #(when ((complement allowed-cards-fn) %) %) cards) [:card :title]))
               (not single-restricted?) (str "Too many restricted cards: "
                                             (get-in (first single-restricted-cards) [:card :title]))
               (not no-banned?) (str "Includes a banned card: "
                                     (get-in (first banned-cards) [:card :title]))
               (not under-point-limit?) (str "Exceeds point limit: " point-total ", Limit: " point-limit))}))

(defn legal-format?
  [fmt deck]
  (mwl-legal? fmt (combine-id-and-cards deck)))

(defn reject-system-gateway-neutral-ids
  [fmt deck]
  (let [id (:title (:identity deck))]
    (when (and (not= :system-gateway fmt)
               (or (= id "The Catalyst: Convention Breaker")
                   (= id "The Syndicate: Profit over Principle")))
      {:legal false
       :reason (str "Illegal identity: " id)})))

(defn build-format-legality
  [valid fmt deck]
  (let [mwl (legal-format? fmt deck)]
    (or (reject-system-gateway-neutral-ids fmt deck)
        {:legal (and (:legal valid) (:legal mwl))
         :reason (or (:reason valid) (:reason mwl))})))

(defn cards-over-one-sg
  "Returns cards in deck that require more than one copy of System Gateway."
  [cards]
  (letfn [(one-box-num-copies? [{:keys [qty card]}] (<= qty (:quantity card 3)))]
    (remove one-box-num-copies? cards)))

(defn build-system-gateway-legality
  [valid {:keys [cards] :as deck}]
  (let [mwl (legal-format? :system-gateway deck)
        example-card (first (cards-over-one-sg cards))]
    {:legal (and (nil? example-card)
                 (:legal valid)
                 (:legal mwl))
     :reason (or (when example-card
                   (str "Only one copy of System Gateway permitted - check: "
                        (get-in example-card [:card :title])))
                 (:reason valid)
                 (:reason mwl))}))

(defn calculate-deck-status
  "Calculates all the deck's validity for the basic deckbuilding rules,
  as well as various official and unofficial formats. Implement any new formats here."
  [deck]
  (let [valid (valid-deck? deck)]
    {:format (:format deck)
     :casual valid
     :standard (build-format-legality valid :standard deck)
     :startup (build-format-legality valid :startup deck)
     :sunset (build-format-legality valid :sunset deck)
     :system-gateway (build-system-gateway-legality valid deck)
     :eternal (build-format-legality valid :eternal deck)
     :neo (build-format-legality valid :neo deck)
     :snapshot (build-format-legality valid :snapshot deck)
     :snapshot-plus (build-format-legality valid :snapshot-plus deck)}))

(defn trusted-deck-status
  [{:keys [status] :as deck}]
    (if status
      status
      (calculate-deck-status deck)))

(defn singleton-deck-status
  "Calculates if a deck is singleton"
  [deck]
  (let [cards (:cards deck)
        duplicates (filter #(not= 1 (:qty %)) cards)
        is-singleton (zero? (count duplicates))]
    {:singleton is-singleton}))

(defn singleton-deck? [deck]
  (if-let [deck (get-in deck [:status :singleton])]
    deck
    (get-in (singleton-deck-status deck)
            [:singleton]
            false)))

(defn legal-deck?
 ([deck] (legal-deck? deck (:format deck)))
 ([deck fmt]
   (if-let [deck (get-in deck [:status (keyword fmt) :legal])]
     deck
     (get-in (trusted-deck-status (assoc deck :format fmt))
       [(keyword fmt) :legal]
       false))))
