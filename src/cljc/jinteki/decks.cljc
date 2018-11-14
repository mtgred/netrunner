(ns jinteki.decks
  (:require [clojure.string :refer [split split-lines join escape] :as s]
            [jinteki.utils :refer [faction-label INFINITY has-subtype?]]
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
                                  (not (has-subtype? % "Alliance")))
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
  (:minimumdecksize identity))

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

;; alternative formats validation
(defn group-cards-from-restricted-sets
  "Return map (big boxes and datapacks) of used sets that are restricted by given format"
  [sets allowed-sets deck]
  (let [restricted-cards (remove (fn [card] (some #(= (:setname (:card card)) %) allowed-sets)) (:cards deck))
        restricted-sets (group-by (fn [card] (:setname (:card card))) restricted-cards)
        sorted-restricted-sets (reverse (sort-by #(count (second %)) restricted-sets))
        [restricted-bigboxes restricted-datapacks] (split-with (fn [[setname _]]
                                                                 (some #(when (= (:name %) setname)
                                                                          (:bigbox %)) sets))
                                                               sorted-restricted-sets)]
    {:bigboxes restricted-bigboxes
     :datapacks restricted-datapacks}))

(defn cards-over-one-core
  "Returns cards in deck that require more than single box."
  [cards]
  (letfn [(one-box-num-copies? [{:keys [qty card]}] (<= qty (:quantity card 3)))]
    (remove one-box-num-copies? cards)))

(defn get-newest-cycles
  "Returns n cycles of data packs from newest backwards"
  [sets n]
  (let [cycles (group-by :cycle (remove :bigbox sets))
        parse-date #?(:clj  #(f/parse (f/formatters :date) %)
                      :cljs identity)
        cycle-release-date (reduce-kv (fn [result cycle sets-in-cycle]
                                        (assoc result
                                          cycle
                                          (first (sort (mapv #(parse-date (:available %)) sets-in-cycle)))))
                                      {} cycles)
        valid-cycles (map first (take-last n (sort-by last (filter (fn [[cycle date]] (before-today? date)) cycle-release-date))))]
    valid-cycles))

(defn sets-in-newest-cycles
  "Returns sets in the n cycles of released datapacks"
  [sets n]
  (map :name (filter (fn [set] (some #(= (:cycle set) %) (get-newest-cycles sets n))) sets)))

(defn cache-refresh-legal
  "Returns true if deck is valid under Cache Refresh rules. http://www.cache-refresh.info/"
  ([sets deck] (cache-refresh-legal sets deck (concat ["Terminal Directive"] (sets-in-newest-cycles sets 2)) "Cache Refresh compliant"))
  ([sets deck valid-sets description]
   (let [over-one-core (cards-over-one-core (:cards deck))
         valid-sets (concat ["Revised Core Set"] valid-sets)
         deck-with-id (assoc deck :cards (cons {:card (:identity deck)} (:cards deck))) ; identity should also be from valid sets
         restricted-sets (group-cards-from-restricted-sets sets valid-sets deck-with-id)
         restricted-bigboxes (rest (:bigboxes restricted-sets)) ; one big box is fine
         restricted-datapacks (:datapacks restricted-sets)
         example-card (fn [cardlist] (get-in (first cardlist) [:card :title]))
         reasons {:onecore (when (not= (count over-one-core) 0)
                             (str "Only one Revised Core Set permitted - check: " (example-card over-one-core)))
                  :bigbox (when (not= (count restricted-bigboxes) 0)
                            (str "Only one Deluxe Expansion permitted - check: " (example-card (second (first restricted-bigboxes)))))
                  :datapack (when (not= (count restricted-datapacks) 0)
                              (str "Only latest 2 cycles are permitted - check: " (example-card (second (first restricted-datapacks)))))}]
     {:legal (not-any? val reasons)
      :reason (join "\n" (filter identity (vals reasons)))
      :description description})))

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

(defn check-deck-status
  "Checks the valid and standard keys of a deck-status map to check if the deck is legal, casual or invalid."
  [{:keys [valid standard eternal core-experience snapshot]}]
  (if valid
    (cond
      (:legal core-experience) "core-experience"
      (:legal standard) "standard"
      (:legal snapshot) "snapshot"
      (:legal eternal) "eternal"
      :else "casual")
    "invalid"))

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
  (and (every? #(case (get-in % [:card :format fmt])
                  ("legal" "restricted" "banned")
                  true
                  false)
               cards)
       (>= 1 (count (filter #(legal-line? fmt "restricted" %) cards)))
       (zero? (count (filter #(legal-line? fmt "banned" %) cards)))))

(defn legal-format?
  [valid fmt deck]
  {:legal (and valid
               (mwl-legal? fmt (conj (:cards deck) {:qty 1 :card (:identity deck)}))
   :description (str "Legal for " (-> fmt name s/capitalize))})

(defn valid-deck?
  "Checks that a given deck follows deckbuilding rules"
  [{:keys [identity cards] :as deck}]
  (and (not (nil? identity))
       (>= (card-count cards) (min-deck-size identity))
       (<= (influence-count deck) (id-inf-limit identity))
       (every? #(and (allowed? (:card %) identity)
                     (legal-num-copies? identity %)) cards)
       (or (= (:side identity) "Runner")
           (let [minimum (min-agenda-points deck)]
             (<= minimum (agenda-points deck) (inc minimum))))))

(defn core-experience?
  [valid {:keys [identity cards]}]
  (let [over-one-core (cards-over-one-core cards)
        example-card (first over-one-core)]
    {:legal (and valid
                 (mwl-legal? :core-experience cards)
                 (mwl-legal? :core-experience [identity])
                 (nil? example-card))
     :reason (when example-card
               (str "Only one System Core 2019 permitted - check: "
                    (get-in example-card [:card :title])))
     :description "Legal for Core Experience"}))

(defn socr?
  [valid {:keys [identity cards]}]
  (every? #(case (get-in % [:card :format :socr8])
                    ("legal" "restricted" "banned")
                    true
                    false)
                 cards)
                 (mwl-legal? fmt (:cards deck))
  (let [big-boxes ["creation-and-control"
                   "honor-and-profit"
                   "order-and-chaos"
                   "data-and-destiny"
                   "terminal-directive"]
        remove-f (fn [card] (some #(= (:cycle_code (:card card)) %) big-boxes))
        restricted-cards (remove remove-f (:cards deck))
        ]
    {:legal (and valid)
     :reason ""
     :description "Legal for Stimhack Online Cache Refresh 8"
     }))

(defn calculate-deck-status
  "Calculates all the deck's validity for the basic deckbuilding rules, as well as various official and unofficial formats.
  Implement any new formats here."
  [deck]
  (let [sets @cards/sets
        valid (valid-deck? deck)
        socr8 (cache-refresh-legal sets deck
                                   (concat ["Terminal Directive" "Reign and Reverie"]
                                           (sets-in-newest-cycles sets 1))
                                   "Stimhack Online Cache Refresh 8")]
    {:standard (legal-format? valid :standard deck)
     :eternal (legal-format? valid :eternal deck)
     :snapshot (legal-format? valid :snapshot deck)
     :core-experience (core-experience? valid deck)
     :socr8 (socr? valid deck)}))

(defn trusted-deck-status
  [{:keys [status date] :as deck}]
  (let [parse-date #?(:clj  #(f/parse (f/formatters :date-time) %)
                      :cljs #(js/Date.parse %))
        deck-date (parse-date date)
        mwl-date (:date-start @cards/mwl)]
    (if (and status
             (> deck-date mwl-date))
      status
      (calculate-deck-status deck))))
