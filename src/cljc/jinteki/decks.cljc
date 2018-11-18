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

;; Card and deck validity
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
  (and (every? #(case (name (get-in % [:card :format fmt]))
                  ("legal" "restricted" "banned")
                  true
                  false)
               cards)
       (>= 1 (count (filter #(legal-line? fmt "restricted" %) cards)))
       (zero? (count (filter #(legal-line? fmt "banned" %) cards)))))

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
  (and (not (nil? identity))
       (>= (card-count cards) (min-deck-size identity))
       (<= (influence-count deck) (id-inf-limit identity))
       (every? #(and (allowed? (:card %) identity)
                     (legal-num-copies? identity %)) cards)
       (or (= (:side identity) "Runner")
           (let [minimum (min-agenda-points deck)]
             (<= minimum (agenda-points deck) (inc minimum))))))

(defn cards-over-one-core
  "Returns cards in deck that require more than single box."
  [cards]
  (letfn [(one-box-num-copies? [{:keys [qty card]}] (<= qty (:quantity card 3)))]
    (remove one-box-num-copies? cards)))

(defn combine-id-and-cards
  [deck]
  (conj (:cards deck) {:qty 1 :card (:identity deck)}))

(defn legal-format?
  [valid fmt deck]
  (and valid
       (mwl-legal? fmt (combine-id-and-cards deck))))

(defn build-format-legality
  [valid fmt deck]
  {:legal (legal-format? valid fmt deck)
   :description (str "Legal for " (-> fmt name s/capitalize))})

(defn build-deck-validity
  [valid]
  {:legal valid
   :description "Basic deckbuilding rules"})

(defn build-core-experience-legality
  [valid {:keys [cards] :as deck}]
  (let [example-card (first (cards-over-one-core cards))]
    {:legal (and (legal-format? valid :core-experience deck)
                 (nil? example-card))
     :reason (when example-card
               (str "Only one System Core 2019 permitted - check: "
                    (get-in example-card [:card :title])))
     :description "Legal for Core Experience"}))

(defn build-socr-legality
  [valid deck]
  (let [big-boxes ["creation-and-control"
                   "honor-and-profit"
                   "order-and-chaos"
                   "data-and-destiny"
                   "terminal-directive"]
        single-set? (as-> deck $
                      (combine-id-and-cards $)
                      (group-by #(get-in % [:card :cycle_code]) $)
                      (select-keys $ big-boxes)
                      (keys $)
                      (>= 1 (count $)))]
    {:legal (and single-set?
                 (legal-format? valid :socr8 deck))
     :description "Legal for Stimhack Online Cache Refresh 8"}))

(defn calculate-deck-status
  "Calculates all the deck's validity for the basic deckbuilding rules, as well as various official and unofficial formats.
  Implement any new formats here."
  [deck]
  (let [valid (valid-deck? deck)]
    {:format (:format deck)
     :casual (build-deck-validity valid)
     :standard (build-format-legality valid :standard deck)
     :eternal (build-format-legality valid :eternal deck)
     :snapshot (build-format-legality valid :snapshot deck)
     :core-experience (build-core-experience-legality valid deck)
     :socr8 (build-socr-legality valid deck)}))

(defn trusted-deck-status
  [{:keys [status] :as deck}]
  status)
