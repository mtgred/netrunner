(ns game.core.card
  (:require [clojure.string :refer [lower-case includes?]]))

(defrecord Card
  [advancementcost
   agendapoints
   art
   baselink
   cid
   code
   cost
   cycle_code
   deck-limit
   faction
   format
   image_url
   implementation
   init
   memoryunits
   minimumdecksize
   normalizedtitle
   previous-versions
   quantity
   rotated
   set_code
   side
   strength
   subtype
   title
   trash
   type
   uniqueness])

(defn private-card
  "Returns only the public information of a given card when it's in a private state,
  for example, when it's facedown or in the hand"
  [card]
  (select-keys card [:zone :cid :side :new :host :counter :advance-counter :hosted :icon]))

(defn get-cid
  "Gets the cid of a given card when wrapped in an effect-handler map"
  [card]
  (get-in card [:card :cid]))

(defn in-server?
  "Checks if the specified card is installed in -- and not PROTECTING -- a server"
  [card]
  (= (last (:zone card)) :content))

(defn in-hand?
  "Checks if the specified card is in the hand."
  [card]
  (= (:zone card) [:hand]))

(defn in-discard?
  "Checks if the specified card is in the discard pile."
  [card]
  (= (:zone card) [:discard]))

(defn in-deck?
  "Checks if the specified card is in the draw deck."
  [card]
  (= (:zone card) [:deck]))

(defn in-archives-root?
  [card]
  (= (:zone card) [:servers :archives :content]))

(defn in-hq-root?
  [card]
  (= (:zone card) [:servers :hq :content]))

(defn in-rd-root?
  [card]
  (= (:zone card) [:servers :rd :content]))

(defn in-root?
  [card]
  (or (in-archives-root? card)
      (in-hq-root? card)
      (in-rd-root? card)))

(defn in-play-area?
  "Checks if the specified card is in the play area."
  [card]
  (= (:zone card) [:play-area]))

(defn in-current?
  "Checks if the specified card is in the 'current' zone."
  [card]
  (= (:zone card) [:current]))

(defn in-scored?
  "Checks if the specified card is in _a_ score area (don't know which one)."
  [card]
  (= (:zone card) [:scored]))

(defn- card-is?
  "Checks the property of the card to see if it is equal to the given value,
  as either a string or a keyword"
  [card property value]
  (let [cv (property card)]
    (cond
      (or (keyword? cv)
          (and (string? value)
               (string? cv)))
      (= value cv)
      (and (keyword? value)
           (string? cv))
      (= value (keyword (lower-case cv)))
      :else
      (= value cv))))

(defn runner?
  [card]
  (card-is? card :side "Runner"))

(defn corp?
  [card]
  (card-is? card :side "Corp"))

(defn is-type?
  "Checks if the card is of the specified type, where the type is a string."
  [card type]
  (card-is? card :type type))

(declare facedown?)

(defn agenda?
  [card]
  (is-type? card "Agenda"))

(defn asset?
  [card]
  (is-type? card "Asset"))

(defn event?
  [card]
  (and (not (facedown? card))
       (is-type? card "Event")))

(defn hardware?
  [card]
  (and (not (facedown? card))
       (is-type? card "Hardware")))

(defn ice?
  [card]
  (is-type? card "ICE"))

(defn fake-identity?
  [card]
  (is-type? card "Fake-Identity"))

(defn identity?
  [card]
  (or (is-type? card "Identity")
      (fake-identity? card)))

(defn operation?
  [card]
  (is-type? card "Operation"))

(defn program?
  [card]
  (and (not (facedown? card))
       (is-type? card "Program")))

(defn resource?
  [card]
  (and (not (facedown? card))
       (is-type? card "Resource")))

(defn upgrade?
  [card]
  (is-type? card "Upgrade"))

(defn has-subtype?
  "Checks if the specified subtype is present in the card, ignoring case."
  [card subtype]
  (letfn [(contains-sub? [card]
            (when-let [sub (:subtype card)]
              (includes? (lower-case sub) (lower-case subtype))))]
    (or (contains-sub? card)
        (contains-sub? (:persistent card)))))

(defn virus-program?
  [card]
  (and (program? card)
       (has-subtype? card "Virus")))

(defn corp-installable-type?
  "Is the card of an acceptable type to be installed in a server"
  [card]
  (or (asset? card)
      (agenda? card)
      (ice? card)
      (upgrade? card)))

(defn rezzed?
  [card]
  (:rezzed card))

(defn faceup?
  [card]
  (or (:seen card)
      (rezzed? card)))

(defn installed?
  [card]
  (or (:installed card)
      (= :servers (first (:zone card)))))

(defn facedown?
  "Checks if the specified card is facedown."
  [card]
  (or (= (:zone card) [:rig :facedown])
      (:facedown card)))

(defn active?
  "Checks if the card is active and should receive game events/triggers."
  [card]
  (or (identity? card)
      (in-play-area? card)
      (in-current? card)
      (in-scored? card)
      (and (corp? card)
           (installed? card)
           (rezzed? card))
      (and (runner? card)
           (installed? card)
           (not (facedown? card)))))

(defn can-be-advanced?
  "Returns true if the card can be advanced"
  [card]
  (or (card-is? card :advanceable :always)
      ;; e.g. Tyrant, Woodcutter
      (and (card-is? card :advanceable :while-rezzed)
           (rezzed? card))
      ;; e.g. Haas Arcology AI
      (and (card-is? card :advanceable :while-unrezzed)
           (not (rezzed? card)))
      (and (is-type? card "Agenda")
           (installed? card))))

(defn get-counters
  "Get number of counters of specified type."
  [card counter]
  (cond
    (= counter :advancement)
    (+ (:advance-counter card 0) (:extra-advance-counter card 0))
    (= counter :recurring)
    (:rec-counter card 0)
    :else
    (get-in card [:counter counter] 0)))

(defn get-nested-host
  "Recursively searches upward to find the 'root' card of a hosting chain."
  [card]
  (if (:host card) (recur (:host card)) card))

(defn get-nested-zone
  "Returns the zone of the 'root' card of a hosting chain"
  [card]
  (:zone (get-nested-host card)))

(defn assoc-host-zones
  "Associates a new zone onto a card and its host(s)."
  [card]
  (let [card (update-in card [:zone] #(map keyword %))]
    (if (:host card)
      (update-in card [:host] assoc-host-zones)
      card)))

(declare get-card-hosted)

(defn- to-keyword [string]
  (if (string? string)
    (keyword (lower-case string))
    string))

(defn get-card
  "Returns the most recent copy of the card from the current state, as identified
  by the argument's :zone and :cid."
  [state {:keys [cid zone side host type] :as card}]
  (when card
    (if (= type "Identity")
      (get-in @state [(to-keyword side) :identity])
      (if zone
        (if host
          (get-card-hosted state card)
          (some #(when (= cid (:cid %)) %)
                (let [zones (map to-keyword zone)]
                  (if (= (first zones) :scored)
                    (into (get-in @state [:corp :scored]) (get-in @state [:runner :scored]))
                    (get-in @state (cons (to-keyword side) zones))))))
        card))))

(defn- same-card?
  "Checks if the two cards are the same by :cid. Alternatively specify 1-function to use to check the card"
  ([card1 card2] (same-card? :cid card1 card2))
  ([func card1 card2]
    (= (func card1) (func card2))))

(defn get-card-hosted
  "Finds the current version of the given card by finding its host."
  [state {:keys [cid zone side host] :as card}]
  (let [root-host (get-card state (get-nested-host card))
        helper (fn search [card target]
                 (when-not (nil? card)
                   (if-let [c (some #(when (same-card? % target) %) (:hosted card))]
                     c
                     (some #(when-let [s (search % target)] s) (:hosted card)))))]
    (helper root-host card)))
