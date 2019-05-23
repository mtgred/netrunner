(ns game.core.card-properties
  (:require [clojure.string :refer [lower-case]]
            [game.core.card-defs :refer [card-def]]
            [game.utils :refer [to-keyword]]
            [jinteki.utils :as jutils]))

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

(defn when-scored?
  "Checks if the specified card is able to be used for a when-scored text ability"
  [card]
  (not (:not-when-scored (card-def card))))

(defn in-deck?
  "Checks if the specified card is in the draw deck."
  [card]
  (= (:zone card) [:deck]))

(defn card-is?
  "Checks the property of the card to see if it is equal to the given value,
  as either a string or a keyword"
  [card property value]
  (let [cv (get card property)]
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

(defn agenda?
  [card]
  (is-type? card "Agenda"))

(defn asset?
  [card]
  (is-type? card "Asset"))

(defn event?
  [card]
  (is-type? card "Event"))

(defn hardware?
  [card]
  (is-type? card "Hardware"))

(defn ice?
  [card]
  (is-type? card "ICE"))

(defn identity?
  [card]
  (or (is-type? card "Identity")
      (is-type? card "Fake-Identity")))

(defn operation?
  [card]
  (is-type? card "Operation"))

(defn program?
  [card]
  (is-type? card "Program"))

(defn resource?
  [card]
  (is-type? card "Resource"))

(defn upgrade?
  [card]
  (is-type? card "Upgrade"))

; Instead of repeating ourselves, just bring that function into this namespace
(def has-subtype? jutils/has-subtype?)

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

(defn facedown?
  "Checks if the specified card is facedown."
  [card]
  (or (:facedown card)
      (= (:zone card) [:rig :facedown])))

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

(defn active?
  "Checks if the card is active and should receive game events/triggers."
  [{:keys [zone] :as card}]
  (or (identity? card)
      (= zone [:current])
      (and (corp? card)
           (installed? card)
           (rezzed? card))
      (and (runner? card)
           (installed? card)
           (not (facedown? card)))))

(defn get-cid
  "Gets the cid of a given card"
  [card]
  (get-in card [:card :cid]))

(defn private-card
  [card]
  (select-keys card [:zone :cid :side :new :host :counter :advance-counter :hosted :icon]))

(defn can-host?
  "Checks if the specified card is able to host other cards"
  [card]
  (or (not (rezzed? card))
      (not (:cannot-host (card-def card)))))

(defn get-nested-host
  "Recursively searches upward to find the 'root' card of a hosting chain."
  [card]
  (if (:host card)
    (recur (:host card))
    card))

(defn get-nested-zone
  [card]
  (:zone (get-nested-host card)))

(defn assoc-host-zones
  "Associates a new zone onto a card and its host(s)."
  [card]
  (let [card (update-in card [:zone] #(map to-keyword %))]
    (if (:host card)
      (update-in card [:host] assoc-host-zones)
      card)))

(defn can-be-advanced?
  "Returns true if the card can be advanced"
  [card]
  (or (and (agenda? card)
           (installed? card))
      ; e.g. Orion, Contract Killer
      (card-is? card :advanceable :always)
      ; e.g. Tyrant, Woodcutter
      (and (card-is? card :advanceable :while-rezzed)
           (rezzed? card))
      ; e.g. Haas Arcology AI
      (and (card-is? card :advanceable :while-unrezzed)
           (not (rezzed? card)))))
