(in-ns 'game.core)

;;;; Various functions for checking small "flag" values of cards, runs, players, etc.

(defn card-flag?
  "Checks the card to see if it has a :flags entry of the given flag-key with the given value"
  ;TODO: add a register for mutable state card flags, separate from this
  [card flag-key value]
  (let [cdef (card-def card)]
    (= value (get-in cdef [:flags flag-key]))))

(defn card-flag-fn?
  "Checks the card to see if it has a :flags entry of the given flag-key, whose value is a four-argument
  function that returns the given value"
  [state side card flag-key value]
  (let [cdef (card-def card)
        func (get-in cdef [:flags flag-key])]
    (and func (= (func state side card nil) value))))

(defn is-tagged?
  "Returns true if the runner is tagged."
  [state]
  (or (pos? (get-in state [:runner :tag]))
      (pos? (get-in state [:runner :tagged]))))

(defn register-run-flag! [state side card flag condition]
  "Registers a flag for the current run only. The flag gets cleared in end-run.
  Example: Blackmail flags the inability to rez ice."
  (let [stack (get-in @state [:stack :current-run flag])]
    (swap! state assoc-in [:stack :current-run flag] (conj stack {:card card :condition condition}))))

(defn run-flag? [state side card flag]
  "Execute all conditions for the given run flag
  The resulting collection is expected to be empty if nothing is blocking the action
  If the collection has any contents, the flag is considered to be false
  (consider it as something has flagged the action as not being allowed)"
  (empty?
    (for [condition (get-in @state [:stack :current-run flag])
          :let [result ((:condition condition) state side card)]
          :when (not result)]
      [result])))

(defn clear-run-register!
  "Clears the run-flag register."
  [state]
  (swap! state assoc-in [:stack :current-run] nil))

(defn register-turn-flag!
  "As register-run-flag, but for the entire turn."
  [state side card flag condition]
  (let [stack (get-in @state [:stack :current-turn flag])]
    (swap! state assoc-in [:stack :current-turn flag] (conj stack {:card card :condition condition}))))

(defn turn-flag? [state side card flag]
  (empty?
    (for [condition (get-in @state [:stack :current-turn flag])
          :let [result ((:condition condition) state side card)]
          :when (not result)]
      [result])))

(defn clear-turn-register! [state]
  (swap! state assoc-in [:stack :current-turn] nil))

(defn register-persistent-flag!
  "A flag that persists until cleared."
  [state side card flag condition]
  (let [stack (get-in @state [:stack :persistent flag])]
    (swap! state assoc-in [:stack :persistent flag] (conj stack {:card card :condition condition}))))

(defn persistent-flag?
  "Check if any conditions for the flag evaluate to true for the given card."
  [state side card flag]
  (some true?
        (for [condition (get-in @state [:stack :persistent flag])
              :let [result ((:condition condition) state side card)]]
          result)))

(defn clear-persistent-flag!
  "Remove any entry associated with card for the given flag"
  [state side card flag]
  (swap! state update-in [:stack :persistent flag]
         #(remove (fn [map] (= (:cid (map :card)) (:cid %2))) %1) card))

;;; Functions for preventing specific game actions.
;;; TODO: look into migrating these to turn-flags and run-flags.
(defn prevent-run [state side]
  (swap! state assoc-in [:runner :register :cannot-run] true))

(defn prevent-draw [state side]
  (swap! state assoc-in [:runner :register :cannot-draw] true))

(defn prevent-jack-out [state side]
  (swap! state assoc-in [:run :cannot-jack-out] true))

(defn prevent-steal [state side]
  (swap! state assoc-in [:runner :register :cannot-steal] true))

(defn prevent-current [state side]
  (swap! state assoc-in [:runner :register :cannot-play-current] true))

(defn lock-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(conj % cid)))

(defn release-zone [state side cid tside tzone]
  (swap! state update-in [tside :locked tzone] #(remove #{cid} %)))


;;; Small utilities for card properties.
(defn in-server?
  "Checks if the specified card is installed in -- and not PROTECTING -- a server"
  [card]
  (= (last (:zone card)) :content))

(defn in-hand?
  "Checks if the specified card is in the hand."
  [card]
  (= (:zone card) [:hand]))

(defn is-scored?
  "Checks if the specified card is in the scored area of the specified player."
  [state side card]
  (some #(= (:cid %) (:cid card)) (get-in @state [side :scored])))

(defn in-deck?
  "Checks if the specified card is in the draw deck."
  [card]
  (= (:zone card) [:deck]))

(defn in-corp-scored?
  "Checks if the specified card is in the Corp score area."
  [state side card]
  (not (empty? (filter #(= (:cid card) (:cid %)) (get-in @state [:corp :scored])))))

(defn in-runner-scored?
  "Checks if the specified card is in the Runner score area."
  [state side card]
  (not (empty? (filter #(= (:cid card) (:cid %)) (get-in @state [:runner :scored])))))

(defn is-type?
  "Checks if the card is of the specified type, where the type is a string."
  [card type]
  (card-is? card :type type))

(defn has-subtype?
  "Checks if the specified subtype is present in the card.
  Mostly sugar for the has? function."
  [card subtype]
  (has? card :subtype subtype))

(defn ice? [card]
  (is-type? card "ICE"))

(defn rezzed? [card]
  (:rezzed card))

(defn installed? [card]
  (or (:installed card) (= :servers (first (:zone card)))))

(defn untrashable-while-rezzed? [card]
  (and (card-flag? card :untrashable-while-rezzed true) (rezzed? card)))

(defn can-rez?
  ([state side card] (can-rez? state side card nil))
  ([state side card {:as args}]
   (and (run-flag? state side card :can-rez)
        (turn-flag? state side card :can-rez)
        (if-let [rez-req (:rez-req (card-def card))]
          (rez-req state side card nil)
          true))))

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
