(in-ns 'game.core)

; Various functions for checking small "flag" values of cards, runs, players, etc.

(defn card-flag?
  "Checks the card to see if it has a :flags entry of the given flag-key with the given value"
  ;TODO: add a register for mutable state card flags, separate from this
  [card flag-key value]
  (let [cdef (card-def card)]
    (= value (get-in cdef [:flags flag-key]))))

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

; Functions for preventing specific game actions.
; TODO: look into migrating these to turn-flags and run-flags.
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


; Small utilities for card properties.
(defn ice? [card]
  (= (:type card) "ICE"))

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
        (turn-flag? state side card :can-rez))))

(defn can-be-advanced?
  "Returns true if the card can be advanced"
  [card]
  (or (card-is? card :advanceable :always)
      (and (card-is? card :advanceable :while-rezzed)
           (rezzed? card))
      (and (card-is? card :type "Agenda")
           (= (first (:zone card)) :servers))))
