(ns game.core.shuffling
  (:require
    [game.core.events :refer [trigger-event]]
    [game.core.moving :refer [move-zone]]))

(defn shuffle!
  "Shuffles the vector in @state [side kw]."
  [state side kw]
  (when (contains? #{:deck :hand :discard} kw)
    (trigger-event state side (when (= :deck kw) (if (= :corp side) :corp-shuffle-deck :runner-shuffle-deck)) nil)
    (when (and (:access @state)
               (= :deck kw))
      (swap! state assoc-in [:run :shuffled-during-access :rd] true))
    (swap! state update-in [side kw] shuffle)))

(defn shuffle-into-deck
  [state side & args]
  (doseq [zone (filter keyword? args)]
    (move-zone state side zone :deck))
  (shuffle! state side :deck))
