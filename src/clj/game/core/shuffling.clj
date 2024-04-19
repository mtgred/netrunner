(ns game.core.shuffling
  (:require
    [game.core.card :refer [corp? in-discard?]]
    [game.core.engine :refer [trigger-event]]
    [game.core.moving :refer [move move-zone]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [continue-ability msg req]]
    [game.utils :refer [enumerate-str quantify]]))

(defn shuffle!
  "Shuffles the vector in @state [side kw]."
  [state side kw]
  (when (contains? #{:deck :hand :discard} kw)
    (trigger-event state side (when (= :deck kw) (if (= :corp side) :corp-shuffle-deck :runner-shuffle-deck)))
    (when (and (:access @state)
               (:run @state)
               (= :corp side)
               (= :deck kw))
      (swap! state assoc-in [:run :shuffled-during-access :rd] true))
    (swap! state update-in [side kw] shuffle)))

(defn shuffle-into-deck
  [state side & args]
  (doseq [zone (filter keyword? args)]
    (move-zone state side zone :deck))
  (shuffle! state side :deck))

(defn shuffle-into-rd-effect
  ([state side eid card n] (shuffle-into-rd-effect state side eid card n false))
  ([state side eid card n all?]
   (continue-ability
     state side
     {:show-discard  true
      :choices {:max (min (-> @state :corp :discard count) n)
                :card #(and (corp? %)
                            (in-discard? %))
                :all all?}
      :msg (msg "shuffle "
                (let [seen (filter :seen targets)
                      m (count (filter #(not (:seen %)) targets))]
                  (str (enumerate-str (map :title seen))
                       (when (pos? m)
                         (str (when-not (empty? seen) " and ")
                              (quantify m "unseen card")))))
                " into R&D")
      :effect (req (doseq [c targets]
                     (move state side c :deck))
                   (shuffle! state side :deck))
      :cancel-effect (req 
                      (system-msg state side (str " uses " (:title card) " to shuffle their deck")) 
                      (shuffle! state side :deck))}
     card nil)))

(defn shuffle-deck
  "Shuffle R&D/Stack."
  [state side {:keys [close]}]
  (swap! state update-in [side :deck] shuffle)
  (if close
    (do
      (swap! state update-in [side] dissoc :view-deck)
      (system-msg state side "stops looking at their deck and shuffles it"))
    (system-msg state side "shuffles their deck")))
