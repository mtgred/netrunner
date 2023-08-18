(ns game.core.hand-size
  (:require
    [game.core.effects :refer [sum-effects]]))

(defn hand-size
  [state side]
  (or (get-in @state [side :hand-size :total]) 5))

(defn sum-hand-size-effects
  [state side]
  (+ (or (get-in @state [side :hand-size :base]) 5)
     (- (or (get-in @state [side :brain-damage]) 0))
     (sum-effects state side :hand-size)
     (sum-effects state side :user-hand-size)))

(defn update-hand-size
  "Update the player's hand-size"
  [state side]
  (let [old-total (get-in @state [side :hand-size :total])
        new-total (sum-hand-size-effects state side)
        changed? (not= old-total new-total)]
    (when changed?
      (swap! state assoc-in [side :hand-size :total] new-total))
    changed?))

(defn hand-size+
  ([value] (hand-size+ (constantly true) value))
  ([req value]
   {:type :hand-size
    :req req
    :value value}))

(defn corp-hand-size+
  ([value] (corp-hand-size+ (constantly true) value))
  ([req value]
   (hand-size+ (fn [state side eid card targets]
                 (and (= :corp side)
                      (req state side eid card targets)))
               value)))

(defn runner-hand-size+
  ([value] (runner-hand-size+ (constantly true) value))
  ([req value]
   (hand-size+ (fn [state side eid card targets]
                 (and (= :runner side)
                      (req state side eid card targets)))
               value)))
