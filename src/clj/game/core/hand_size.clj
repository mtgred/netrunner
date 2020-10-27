(ns game.core.hand-size
  (:require
    [game.core.effects :refer [get-effects sum-effects]]
    [game.core.engine :refer [trigger-event]]))

(defn hand-size
  [state side]
  (or (get-in @state [side :hand-size :total]) 5))

(defn sum-hand-size-effects
  [state side]
  (+ (or (get-in @state [side :hand-size :base]) 5)
     (- (or (get-in @state [side :brain-damage]) 0))
     (sum-effects state side nil :hand-size)
     (sum-effects state side nil :user-hand-size)))

(defn update-hand-size
  "Update the player's hand-size"
  [state side]
  (let [old-total (get-in @state [side :hand-size :total])
        new-total (sum-hand-size-effects state side)]
    (swap! state assoc-in [side :hand-size :total] new-total)))
