(ns game.core.link
  (:require
    [game.core.effects :refer [sum-effects]]
    [game.core.engine :refer [trigger-event]]))

(defn get-link
  ([state] (get-link state nil))
  ([state _]
   (or (get-in @state [:runner :link])
       (get-in @state [:runner :identity :baselink])
       0)))

(defn- sum-link-effects
  [state id]
  (+ (or (get-in @state [:runner :identity :baselink]) 0)
     (sum-effects state :runner nil :user-link nil)
     (sum-effects state :runner id :link nil)))

(defn update-link
  "Update the runner's link"
  ([state] (update-link state nil))
  ([state _]
   (let [id (get-in @state [:runner :identity])
         old-link (get-link state :runner)
         new-link (sum-link-effects state id)]
     (swap! state assoc-in [:runner :link] new-link))))
