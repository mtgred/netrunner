(ns game.core.link
  (:require
    [game.core.effects :refer [sum-effects]]))

(defn get-link
  ([state] (get-link state nil))
  ([state _]
   (or (get-in @state [:runner :link])
       (get-in @state [:runner :identity :baselink])
       0)))

(defn- sum-link-effects
  [state id]
  (+ (or (get-in @state [:runner :identity :baselink]) 0)
     (sum-effects state :runner :user-link)
     (sum-effects state :runner :link id)))

(defn update-link
  "Update the runner's link"
  ([state] (update-link state nil))
  ([state _]
   (let [id (get-in @state [:runner :identity])
         old-link (get-link state)
         new-link (sum-link-effects state id)
         changed? (not= old-link new-link)]
     (when changed?
       (swap! state assoc-in [:runner :link] new-link))
     changed?)))

(defn link+
  ([value] (link+ (constantly true) value))
  ([req value]
   {:type :link
    :req req
    :value value}))
