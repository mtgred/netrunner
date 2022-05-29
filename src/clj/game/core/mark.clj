(ns game.core.mark
  (:require
    [game.core.say :refer [system-msg]]
    [game.core.servers :refer [central->name]]
    [game.macros :refer [req]]))

(defn set-mark
  [state new-mark]
  (swap! state assoc :mark new-mark))

(defn is-mark?
  [state s]
  (= s (:mark @state)))

(defn identify-mark
  [state]
  (let [new-mark (nth '(:hq :rd :archives) (rand-int 3))]
    (set-mark state new-mark)
    (system-msg state :runner (str "identifies their mark to be " (central->name new-mark)))))

(def identify-mark-ability
  {:req (req (nil? (:mark @state)))
   :effect (req (identify-mark state))})
