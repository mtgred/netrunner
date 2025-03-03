(ns game.core.mark
  (:require
   [game.core.engine :refer [trigger-event]]
   [game.core.say :refer [system-msg]]
   [game.core.servers :refer [central->name]]
   [game.core.update :refer [update!]]
   [game.macros :refer [req]]))

(defn set-mark
  [state new-mark]
  (swap! state assoc :mark new-mark)
  (trigger-event state :runner :mark-changed))

(defn is-mark?
  [state s]
  (= s (:mark @state)))

(defn identify-mark
  [state]
  (let [new-mark (rand-nth [:hq :rd :archives])]
    (set-mark state new-mark)
    (system-msg state :runner (str "identifies [their] mark to be " (central->name new-mark)))))

(def identify-mark-ability
  {:effect (req (when (nil? (:mark @state)) (identify-mark state)))})

(def mark-changed-event
  {:event :mark-changed
   :silent (req true)
   :interactive (req false)
   :effect (req (update! state :runner (assoc card :card-target (central->name (:mark @state)))))})
