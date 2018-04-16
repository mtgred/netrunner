(in-ns 'game.core)

(declare run-event)

(def card-events-quest-completed
  {"Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:req installed?} :msg (msg "access " (:title target))
    :effect (effect (handle-access targets))}})