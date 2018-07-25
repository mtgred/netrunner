(in-ns 'game.cards.events)

(def card-definition-quest-completed
  {"Quest Completed"
   {:req (req (and (some #{:hq} (:successful-run runner-reg))
                   (some #{:rd} (:successful-run runner-reg))
                   (some #{:archives} (:successful-run runner-reg))))
    :choices {:req installed?} :msg (msg "access " (:title target))
    :effect (effect (access-card target))}})
