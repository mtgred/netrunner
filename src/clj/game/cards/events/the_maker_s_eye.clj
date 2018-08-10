(in-ns 'game.cards.events)

(def card-definition-the-maker-s-eye
  {"The Maker's Eye"
   {:req (req rd-runnable)
    :effect (effect (run :rd nil card)
                    (register-events (:events (card-def card)) (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :rd))
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}})
