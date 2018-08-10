(in-ns 'game.cards.operations)

(def card-definition-load-testing
  {"Load Testing"
   {:msg "make the Runner lose [Click] when their next turn begins"
    :effect (effect (register-events (:events (card-def card))
                                     (assoc card :zone '(:discard))))
    :events {:runner-turn-begins {:msg "make the Runner lose [Click]"
                                  :effect (effect (lose :runner :click 1)
                                                  (unregister-events card))}}}})
