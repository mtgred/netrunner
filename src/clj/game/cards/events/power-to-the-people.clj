(in-ns 'game.core)

(declare run-event)

(def card-events-power-to-the-people
  {"Power to the People"
   {:effect (effect (register-events {:pre-steal-cost
                                      {:once :per-turn :effect (effect (gain :credit 7))
                                                       :msg "gain 7 [Credits]"}
                                      :runner-turn-ends
                                      {:effect (effect (unregister-events card))}}
                    (assoc card :zone '(:discard))))
    :events {:pre-steal-cost nil :runner-turn-ends nil}}})