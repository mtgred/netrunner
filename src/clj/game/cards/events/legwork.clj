(in-ns 'game.core)

(def card-definitions-events-legwork
  {"Legwork"
   {:req (req hq-runnable)
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:silent (req true)
                              :effect (effect (access-bonus 2))}
             :run-ends {:effect (effect (unregister-events card))}}}})
