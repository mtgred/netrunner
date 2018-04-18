(in-ns 'game.core)

(def card-definitions-events-feint
  {"Feint"
   {:req (req hq-runnable)
    :implementation "Bypass is manual"
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    :events {:successful-run {:msg "access 0 cards"
                              :effect (effect (max-access 0))}
             :run-ends {:effect (effect (unregister-events card))}}}})
