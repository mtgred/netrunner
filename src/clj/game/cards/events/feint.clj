(in-ns 'game.cards.events)

(def card-definition-feint
  {"Feint"
   {:req (req hq-runnable)
    :implementation "Bypass is manual"
    :effect (effect (run :hq nil card) (register-events (:events (card-def card))
                                                        (assoc card :zone '(:discard))))
    ;; Don't need a msg since game will print that card access is prevented
    :events {:successful-run {:effect (effect (prevent-access))}
             :run-ends {:effect (effect (unregister-events card))}}}})
