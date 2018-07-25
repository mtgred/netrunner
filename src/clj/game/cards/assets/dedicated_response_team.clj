(in-ns 'game.cards.assets)

(def card-definition-dedicated-response-team
  {"Dedicated Response Team"
   {:events {:successful-run-ends {:req (req tagged)
                                   :msg "do 2 meat damage"
                                   :async true
                                   :effect (effect (damage eid :meat 2 {:card card}))}}}})
