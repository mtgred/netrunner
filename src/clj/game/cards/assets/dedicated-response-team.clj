(in-ns 'game.core)

(def card-definitions-assets-dedicated-response-team
  {"Dedicated Response Team"
   {:events {:successful-run-ends {:req (req tagged)
                                   :msg "do 2 meat damage"
                                   :delayed-completion true
                                   :effect (effect (damage eid :meat 2 {:card card}))}}}})
