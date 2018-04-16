(in-ns 'game.core)

(declare expose-prevent in-server? installed-access-trigger advance-ambush campaign as-trashed-agenda)

(def card-assets-dedicated-response-team
  {"Dedicated Response Team"
   {:events {:successful-run-ends {:req (req tagged)
                                   :msg "do 2 meat damage"
                                   :delayed-completion true
                                   :effect (effect (damage eid :meat 2 {:card card}))}}}})
