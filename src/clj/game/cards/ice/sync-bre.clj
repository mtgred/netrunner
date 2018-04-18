(in-ns 'game.core)

(def card-definitions-ice-sync-bre
  {"SYNC BRE"
   {:subroutines [(trace-ability 4 give-tag)
                  (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                    :delayed-completion true
                                    :msg "reduce cards accessed for this run by 1"
                                    :effect (effect (access-bonus -1))})]}})
