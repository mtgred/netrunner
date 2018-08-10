(in-ns 'game.cards.ice)

(def card-definition-sync-bre
  {"SYNC BRE"
   {:subroutines [(tag-trace 4)
                  (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                    :async true
                                    :msg "reduce cards accessed for this run by 1"
                                    :effect (effect (access-bonus -1))})]}})
