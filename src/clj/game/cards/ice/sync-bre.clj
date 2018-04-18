(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-definitions-ice-sync-bre
  {"SYNC BRE"
   {:subroutines [(trace-ability 4 give-tag)
                  (trace-ability 2 {:label "Runner reduces cards accessed by 1 for this run"
                                    :delayed-completion true
                                    :msg "reduce cards accessed for this run by 1"
                                    :effect (effect (access-bonus -1))})]}})
