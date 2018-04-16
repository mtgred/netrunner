(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-shinobi
  {"Shinobi"
   {:effect take-bad-pub
    :subroutines [(trace-ability 1 (do-net-damage 1))
                  (trace-ability 2 (do-net-damage 2))
                  (trace-ability 3 {:label "Do 3 net damage and end the run"
                                    :msg "do 3 net damage and end the run"
                                    :effect (effect (damage eid :net 3 {:card card}) (end-run))})]}})
