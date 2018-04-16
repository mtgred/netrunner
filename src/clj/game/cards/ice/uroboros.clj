(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-uroboros
  {"Uroboros"
   {:subroutines [(trace-ability 4 {:label "Prevent the Runner from making another run"
                                    :msg "prevent the Runner from making another run"
                                    :effect (effect (register-turn-flag! card :can-run nil))})

                  (trace-ability 4 end-the-run)]}})
