(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-ichi-10
  {"Ichi 1.0"
   {:subroutines [trash-program
                  (trace-ability 1 {:label "Give the Runner 1 tag and do 1 brain damage"
                                    :msg "give the Runner 1 tag and do 1 brain damage"
                                    :delayed-completion true
                                    :effect (req (when-completed (damage state :runner :brain 1 {:card card})
                                                                 (tag-runner state :runner eid 1)))})]
    :runner-abilities [(runner-break [:click 1] 1)]}})