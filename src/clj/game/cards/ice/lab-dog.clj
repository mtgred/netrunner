(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-lab-dog
  {"Lab Dog"
   {:subroutines [(assoc trash-hardware :label "Force the Runner to trash an installed piece of hardware"
                                        :player :runner
                                        :msg (msg "force the Runner to trash " (:title target))
                                        :effect (req (trash state side target)
                                                     (when current-ice
                                                       (no-action state side nil)
                                                       (continue state side nil))
                                                     (trash state side card)))]}})