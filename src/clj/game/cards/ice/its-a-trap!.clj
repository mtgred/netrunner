(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-its-a-trap!
  {"Its a Trap!"
   {:expose {:msg "do 2 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 2 {:card card}))}
    :subroutines [(assoc trash-installed :effect (req (trash state side target {:cause :subroutine})
                                                      (when current-ice
                                                        (no-action state side nil)
                                                        (continue state side nil))
                                                      (trash state side card)))]}})