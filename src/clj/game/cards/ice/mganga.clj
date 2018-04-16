(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-mganga
  {"Mganga"
   {:subroutines [(do-psi {:label "do 2 net damage"
                           :delayed-completion true
                           :player :corp
                           :effect (req (when-completed (damage state :corp :net 2 {:card card})
                                                        (trash state :corp eid card nil)))}
                          {:label "do 1 net damage"
                           :delayed-completion true
                           :player :corp
                           :effect (req (when-completed (damage state :corp :net 1 {:card card})
                                                        (trash state :corp eid card nil)))})]}})