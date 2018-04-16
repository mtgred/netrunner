(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-flare
  {"Flare"
   {:subroutines [(trace-ability 6 {:label "Trash 1 hardware, do 2 meat damage, and end the run"
                                    :msg "trash 1 hardware, do 2 meat damage, and end the run"
                                    :delayed-completion true
                                    :effect (effect (continue-ability
                                                     {:prompt "Select a piece of hardware to trash"
                                                      :label "Trash a piece of hardware"
                                                      :choices {:req #(is-type? % "Hardware")}
                                                      :msg (msg "trash " (:title target))
                                                      :effect (req (when-completed
                                                                     (trash state side target {:cause :subroutine})
                                                                     (do (damage state side eid :meat 2 {:unpreventable true
                                                                                              :card card})
                                                                         (end-run state side))))
                                                      :cancel-effect (effect (damage eid :meat 2 {:unpreventable true :card card})
                                                                             (end-run))}
                                                     card nil))})]}})
