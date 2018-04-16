(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-holmegaard
  {"Holmegaard"
   {:subroutines [(trace-ability 4 {:label "Runner cannot access any cards this run"
                                    :msg "stop the Runner from accessing any cards this run"
                                    :effect (req (max-access state side 0)
                                                 (swap! state update-in [:run :run-effect] dissoc :replace-access))})
                  {:label "Trash an icebreaker"
                   :prompt "Choose an icebreaker to trash"
                   :msg (msg "trash " (:title target))
                   :choices {:req #(and (installed? %)
                                        (has? % :subtype "Icebreaker"))}
                   :effect (effect (trash target {:cause :subroutine})
                                   (clear-wait-prompt :runner))}]}})