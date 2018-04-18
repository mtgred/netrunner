(in-ns 'game.core)

(def card-definitions-ice-holmegaard
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
