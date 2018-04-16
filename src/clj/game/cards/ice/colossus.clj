(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-colossus
  {"Colossus"
   {:advanceable :always
    :subroutines [{:label "Give the Runner 1 tag (Give the Runner 2 tags)"
                   :delayed-completion true
                   :msg (msg "give the Runner " (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) "1 tag" "2 tags"))
                   :effect (effect (tag-runner :runner eid (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) 1 2)))}
                  {:label "Trash 1 program (Trash 1 program and 1 resource)"
                   :delayed-completion true
                   :msg (msg "trash 1 program" (when (< 2 (+ (:advance-counter card 0) (:extra-advance-counter card 0))) " and 1 resource"))
                   :effect (req (when-completed (resolve-ability state side trash-program card nil)
                                                (if (> 3 (+ (:advance-counter card 0) (:extra-advance-counter card 0)))
                                                  (effect-completed state side eid)
                                                  (continue-ability state side
                                                    {:prompt "Choose a resource to trash"
                                                     :msg (msg "trash " (:title target))
                                                     :choices {:req #(and (installed? %)
                                                                          (is-type? % "Resource"))}
                                                     :cancel-effect (req (effect-completed state side eid))
                                                     :effect (effect (trash target {:cause :subroutine}))}
                                                   card nil))))}]
    :strength-bonus advance-counters}})
