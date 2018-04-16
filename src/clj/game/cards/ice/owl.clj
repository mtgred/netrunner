(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installedrunner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-counter morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-owl
  {"Owl"
   {:subroutines [{:choices {:req #(and (installed? %)
                                        (is-type? % "Program"))}
                   :label "Add installed program to the top of the Runner's Stack"
                   :msg "add an installed program to the top of the Runner's Stack"
                   :effect (effect (move :runner target :deck {:front true})
                                   (system-msg (str "adds " (:title target) " to the top of the Runner's Stack")))}]}})