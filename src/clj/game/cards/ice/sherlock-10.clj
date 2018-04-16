(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-sherlock-10
  {"Sherlock 1.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the top of the Runner's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                           :effect (effect (move :runner target :deck {:front true}))}}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
