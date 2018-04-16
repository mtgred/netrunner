(in-ns 'game.core)

(declare trash-program trash-hardware trash-resource-sub trash-installed runner-break end-the-run end-the-run-if-tagged
         give-tag add-power-counter trace-ability tag-trace do-net-damage do-brain-damage gain-credits
         power-counter-ability do-psi take-bad-pub runner-loses-click advance-counters space-ice-rez-bonus space-ice
         grail-in-hand reveal-grail resolve-grail grail-ice next-ice-count morph morph-effect morph-ice
         constellation-ice implementation-note)

(def card-ice-sherlock-20
  {"Sherlock 2.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the bottom of the Runner's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg     (msg "add " (:title target) " to the bottom of the Runner's Stack")
                           :effect  (effect (move :runner target :deck))}}
                  {:label  "Give the Runner 1 tag"
                   :msg    "give the Runner 1 tag"
                   :delayed-completion true
                   :effect (effect (tag-runner :runner eid 1))}]
    :runner-abilities [(runner-break [:click 2] 2)]}})
