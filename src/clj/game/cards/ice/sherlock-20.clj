(in-ns 'game.core)

(def card-definitions-ice-sherlock-20
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
