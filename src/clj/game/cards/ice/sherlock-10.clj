(in-ns 'game.core)

(def card-definitions-ice-sherlock-10
  {"Sherlock 1.0"
   {:subroutines [{:label "Trace 4 - Add an installed program to the top of the Runner's Stack"
                   :trace {:base 4
                           :choices {:req #(and (installed? %)
                                                (is-type? % "Program"))}
                           :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                           :effect (effect (move :runner target :deck {:front true}))}}]
    :runner-abilities [(runner-break [:click 1] 1)]}})
