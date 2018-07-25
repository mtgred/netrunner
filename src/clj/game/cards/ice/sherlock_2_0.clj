(in-ns 'game.cards.ice)

(def card-definition-sherlock-2-0
  {"Sherlock 2.0"
   {:subroutines [(trace-ability 4 {:choices {:req #(and (installed? %)
                                                         (is-type? % "Program"))}
                                    :label "Add an installed program to the bottom of the Runner's Stack"
                                    :msg (msg "add " (:title target) " to the bottom of the Runner's Stack")
                                    :effect (effect (move :runner target :deck))})
                  (give-tags 1)]
    :runner-abilities [(runner-break [:click 2] 2)]}})
