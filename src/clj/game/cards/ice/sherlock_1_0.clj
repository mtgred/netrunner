(in-ns 'game.cards.ice)

(def card-definition-sherlock-1-0
  {"Sherlock 1.0"
   {:subroutines [(trace-ability 4 {:choices {:req #(and (installed? %)
                                                         (is-type? % "Program"))}
                                    :label "Add an installed program to the top of the Runner's Stack"
                                    :msg (msg "add " (:title target) " to the top of the Runner's Stack")
                                    :effect (effect (move :runner target :deck {:front true}))})]
    :runner-abilities [(runner-break [:click 1] 1)]}})
