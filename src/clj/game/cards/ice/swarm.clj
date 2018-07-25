(in-ns 'game.cards.ice)

(def card-definition-swarm
  {"Swarm"
   {:effect take-bad-pub
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (get-counters card :advancement) " subroutines")}]
    :subroutines [trash-program]
    :runner-abilities [(runner-pay [:credit 3] 1)]}})
