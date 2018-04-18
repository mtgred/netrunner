(in-ns 'game.core)

(def card-definitions-ice-swarm
  {"Swarm"
   {:effect take-bad-pub
    :advanceable :always
    :abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (:advance-counter card 0) " subroutines")}]
    :subroutines [trash-program]
    :runner-abilities [(runner-break [:credit 3] 1)]}})
