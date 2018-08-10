(in-ns 'game.cards.ice)

(def card-definition-komainu
  {"Komainu"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand runner)) " subroutines")}]
    :subroutines [(do-net-damage 1)]}})
