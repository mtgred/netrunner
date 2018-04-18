(in-ns 'game.core)

(def card-definitions-ice-komainu
  {"Komainu"
   {:abilities [{:label "Gain subroutines"
                 :msg (msg "gain " (count (:hand runner)) " subroutines")}]
    :subroutines [(do-net-damage 1)]}})
