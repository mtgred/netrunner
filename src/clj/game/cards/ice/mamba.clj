(in-ns 'game.core)

(def card-definitions-ice-mamba
  {"Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))]
    :subroutines [(do-net-damage 1)
                  (do-psi add-power-counter)]}})
