(in-ns 'game.cards.ice)

(def card-definition-mamba
  {"Mamba"
   {:abilities [(power-counter-ability (do-net-damage 1))]
    :subroutines [(do-net-damage 1)
                  (do-psi add-power-counter)]}})
