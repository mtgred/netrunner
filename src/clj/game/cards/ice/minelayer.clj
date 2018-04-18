(in-ns 'game.core)

(def card-definitions-ice-minelayer
  {"Minelayer"
   {:subroutines [{:msg "install an ICE from HQ"
                   :choices {:req #(and (ice? %)
                                        (in-hand? %))}
                   :prompt "Choose an ICE to install from HQ"
                   :effect (req (corp-install state side target (zone->name (first (:server run))) {:no-install-cost true}))}]}})
