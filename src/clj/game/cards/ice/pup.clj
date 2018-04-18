(in-ns 'game.core)

(def card-definitions-ice-pup
  {"Pup"
   {:subroutines [(do-net-damage 1)]
    :runner-abilities [(runner-break [:credit 1] 1)]}})
