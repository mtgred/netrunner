(in-ns 'game.cards.ice)

(def card-definition-pup
  {"Pup"
   {:subroutines [(do-net-damage 1)]
    :runner-abilities [(runner-pay [:credit 1] 1)]}})
