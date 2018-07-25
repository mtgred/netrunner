(in-ns 'game.cards.ice)

(def card-definition-negotiator
  {"Negotiator"
   {:subroutines [(gain-credits-sub 2)
                  trash-program]
    :runner-abilities [(runner-break [:credit 2] 1)]}})
