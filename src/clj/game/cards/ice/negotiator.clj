(in-ns 'game.core)

(def card-definitions-ice-negotiator
  {"Negotiator"
   {:subroutines [(gain-credits 2)
                  trash-program]
    :runner-abilities [(runner-break [:credit 2] 1)]}})
