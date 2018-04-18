(in-ns 'game.core)

(def card-definitions-ice-pop-up-window
  {"Pop-up Window"
   {:implementation "Encounter effect is manual. Runner choice is not implemented"
    :abilities [(gain-credits 1)]
    :subroutines [end-the-run]
    :runner-abilities [(runner-break [:credit 1] 1)]}})
