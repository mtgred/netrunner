(in-ns 'game.cards.ice)

(def card-definition-pop-up-window
  {"Pop-up Window"
   {:implementation "Encounter effect is manual. Runner choice is not implemented"
    :abilities [(gain-credits-sub 1)]
    :subroutines [end-the-run]
    :runner-abilities [(runner-pay [:credit 1] 1)]}})
