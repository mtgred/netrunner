(in-ns 'game.cards.ice)

(def card-definition-endless-eula
  {"Endless EULA"
   {:subroutines [end-the-run]
    :runner-abilities [(runner-pay [:credit 1] 1)
                       (runner-pay [:credit 6] 6)]}})
