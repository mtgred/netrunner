(in-ns 'game.cards.ice)

(def card-definition-wotan
  {"Wotan"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :runner-abilities [(runner-pay [:click 2] 1)
                       (runner-pay [:credit 3] 1)]}})
