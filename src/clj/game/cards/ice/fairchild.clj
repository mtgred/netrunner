(in-ns 'game.cards.ice)

(def card-definition-fairchild
  {"Fairchild"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:credit 4] 1)]}})
