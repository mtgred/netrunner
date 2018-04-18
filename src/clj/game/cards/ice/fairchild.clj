(in-ns 'game.core)

(def card-definitions-ice-fairchild
  {"Fairchild"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:credit 4] 1)]}})
