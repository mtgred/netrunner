(in-ns 'game.core)

(def card-definitions-ice-wotan
  {"Wotan"
   {:subroutines [end-the-run
                  (do-brain-damage 1)]
    :runner-abilities [(runner-break [:click 2] 1)
                       (runner-break [:credit 3] 1)]}})
