(in-ns 'game.core)

(def card-definitions-ice-turing
  {"Turing"
   {:implementation "AI restriction not implemented"
    :subroutines [end-the-run]
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))
    :runner-abilities [(runner-break [:click 3] 1)]}})
