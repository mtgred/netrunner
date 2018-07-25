(in-ns 'game.cards.ice)

(def card-definition-turing
  {"Turing"
   {:implementation "AI restriction not implemented"
    :subroutines [end-the-run]
    :strength-bonus (req (if (is-remote? (second (:zone card))) 3 0))
    :runner-abilities [(runner-pay [:click 3] 1)]}})
