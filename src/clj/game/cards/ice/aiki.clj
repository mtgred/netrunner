(in-ns 'game.cards.ice)

(def card-definition-aiki
  {"Aiki"
   {:subroutines [(do-psi {:label "Runner draws 2 cards"
                           :msg "make the Runner draw 2 cards"
                           :effect (effect (draw :runner 2))})
                  (do-net-damage 1)]}})
