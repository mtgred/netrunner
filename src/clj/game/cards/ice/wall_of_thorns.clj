(in-ns 'game.cards.ice)

(def card-definition-wall-of-thorns
  {"Wall of Thorns"
   {:subroutines [end-the-run
                  (do-net-damage 2)]}})
