(in-ns 'game.cards.events)

(def card-definition-executive-wiretaps
  {"Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand corp))))}})
