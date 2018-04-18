(in-ns 'game.core)

(def card-definitions-events-executive-wiretaps
  {"Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand corp))))}})
