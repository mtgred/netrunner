(in-ns 'game.core)

(declare run-event)

(def card-events-executive-wiretaps
  {"Executive Wiretaps"
   {:msg (msg "reveal cards in HQ: " (join ", " (map :title (:hand corp))))}})
