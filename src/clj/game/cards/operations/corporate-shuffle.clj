(in-ns 'game.core)

(def card-operations-corporate-shuffle
  {"Corporate Shuffle"
   {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
    :delayed-completion true
    :effect (effect (shuffle-into-deck :hand)
                    (draw eid 5 nil))}})