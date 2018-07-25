(in-ns 'game.cards.operations)

(def card-definition-corporate-shuffle
  {"Corporate Shuffle"
   {:msg "shuffle all cards in HQ into R&D and draw 5 cards"
    :async true
    :effect (effect (shuffle-into-deck :hand)
                    (draw eid 5 nil))}})
