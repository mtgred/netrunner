(in-ns 'game.cards.operations)

(def card-definition-blue-level-clearance
  {"Blue Level Clearance"
   {:msg "gain 5 [Credits] and draw 2 cards"
    :async true
    :effect (effect (gain-credits 5)
                    (draw eid 2 nil))}})
