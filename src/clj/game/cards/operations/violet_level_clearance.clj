(in-ns 'game.cards.operations)

(def card-definition-violet-level-clearance
  {"Violet Level Clearance"
   {:msg "gain 8 [Credits] and draw 4 cards"
    :async true
    :effect (effect (gain-credits 8)
                    (draw eid 4 nil))}})
