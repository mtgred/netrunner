(in-ns 'game.cards.operations)

(def card-definition-green-level-clearance
  {"Green Level Clearance"
   {:msg "gain 3 [Credits] and draw 1 card"
    :async true
    :effect (effect (gain-credits 3)
                    (draw eid 1 nil))}})
