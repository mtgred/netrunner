(in-ns 'game.cards.operations)

(def card-definition-anonymous-tip
  {"Anonymous Tip"
   {:msg "draw 3 cards"
    :async true
    :effect (effect (draw eid 3 nil))}})
