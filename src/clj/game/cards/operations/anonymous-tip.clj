(in-ns 'game.core)

(def card-definitions-operations-anonymous-tip
  {"Anonymous Tip"
   {:msg "draw 3 cards"
    :delayed-completion true
    :effect (effect (draw eid 3 nil))}})
