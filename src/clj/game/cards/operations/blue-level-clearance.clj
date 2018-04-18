(in-ns 'game.core)

(def card-definitions-operations-blue-level-clearance
  {"Blue Level Clearance"
   {:msg "gain 5 [Credits] and draw 2 cards"
    :delayed-completion true
    :effect (effect (gain :credit 5)
                    (draw eid 2 nil))}})
