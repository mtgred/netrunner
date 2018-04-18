(in-ns 'game.core)

(def card-definitions-operations-violet-level-clearance
  {"Violet Level Clearance"
   {:msg "gain 8 [Credits] and draw 4 cards"
    :delayed-completion true
    :effect (effect (gain :credit 8)
                    (draw eid 4 nil))}})
