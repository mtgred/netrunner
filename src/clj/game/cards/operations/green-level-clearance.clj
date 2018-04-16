(in-ns 'game.core)

(def card-operations-green-level-clearance
  {"Green Level Clearance"
   {:msg "gain 3 [Credits] and draw 1 card"
    :delayed-completion true
    :effect (effect (gain :credit 3)
                    (draw eid 1 nil))}})