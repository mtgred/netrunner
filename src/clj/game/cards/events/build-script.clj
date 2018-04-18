(in-ns 'game.core)

(def card-definitions-events-build-script
  {"Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :effect (effect (gain :credit 1) (draw 2))}})
