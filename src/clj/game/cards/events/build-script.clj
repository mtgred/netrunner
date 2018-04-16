(in-ns 'game.core)

(declare run-event)

(def card-events-build-script
  {"Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :effect (effect (gain :credit 1) (draw 2))}})