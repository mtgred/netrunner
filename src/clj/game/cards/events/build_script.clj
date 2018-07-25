(in-ns 'game.cards.events)

(def card-definition-build-script
  {"Build Script"
   {:msg "gain 1 [Credits] and draw 2 cards"
    :effect (effect (gain-credits 1) (draw 2))}})
