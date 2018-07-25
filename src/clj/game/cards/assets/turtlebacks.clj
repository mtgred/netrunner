(in-ns 'game.cards.assets)

(def card-definition-turtlebacks
  {"Turtlebacks"
   {:events {:server-created {:msg "gain 1 [Credits]"
                              :effect (effect (gain-credits 1))}}}})
