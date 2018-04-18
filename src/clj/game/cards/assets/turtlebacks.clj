(in-ns 'game.core)

(def card-definitions-assets-turtlebacks
  {"Turtlebacks"
   {:events {:server-created {:msg "gain 1 [Credits]" :effect (effect (gain :credit 1))}}}})
