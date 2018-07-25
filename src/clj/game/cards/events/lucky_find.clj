(in-ns 'game.cards.events)

(def card-definition-lucky-find
  {"Lucky Find"
   {:msg "gain 9 [Credits]"
    :effect (effect (gain-credits 9))}})
