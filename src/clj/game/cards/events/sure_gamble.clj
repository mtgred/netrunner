(in-ns 'game.cards.events)

(def card-definition-sure-gamble
  {"Sure Gamble"
   {:msg "gain 9 [Credits]" :effect (effect (gain-credits 9))}})
