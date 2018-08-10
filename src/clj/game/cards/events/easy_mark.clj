(in-ns 'game.cards.events)

(def card-definition-easy-mark
  {"Easy Mark"
   {:msg "gain 3 [Credits]" :effect (effect (gain-credits 3))}})
