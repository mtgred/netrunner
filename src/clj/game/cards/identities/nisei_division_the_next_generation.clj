(in-ns 'game.cards.identities)

(def card-definition-nisei-division-the-next-generation
  {"Nisei Division: The Next Generation"
   {:events {:psi-game {:msg "gain 1 [Credits]" :effect (effect (gain-credits :corp 1))}}}})
