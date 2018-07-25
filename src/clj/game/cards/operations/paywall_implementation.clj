(in-ns 'game.cards.operations)

(def card-definition-paywall-implementation
  {"Paywall Implementation"
   {:events {:successful-run {:msg "gain 1 [Credits]" :effect (effect (gain-credits :corp 1))}}}})
