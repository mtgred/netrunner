(in-ns 'game.cards.assets)

(def card-definition-cpc-generator
  {"CPC Generator"
   {:events {:runner-click-credit {:req (req (first-event? state side :runner-click-credit))
                                   :msg "gain 1 [Credits]"
                                   :effect (effect (gain-credits :corp 1))}}}})
