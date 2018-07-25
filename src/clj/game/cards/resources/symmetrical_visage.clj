(in-ns 'game.cards.resources)

(def card-definition-symmetrical-visage
  {"Symmetrical Visage"
   {:events {:runner-click-draw {:req (req (genetics-trigger? state side :runner-click-draw))
                                 :msg "gain 1 [Credits]"
                                 :effect (effect (gain-credits 1))}}}})
