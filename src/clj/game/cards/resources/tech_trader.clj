(in-ns 'game.cards.resources)

(def card-definition-tech-trader
  {"Tech Trader"
   {:events {:runner-trash {:req (req (and (= side :runner) (= (second targets) :ability-cost)))
                            :msg "gain 1 [Credits]"
                            :effect (effect (gain-credits 1))}}}})
