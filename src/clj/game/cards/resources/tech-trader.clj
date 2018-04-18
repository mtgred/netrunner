(in-ns 'game.core)

(def card-definitions-resources-tech-trader
  {"Tech Trader"
   {:events {:runner-trash {:req (req (and (= side :runner) (= (second targets) :ability-cost)))
                            :msg "gain 1 [Credits]"
                            :effect (effect (gain :credit 1))}}}})
