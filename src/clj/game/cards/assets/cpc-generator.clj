(in-ns 'game.core)

(def card-definitions-assets-cpc-generator
  {"CPC Generator"
   {:events {:runner-click-credit {:req (req (first-event? state side :runner-click-credit))
                                   :msg "gain 1 [Credits]"
                                   :effect (effect (gain :corp :credit 1))}}}})
