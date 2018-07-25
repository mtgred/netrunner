(in-ns 'game.cards.assets)

(def card-definition-capital-investors
  {"Capital Investors"
   {:abilities [{:cost [:click 1]
                 :msg "gain 2 [Credits]"
                 :effect (effect (gain-credits 2))}]}})
