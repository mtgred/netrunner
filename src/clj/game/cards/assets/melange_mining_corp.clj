(in-ns 'game.cards.assets)

(def card-definition-melange-mining-corp
  {"Melange Mining Corp."
   {:abilities [{:cost [:click 3]
                 :effect (effect (gain-credits 7))
                 :msg "gain 7 [Credits]"}]}})
