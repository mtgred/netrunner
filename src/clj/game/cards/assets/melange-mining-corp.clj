(in-ns 'game.core)

(def card-definitions-assets-melange-mining-corp
  {"Melange Mining Corp."
   {:abilities [{:cost [:click 3] :effect (effect (gain :credit 7)) :msg "gain 7 [Credits]"}]}})
