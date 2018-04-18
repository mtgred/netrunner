(in-ns 'game.core)

(def card-definitions-assets-capital-investors
  {"Capital Investors"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}})
