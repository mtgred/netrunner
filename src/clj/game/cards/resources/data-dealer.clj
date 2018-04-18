(in-ns 'game.core)

(def card-definitions-resources-data-dealer
  {"Data Dealer"
   {:abilities [{:cost [:click 1 :forfeit] :effect (effect (gain :credit 9))
                 :msg (msg "gain 9 [Credits]")}]}})
