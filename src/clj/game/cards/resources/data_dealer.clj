(in-ns 'game.cards.resources)

(def card-definition-data-dealer
  {"Data Dealer"
   {:abilities [{:cost [:click 1 :forfeit] :effect (effect (gain-credits 9))
                 :msg (msg "gain 9 [Credits]")}]}})
