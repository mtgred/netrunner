(in-ns 'game.core)

(def card-definitions-agendas-government-contracts
  {"Government Contracts"
   {:abilities [{:cost [:click 2]
                 :effect (effect (gain :credit 4))
                 :msg "gain 4 [Credits]"}]}})
