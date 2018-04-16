(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-government-contracts
  {"Government Contracts"
   {:abilities [{:cost [:click 2]
                 :effect (effect (gain :credit 4))
                 :msg "gain 4 [Credits]"}]}})
