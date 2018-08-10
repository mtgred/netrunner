(in-ns 'game.cards.agendas)

(def card-definition-government-contracts
  {"Government Contracts"
   {:abilities [{:cost [:click 2]
                 :effect (effect (gain-credits 4))
                 :msg "gain 4 [Credits]"}]}})
