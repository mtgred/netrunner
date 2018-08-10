(in-ns 'game.cards.agendas)

(def card-definition-government-takeover
  {"Government Takeover"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain-credits 3))
                 :msg "gain 3 [Credits]"}]}})
