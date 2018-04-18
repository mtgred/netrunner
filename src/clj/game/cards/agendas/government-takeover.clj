(in-ns 'game.core)

(def card-definitions-agendas-government-takeover
  {"Government Takeover"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain :credit 3))
                 :msg "gain 3 [Credits]"}]}})
