(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-government-takeover
  {"Government Takeover"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain :credit 3))
                 :msg "gain 3 [Credits]"}]}})