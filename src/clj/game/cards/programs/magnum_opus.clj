(in-ns 'game.cards.programs)

(def card-definition-magnum-opus
  {"Magnum Opus"
   {:abilities [{:cost [:click 1]
                 :effect (effect (gain-credits 2))
                 :msg "gain 2 [Credits]"}]}})
