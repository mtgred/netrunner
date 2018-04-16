(in-ns 'game.core)

(declare can-host?)

(def card-programs-magnum-opus
  {"Magnum Opus"
   {:abilities [{:cost [:click 1] :effect (effect (gain :credit 2)) :msg "gain 2 [Credits]"}]}})
