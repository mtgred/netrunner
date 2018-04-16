(in-ns 'game.core)

(declare can-host?)

(def card-programs-net-shield
  {"Net Shield"
   {:prevent {:damage [:net]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}})
