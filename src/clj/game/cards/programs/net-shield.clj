(in-ns 'game.core)

(def card-definitions-programs-net-shield
  {"Net Shield"
   {:prevent {:damage [:net]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}})
