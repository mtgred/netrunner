(in-ns 'game.cards.programs)

(def card-definition-net-shield
  {"Net Shield"
   {:interactions {:prevent [{:type #{:net}
                              :req (req true)}]}
    :abilities [{:cost [:credit 1] :once :per-turn :msg "prevent the first net damage this turn"
                 :effect (effect (damage-prevent :net 1))}]}})
