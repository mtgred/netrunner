(in-ns 'game.cards.agendas)

(def card-definition-sentinel-defense-program
  {"Sentinel Defense Program"
   {:events {:pre-resolve-damage {:req (req (and (= target :brain)
                                                 (pos? (last targets))))
                                  :msg "do 1 net damage"
                                  :effect (effect (damage eid :net 1 {:card card}))}}}})
