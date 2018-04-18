(in-ns 'game.core)

(def card-definitions-agendas-sentinel-defense-program
  {"Sentinel Defense Program"
   {:events {:pre-resolve-damage {:req (req (and (= target :brain) (> (last targets) 0)))
                                  :msg "do 1 net damage"
                                  :effect (effect (damage eid :net 1 {:card card}))}}}})
