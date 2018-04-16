(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-sentinel-defense-program
  {"Sentinel Defense Program"
   {:events {:pre-resolve-damage {:req (req (and (= target :brain) (> (last targets) 0)))
                                  :msg "do 1 net damage"
                                  :effect (effect (damage eid :net 1 {:card card}))}}}})