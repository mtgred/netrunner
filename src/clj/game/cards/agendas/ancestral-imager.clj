(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-ancestral-imager
  {"Ancestral Imager"
   {:events {:jack-out {:msg "do 1 net damage"
                        :effect (effect (damage :net 1))}}}})