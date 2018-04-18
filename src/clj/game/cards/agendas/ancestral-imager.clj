(in-ns 'game.core)

(def card-definitions-agendas-ancestral-imager
  {"Ancestral Imager"
   {:events {:jack-out {:msg "do 1 net damage"
                        :effect (effect (damage :net 1))}}}})
