(in-ns 'game.cards.agendas)

(def card-definition-ancestral-imager
  {"Ancestral Imager"
   {:events {:jack-out {:msg "do 1 net damage"
                        :effect (effect (damage :net 1))}}}})
