(in-ns 'game.cards.agendas)

(def card-definition-remote-data-farm
  {"Remote Data Farm"
   {:silent (req true)
    :msg "increase their maximum hand size by 2"
    :effect (effect (gain :hand-size 2))
    :swapped {:msg "increase their maximum hand size by 2"
              :effect (effect (gain :hand-size 2))}
    :leave-play (effect (lose :hand-size 2))}})
