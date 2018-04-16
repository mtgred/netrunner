(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-remote-data-farm
  {"Remote Data Farm"
   {:silent (req true)
    :msg "increase their maximum hand size by 2"
    :effect (effect (gain :hand-size-modification 2))
    :swapped {:msg "increase their maximum hand size by 2"
              :effect (effect (gain :hand-size-modification 2))}
    :leave-play (effect (lose :hand-size-modification 2))}})
