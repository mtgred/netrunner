(in-ns 'game.core)

(declare run-event)

(def card-events-amped-up
  {"Amped Up"
   {:msg "gain [Click][Click][Click] and suffer 1 brain damage"
    :effect (effect (gain :click 3) (damage eid :brain 1 {:unpreventable true :card card}))}})