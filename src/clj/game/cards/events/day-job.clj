(in-ns 'game.core)

(declare run-event)

(def card-events-day-job
  {"Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain :credit 10))}})