(in-ns 'game.core)

(def card-definitions-events-day-job
  {"Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain :credit 10))}})
