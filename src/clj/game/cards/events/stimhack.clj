(in-ns 'game.core)

(def card-definitions-events-stimhack
  {"Stimhack"
   (run-event
    nil
    {:end-run {:msg "take 1 brain damage"
               :effect (effect (damage eid :brain 1 {:unpreventable true :card card}))}}
    (effect (gain-run-credits 9)))})
