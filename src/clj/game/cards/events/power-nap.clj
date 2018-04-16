(in-ns 'game.core)

(declare run-event)

(def card-events-power-nap
  {"Power Nap"
   {:effect (effect (gain :credit (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard runner))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")}})