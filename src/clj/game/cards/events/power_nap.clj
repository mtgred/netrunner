(in-ns 'game.cards.events)

(def card-definition-power-nap
  {"Power Nap"
   {:effect (effect (gain-credits (+ 2 (count (filter #(has-subtype? % "Double")
                                                      (:discard runner))))))
    :msg (msg "gain " (+ 2 (count (filter #(has-subtype? % "Double") (:discard runner)))) " [Credits]")}})
