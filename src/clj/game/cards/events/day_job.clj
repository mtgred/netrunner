(in-ns 'game.cards.events)

(def card-definition-day-job
  {"Day Job"
   {:additional-cost [:click 3]
    :msg "gain 10 [Credits]" :effect (effect (gain-credits 10))}})
