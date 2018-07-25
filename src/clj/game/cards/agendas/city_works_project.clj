(in-ns 'game.cards.agendas)

(def card-definition-city-works-project
  {"City Works Project"
   (letfn [(meat-damage [s c] (+ 2 (get-counters (get-card s c) :advancement)))]
     {:install-state :face-up
      :access {:req (req installed)
               :msg (msg "do " (meat-damage state card) " meat damage")
               :async true
               :effect (effect (damage eid :meat (meat-damage state card) {:card card}))}})})
