(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-city-works-project
  {"City Works Project"
   (letfn [(meat-damage [s c] (+ 2 (:advance-counter (get-card s c) 0)))]
     {:install-state :face-up
      :access {:req (req installed)
               :msg (msg "do " (meat-damage state card) " meat damage")
               :delayed-completion true
               :effect (effect (damage eid :meat (meat-damage state card) {:card card}))}})})
