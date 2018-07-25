(in-ns 'game.cards.operations)

(def card-definition-traffic-accident
  {"Traffic Accident"
   {:req (req (>= (:tag runner) 2))
    :msg "do 2 meat damage"
    :async true
    :effect (effect (damage eid :meat 2 {:card card}))}})
