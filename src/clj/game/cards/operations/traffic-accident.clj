(in-ns 'game.core)

(def card-operations-traffic-accident
  {"Traffic Accident"
   {:req (req (>= (:tag runner) 2))
    :msg "do 2 meat damage"
    :delayed-completion true
    :effect (effect (damage eid :meat 2 {:card card}))}})
