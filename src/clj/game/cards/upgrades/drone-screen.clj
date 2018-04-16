(in-ns 'game.core)

(declare expose-prevent)

(def card-upgrades-drone-screen
  {"Drone Screen"
   {:events {:run {:req (req (and this-server tagged))
                   :delayed-completion true
                   :trace {:base 3
                           :msg "do 1 meat damage"
                           :effect (effect (damage eid :meat 1 {:card card :unpreventable true}))}}}}})
