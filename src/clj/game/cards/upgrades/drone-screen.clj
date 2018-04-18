(in-ns 'game.core)

(def card-definitions-upgrades-drone-screen
  {"Drone Screen"
   {:events {:run {:req (req (and this-server tagged))
                   :delayed-completion true
                   :trace {:base 3
                           :msg "do 1 meat damage"
                           :effect (effect (damage eid :meat 1 {:card card :unpreventable true}))}}}}})
