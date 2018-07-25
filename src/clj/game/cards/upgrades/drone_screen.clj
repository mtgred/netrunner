(in-ns 'game.cards.upgrades)

(def card-definition-drone-screen
  {"Drone Screen"
   {:events {:run {:req (req (and this-server tagged))
                   :async true
                   :trace {:base 3
                           :successful
                           {:msg "do 1 meat damage"
                            :effect (effect (damage eid :meat 1 {:card card
                                                                 :unpreventable true}))}}}}}})
