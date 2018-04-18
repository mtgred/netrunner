(in-ns 'game.core)

(def card-definitions-assets-shock!
  {"Shock!"
   {:flags {:rd-reveal (req true)}
    :access {:msg "do 1 net damage"
             :delayed-completion true
             :effect (effect (damage eid :net 1 {:card card}))}}})
