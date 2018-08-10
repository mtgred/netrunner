(in-ns 'game.cards.assets)

(def card-definition-shock
  {"Shock!"
   {:flags {:rd-reveal (req true)}
    :access {:msg "do 1 net damage"
             :async true
             :effect (effect (damage eid :net 1 {:card card}))}}})
