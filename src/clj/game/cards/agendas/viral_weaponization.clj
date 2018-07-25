(in-ns 'game.cards.agendas)

(def card-definition-viral-weaponization
  {"Viral Weaponization"
   (let [dmg {:msg "do 1 net damage for each card in the grip"
              :async true
              :effect (req (let [cnt (count (:hand runner))]
                             (unregister-events state side card)
                             (damage state side eid :net cnt {:card card})))}]
     {:effect (effect (register-events
                        {:corp-turn-ends dmg
                         :runner-turn-ends dmg}
                        card))
      :events {:corp-turn-ends nil
               :runner-turn-ends nil}})})
