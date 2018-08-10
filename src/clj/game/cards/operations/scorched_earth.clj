(in-ns 'game.cards.operations)

(def card-definition-scorched-earth
  {"Scorched Earth"
   {:req (req tagged)
    :async true
    :msg "do 4 meat damage"
    :effect (effect (damage eid :meat 4 {:card card}))}})
