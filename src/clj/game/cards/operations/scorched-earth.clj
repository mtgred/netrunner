(in-ns 'game.core)

(def card-operations-scorched-earth
  {"Scorched Earth"
   {:req (req tagged)
    :delayed-completion true
    :msg "do 4 meat damage"
    :effect (effect (damage eid :meat 4 {:card card}))}})
