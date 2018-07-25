(in-ns 'game.cards.ice)

(def card-definition-shinobi
  {"Shinobi"
   {:effect take-bad-pub
    :subroutines [(trace-ability 1 (do-net-damage 1))
                  (trace-ability 2 (do-net-damage 2))
                  (trace-ability 3 {:label "Do 3 net damage and end the run"
                                    :msg "do 3 net damage and end the run"
                                    :effect (effect (damage eid :net 3 {:card card})
                                                    (end-run))})]}})
