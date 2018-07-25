(in-ns 'game.cards.operations)

(def card-definition-neural-emp
  {"Neural EMP"
   {:req (req (last-turn? state :runner :made-run))
    :msg "do 1 net damage"
    :effect (effect (damage eid :net 1 {:card card}))}})
