(in-ns 'game.cards.operations)

(def card-definition-enforced-curfew
  {"Enforced Curfew"
   {:msg "reduce the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size 1))
    :leave-play (effect (gain :runner :hand-size 1))}})
