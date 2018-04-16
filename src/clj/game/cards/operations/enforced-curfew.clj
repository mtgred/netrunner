(in-ns 'game.core)

(def card-operations-enforced-curfew
  {"Enforced Curfew"
   {:msg "reduce the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size-modification 1))
    :leave-play (effect (gain :runner :hand-size-modification 1))}})