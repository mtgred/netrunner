(in-ns 'game.core)

(def card-definitions-agendas-self-destruct-chips
  {"Self-Destruct Chips"
   {:silent (req true)
    :msg "decrease the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size-modification 1))
    :swapped {:msg "decrease the Runner's maximum hand size by 1"
              :effect (effect (lose :runner :hand-size-modification 1))}
    :leave-play (effect (gain :runner :hand-size-modification 1))}})
