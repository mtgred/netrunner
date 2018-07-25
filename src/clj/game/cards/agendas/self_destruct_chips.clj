(in-ns 'game.cards.agendas)

(def card-definition-self-destruct-chips
  {"Self-Destruct Chips"
   {:silent (req true)
    :msg "decrease the Runner's maximum hand size by 1"
    :effect (effect (lose :runner :hand-size 1))
    :swapped {:msg "decrease the Runner's maximum hand size by 1"
              :effect (effect (lose :runner :hand-size 1))}
    :leave-play (effect (gain :runner :hand-size 1))}})
