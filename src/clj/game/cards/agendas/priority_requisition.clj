(in-ns 'game.cards.agendas)

(def card-definition-priority-requisition
  {"Priority Requisition"
   {:interactive (req true)
    :choices {:req #(and (ice? %)
                         (not (rezzed? %))
                         (installed? %))}
    :effect (effect (rez target {:ignore-cost :all-costs}))}})
