(in-ns 'game.core)

(declare is-scored? ice-boost-agenda)

(def card-agendas-priority-requisition
  {"Priority Requisition"
   {:interactive (req true)
    :choices {:req #(and (ice? %)
                         (not (rezzed? %))
                         (installed? %))}
    :effect (effect (rez target {:ignore-cost :all-costs}))}})