(in-ns 'game.core)

(declare can-host?)

(def card-programs-clot
  {"Clot"
   {:effect (req (let [agendas (map first (filter #(is-type? (first %) "Agenda")
                                                  (turn-events state :corp :corp-install)))]
                   (swap! state assoc-in [:corp :register :cannot-score] agendas)))
    :events {:purge {:effect (req (swap! state update-in [:corp :register] dissoc :cannot-score)
                                  (trash state side card))}
             :corp-install {:req (req (is-type? target "Agenda"))
                            :effect (req (swap! state update-in [:corp :register :cannot-score] #(cons target %)))}}
    :leave-play (req (swap! state update-in [:corp :register] dissoc :cannot-score))}})