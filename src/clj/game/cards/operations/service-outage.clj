(in-ns 'game.core)

(def card-definitions-operations-service-outage
  {"Service Outage"
   (letfn [(so-activated [state]
             (get-in @state [:corp :register :so-activated] false))
           (add-effect [state side]
             (swap! state assoc-in [:corp :register :so-activated] true)
             (run-cost-bonus state side [:credit 1]))
           (remove-effect [state side]
             (run-cost-bonus state side [:credit -1])
             (swap! state update-in [:corp :register] dissoc :so-activated))]
     {:msg "add a cost of 1 [Credit] for the Runner to make the first run each turn"
      :effect (req (when (and (= :runner (:active-player @state))
                              (empty? (:made-run runner-reg)))
                     (add-effect state side)))
      :events {:runner-turn-begins {:msg "add an additional cost of 1 [Credit] to make the first run this turn"
                                    :effect (effect (add-effect))}
               :runner-turn-ends {:req (req (so-activated state))
                                  :effect (effect (remove-effect))}
               :run-ends {:req (req (so-activated state))
                          :effect (effect (remove-effect))}}
      :leave-play (req (when (so-activated state)
                         (remove-effect state side)))})})
