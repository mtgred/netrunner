(in-ns 'game.core)

(def card-operations-enhanced-login-protocol
  {"Enhanced Login Protocol"
   (letfn [(elp-activated [state]
             (get-in @state [:corp :register :elp-activated] false))
           (add-effect [state side]
             (swap! state assoc-in [:corp :register :elp-activated] true)
             (click-run-cost-bonus state side [:click 1]))
           (remove-effect [state side]
             (click-run-cost-bonus state side [:click -1])
             (swap! state update-in [:corp :register] dissoc :elp-activated))]
     {:effect (req (when (and (= :runner (:active-player @state))
                              (not (:made-click-run runner-reg)))
                     (add-effect state side)
                     (system-msg state side (str "uses Enhanced Login Protocol to add an additional cost of [Click]"
                                                 " to make the first run not through a card ability this turn"))))
      :events {:runner-turn-begins {:msg "add an additional cost of [Click] to make the first run not through a card ability this turn"
                                    :effect (effect (add-effect))}
               :runner-turn-ends {:req (req (elp-activated state))
                                  :effect (effect (remove-effect))}
               :run-ends {:req (req (and (elp-activated state)
                                         (:made-click-run runner-reg)))
                          :effect (effect (remove-effect))}}
      :leave-play (req (when (elp-activated state)
                         (remove-effect state side)))})})
