(in-ns 'game.cards.events)

(def card-definition-interdiction
  {"Interdiction"
   (let [ab (effect (register-turn-flag!
                     card :can-rez
                     (fn [state side card]
                       (if (and (= (:active-player @state) :runner) (not (ice? card)))
                         ((constantly false)
                          (toast state :corp "Cannot rez non-ICE on the Runner's turn due to Interdiction"))
                         true))))]
     {:msg "prevent the Corp from rezzing non-ICE cards on the Runner's turn"
      :effect ab
      :events {:runner-turn-begins {:effect ab}}
      :leave-play (req (clear-all-flags-for-card! state side card))})})
