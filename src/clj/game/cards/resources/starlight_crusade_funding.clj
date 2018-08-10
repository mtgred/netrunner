(in-ns 'game.cards.resources)

(def card-definition-starlight-crusade-funding
  {"Starlight Crusade Funding"
   {:msg "ignore additional costs on Double events"
    :effect (req (swap! state assoc-in [:runner :register :double-ignore-additional] true))
    :events {:runner-turn-begins
             {:msg "lose [Click] and ignore additional costs on Double events"
              :effect (req (lose state :runner :click 1)
                           (swap! state assoc-in [:runner :register :double-ignore-additional] true))}}
    :leave-play (req (swap! state update-in [:runner :register] dissoc :double-ignore-additional))}})
