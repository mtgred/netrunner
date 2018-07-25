(in-ns 'game.cards.operations)

(def card-definition-surveillance-sweep
  {"Surveillance Sweep"
   {:events {:run {:effect (req (swap! state assoc-in [:trace :player] :runner))}
             :run-end {:effect (req (swap! state dissoc-in [:trace :player]))}}
    :leave-play (req (swap! state dissoc-in [:trace :player]))}})
