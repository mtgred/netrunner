(in-ns 'game.cards.operations)

(def card-definition-lag-time
  {"Lag Time"
   {:effect (effect (update-all-ice))
    :events {:pre-ice-strength {:effect (effect (ice-strength-bonus 1 target))}}
    :leave-play (effect (update-all-ice))}})
