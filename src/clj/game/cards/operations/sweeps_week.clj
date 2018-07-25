(in-ns 'game.cards.operations)

(def card-definition-sweeps-week
  {"Sweeps Week"
   {:effect (effect (gain-credits (count (:hand runner))))
    :msg (msg "gain " (count (:hand runner)) " [Credits]")}})
