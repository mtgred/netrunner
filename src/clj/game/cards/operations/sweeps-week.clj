(in-ns 'game.core)

(def card-definitions-operations-sweeps-week
  {"Sweeps Week"
   {:effect (effect (gain :credit (count (:hand runner))))
    :msg (msg "gain " (count (:hand runner)) " [Credits]")}})
