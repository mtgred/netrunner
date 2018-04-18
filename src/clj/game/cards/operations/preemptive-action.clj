(in-ns 'game.core)

(def card-definitions-operations-preemptive-action
  {"Preemptive Action"
   {:effect (effect (rfg-and-shuffle-rd-effect (first (:play-area corp)) 3))}})
