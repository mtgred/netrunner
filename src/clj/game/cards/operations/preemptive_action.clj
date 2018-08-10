(in-ns 'game.cards.operations)

(def card-definition-preemptive-action
  {"Preemptive Action"
   {:effect (effect (rfg-and-shuffle-rd-effect (first (:play-area corp)) (min (count (:discard corp)) 3) true))}})
