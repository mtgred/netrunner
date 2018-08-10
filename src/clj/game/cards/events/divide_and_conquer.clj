(in-ns 'game.cards.events)

(def card-definition-divide-and-conquer
  {"Divide and Conquer"
   {:req (req archives-runnable)
    :makes-run true
    :async true
    :effect
    (effect (run :archives
                 {:end-run
                  {:async true
                   :effect
                   (req (wait-for (do-access state side [:hq] {:no-root true})
                                  (do-access state side eid [:rd] {:no-root true})))}}
                 card))}})
