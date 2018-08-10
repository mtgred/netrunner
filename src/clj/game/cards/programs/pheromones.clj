(in-ns 'game.cards.programs)

(def card-definition-pheromones
  {"Pheromones"
   {:recurring (req (when (< (get-counters card :recurring) (get-counters card :virus))
                      (set-prop state side card :rec-counter (get-counters card :virus))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :hq))
                              :effect (effect (add-counter card :virus 1))}}}})
