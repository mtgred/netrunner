(in-ns 'game.core)

(def card-definitions-programs-pheromones
  {"Pheromones"
   {:recurring (req (when (< (get card :rec-counter 0) (get-in card [:counter :virus] 0))
                      (set-prop state side card :rec-counter
                                (get-in card [:counter :virus] 0))))
    :events {:successful-run {:silent (req true)
                              :req (req (= target :hq))
                              :effect (effect (add-counter card :virus 1))}}}})
