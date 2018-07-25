(in-ns 'game.cards.operations)

(def card-definition-salems-hospitality
  {"Salem's Hospitality"
   {:prompt "Name a Runner card"
    :choices {:card-title (req (and (card-is? target :side "Runner")
                                    (not (card-is? target :type "Identity"))))}
    :effect (req (system-msg state side
                             (str "uses Salem's Hospitality to reveal the Runner's Grip ( "
                                  (join ", " (map :title (sort-by :title (:hand runner))))
                                  " ) and trash any copies of " target))
                 (doseq [c (filter #(= target (:title %)) (:hand runner))]
                   (trash state side c {:unpreventable true})))}})
