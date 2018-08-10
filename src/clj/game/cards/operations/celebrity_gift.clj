(in-ns 'game.cards.operations)

(def card-definition-celebrity-gift
  {"Celebrity Gift"
   {:choices {:max 5
              :req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg (msg "reveal " (join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (effect (gain-credits (* 2 (count targets))))}})
