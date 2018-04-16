(in-ns 'game.core)

(def card-operations-celebrity-gift
  {"Celebrity Gift"
   {:choices {:max 5
              :req #(and (= (:side %) "Corp")
                         (in-hand? %))}
    :msg (msg "reveal " (join ", " (map :title (sort-by :title targets))) " and gain " (* 2 (count targets)) " [Credits]")
    :effect (final-effect (gain :credit (* 2 (count targets))))}})