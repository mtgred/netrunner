(in-ns 'game.core)

(def card-definitions-assets-aggressive-secretary
  {"Aggressive Secretary"
   (advance-ambush 2 {:req (req (< 0 (:advance-counter (get-card state card) 0)))
                      :delayed-completion true
                      :effect
                      (req (let [agg (get-card state card)
                                 n (:advance-counter agg 0)
                                 ab (-> trash-program
                                        (assoc-in [:choices :max] n)
                                        (assoc :prompt (msg "Choose " (quantify n "program") " to trash")
                                               :delayed-completion true
                                               :effect (effect (trash-cards eid targets nil))
                                               :msg (msg "trash " (join ", " (map :title targets)))))]
                             (continue-ability state side ab agg nil)))})})
