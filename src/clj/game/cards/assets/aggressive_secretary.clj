(in-ns 'game.cards.assets)

(def card-definition-aggressive-secretary
  {"Aggressive Secretary"
   (advance-ambush 2 {:req (req (pos? (get-counters (get-card state card) :advancement)))
                      :async true
                      :effect (req (let [agg (get-counters (get-card state card) :advancement)
                                         ab (-> trash-program
                                                (assoc-in [:choices :max] agg)
                                                (assoc :prompt (msg "Choose " (quantify agg "program") " to trash")
                                                       :async true
                                                       :effect (effect (trash-cards eid targets nil))
                                                       :msg (msg "trash " (join ", " (map :title targets)))))]
                                     (continue-ability state side ab card nil)))})})
