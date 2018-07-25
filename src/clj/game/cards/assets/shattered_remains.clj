(in-ns 'game.cards.assets)

(def card-definition-shattered-remains
  {"Shattered Remains"
   (advance-ambush 1 {:async true
                      :req (req (pos? (get-counters (get-card state card) :advancement)))
                      :effect (req (let [counters (get-counters (get-card state card) :advancement)]
                                     (continue-ability
                                       state side
                                       (-> trash-hardware
                                           (assoc-in [:choices :max] counters)
                                           (assoc :prompt (msg "Select " (quantify counters "piece") " of hardware to trash")
                                                  :effect (effect (trash-cards targets))
                                                  :msg (msg "trash " (join ", " (map :title targets)))))
                                       card nil)))})})
