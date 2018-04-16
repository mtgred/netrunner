(in-ns 'game.core)

(declare run-event)

(def card-events-traffic-jam
  {"Traffic Jam"
   {:effect (effect (update-all-advancement-costs))
    :leave-play (effect (update-all-advancement-costs))
    :events {:pre-advancement-cost
             {:effect (req (advancement-cost-bonus
                             state side (count (filter #(= (:title %) (:title target)) (:scored corp)))))}}}})
