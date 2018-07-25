(in-ns 'game.cards.identities)

(def card-definition-reina-roja-freedom-fighter
  {"Reina Roja: Freedom Fighter"
   (letfn [(not-triggered? [state card] (not (get-in @state [:per-turn (:cid card)])))
           (mark-triggered [state card] (swap! state assoc-in [:per-turn (:cid card)] true))]
     {:effect (req (when (pos? (event-count state :corp :rez #(ice? (first %))))
                     (mark-triggered state card)))
      :events {:pre-rez {:req (req (and (ice? target)
                                        (not-triggered? state card)))
                         :effect (effect (rez-cost-bonus 1))}
               :rez {:req (req (and (ice? target)
                                    (not-triggered? state card)))
                     :effect (req (mark-triggered state card))}}})})
