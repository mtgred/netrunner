(in-ns 'game.core)

(declare draft-points-target has-most-faction?)

(def card-identities-reina-roja-freedom-fighter
  {"Reina Roja: Freedom Fighter"
   {:events {:pre-rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                       :effect (effect (rez-cost-bonus 1))}
             :rez {:req (req (and (ice? target) (not (get-in @state [:per-turn (:cid card)]))))
                   :effect (req (swap! state assoc-in [:per-turn (:cid card)] true))}}}})
