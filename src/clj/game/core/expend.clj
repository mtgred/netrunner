(ns game.core.expend
  (:require
   [game.core.payment :refer [can-pay? merge-costs]]
   [game.macros :refer [continue-ability req]]))

(defn expend
  [ex]
  (let [exp-cost [:click 1 :expend]
        merged-cost (if (some? (:cost ex))
                      (merge-costs (conj (:cost ex) exp-cost))
                      exp-cost)]
    {:req (req
            (and
              (can-pay?
                state side (assoc eid :source card :source-type :ability) card nil merged-cost)
              (if (some? (:req ex))
                ((:req ex) state side eid card targets)
                true)))
     :async true
     :effect (req (continue-ability state :corp (assoc ex :cost merged-cost) card nil))}))
