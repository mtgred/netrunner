(ns game.core.expend
  (:require
   [game.core.card-defs :refer [card-def]]
   [game.core.effects :refer [is-disabled-reg?]]
   [game.core.engine :refer [checkpoint queue-event resolve-ability]]
   [game.core.payment :refer [can-pay? merge-costs ->c]]
   [game.macros :refer [req wait-for]]))

(defn expendable?
  "Can a card be expended? (disabled cards will not retain the ability)"
  [state card]
  (and (:expend (card-def card)) (not (is-disabled-reg? state card))))

(defn expend
  [ex]
  (let [exp-cost [(->c :click 1) (->c :expend)]
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
     :effect (req
               (wait-for
                 (resolve-ability state :corp (assoc ex :cost merged-cost) card nil)
                 (queue-event state :expend-resolved {:card card})
                 (checkpoint state nil eid nil)))}))
