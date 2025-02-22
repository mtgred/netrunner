(ns game.core.purging
  (:require
    [game.core.board :refer [get-all-installed]]
    [game.core.card :refer [get-counters]]
    [game.core.eid :refer [effect-completed]]
    [game.core.effects :refer [get-effects]]
    [game.core.engine :refer [queue-event checkpoint]]
    [game.core.ice :refer [update-all-ice]]
    [game.core.props :refer [add-counter]]
    [game.macros :refer [wait-for]]))

(defn- remove-virus-counters [state side eid [{:keys [card quantity] :as current} & remainder]]
  (if-not current
    (effect-completed state side eid)
    (wait-for (add-counter state :runner card :virus (- quantity))
              (remove-virus-counters state side eid remainder))))

(defn purge
  "Purges viruses."
  [state side eid]
  (let [purge-preventions
        (->> (get-effects state side :prevent-purge-virus-counters)
             (reduce
               (fn [acc cur]
                 (assoc acc (-> cur :card :cid) cur))
               {}))
        cards-to-purge
        (->> (get-all-installed state)
             (keep (fn [card]
                     (let [qty (get-counters card :virus)
                           pp (get purge-preventions (:cid card))
                           qty (if pp
                                 (- qty (:quantity pp 0))
                                 qty)]
                       (when (pos? qty)
                         {:card card :quantity qty}))))
             (vec))]
    (wait-for
      (remove-virus-counters state side cards-to-purge)
      ;; (doseq [{:keys [card quantity]} cards-to-purge]
      ;; (add-counter state :runner card :virus (- quantity)))
      (update-all-ice state side)
      (let [total-purged-counters (reduce + 0 (mapv :quantity cards-to-purge))]
        (queue-event state :purge {:total-purged-counters total-purged-counters
                                   :purges cards-to-purge})
        (checkpoint state side eid)))))
