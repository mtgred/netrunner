(ns game.core.update
  (:require [game.core.card :refer [get-card]]
            [game.core.finding :refer [get-scoring-owner]]
            [game.utils :refer [to-keyword]]))

(declare update-hosted!)

(defn update!
  "Updates the state so that its copy of the given card matches the argument given."
  [state side {:keys [type zone cid host] :as card}]
  (cond
    (= type "Identity")
    (when (= side (to-keyword (:side card)))
      (swap! state assoc-in [side :identity] card)
      card)

    host
    (do (update-hosted! state side card)
        (get-card state card))

    :else
    (let [z (cons (to-keyword (or (get-scoring-owner state card) (:side card))) zone)
              [head tail] (split-with #(not= (:cid %) cid) (get-in @state z))]
          (when (not-empty tail)
            (swap! state assoc-in z (vec (concat head [card] (rest tail))))
            card))))

(defn update-hosted!
  "Updates a card that is hosted on another, by recursively updating the host card's
  :hosted vector."
  [state side {:keys [cid] :as card}]
  (if-let [h (get-card state (:host card))]
    (recur state side (let [[head tail] (split-with #(not= (:cid %) cid) (:hosted h))]
                        (assoc h :hosted (vec (concat head [card] (rest tail))))))
    (when-not (:host card)
      (update! state side card))))
