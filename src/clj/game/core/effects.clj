(ns game.core.effects
  (:require [game.core.card-defs :refer [card-def]]
            [game.core.eid :refer [make-eid]]
            [game.core.card :refer [get-card]]
            [game.utils :refer [same-card? to-keyword]]))

(defn register-persistent-effects
  [state card]
  (when-let [abilities (:persistent-effects (card-def card))]
    (swap! state update :effects
           #(apply conj % (for [ability abilities]
                            (assoc
                              (select-keys ability [:type :req :effect])
                              :duration :persistent
                              :card card)))))
  (:effects @state))

(defn unregister-persistent-effects
  [state card]
  (when (:persistent-effects (card-def card))
    (swap! state assoc :effects
           (->> (:effects @state)
                (remove #(same-card? (:card %) card))
                (into [])))))

(defn create-floating-effect
  [state card ability]
  (swap! state update :effects
         conj (assoc
                (select-keys ability [:type :duration :req :effect])
                :card card))
  (:effects @state))

(defn remove-floating-effects
  [state duration]
  (swap! state assoc :effects
         (->> (:effects @state)
              (remove #(= duration (:duration %)))
              (into []))))

(defn- gather-effects
  [state side effect-type]
  (let [get-side #(-> % :card :side to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]
    (->> (:effects @state)
         (filter #(= effect-type (:type %)))
         (sort-by (complement is-active-player)))))

(defn get-effects
  "Filters and then 'executes' the effects of a given type."
  [state side card effect-type]
  (let [eid (make-eid state)]
    (->> (gather-effects state side effect-type)
         (filter #(if-not (:req %)
                    true
                    ((:req %) state side eid (get-card state (:card %)) [card])))
         (mapv #((:effect %) state side eid (get-card state (:card %)) [card])))))

(defn sum-effects
  "Sums the results from get-effects."
  [state side card effect-type]
  (reduce + (get-effects state side card effect-type)))
