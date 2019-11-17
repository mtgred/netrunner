(ns game.core.effects
  (:require [game.core.card-defs :refer [card-def]]
            [game.core.eid :refer [make-eid]]
            [game.core.card :refer [get-card]]
            [game.utils :refer [same-card? to-keyword]]
            [clj-uuid :as uuid]))

(defn register-constant-effects
  [state side card]
  (when (:constant-effects (card-def card))
    (let [constant-effects (:constant-effects (card-def card))
          abilities (for [ability constant-effects]
                      (assoc
                        (select-keys ability [:type :req :value])
                        :duration :constant
                        :card card
                        :uuid (uuid/v1)))]
      (swap! state update :effects
             #(apply conj (into [] %) abilities))
      abilities)))

(defn unregister-constant-effects
  [state side card]
  (when (:constant-effects (card-def card))
    (swap! state assoc :effects
           (->> (:effects @state)
                (remove #(and (same-card? card (:card %))
                              (= :constant (:duration %))))
                (into [])))))

(defn register-floating-effect
  [state side card ability]
  (let [ability (assoc
                  (select-keys ability [:type :duration :req :value])
                  :card card
                  :uuid (uuid/v1))]
    (swap! state update :effects conj ability)
    ability))

(defn unregister-floating-effects
  [state side duration]
  (swap! state assoc :effects
         (->> (:effects @state)
              (remove #(= duration (:duration %)))
              (into []))))

(defn gather-effects
  [state side effect-type]
  (let [get-side #(-> % :card :side to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]
    (->> (:effects @state)
         (filter #(= effect-type (:type %)))
         (sort-by (complement is-active-player)))))

(defn get-effects
  "Filters and then 'executes' the effects of a given type."
  ([state side card effect-type] (get-effects state side card effect-type nil))
  ([state side card effect-type targets]
   (let [eid (make-eid state)]
     (->> (gather-effects state side effect-type)
          (filter #(if-not (:req %)
                     true
                     ((:req %) state side eid (get-card state (:card %)) (cons card targets))))
          (mapv #(if-not (fn? (:value %))
                   (:value %)
                   ((:value %) state side eid (get-card state (:card %)) (cons card targets))))))))

(defn sum-effects
  "Sums the results from get-effects."
  ([state side card effect-type] (sum-effects state side card effect-type nil))
  ([state side card effect-type targets]
   (reduce + (filter identity (get-effects state side card effect-type targets)))))
