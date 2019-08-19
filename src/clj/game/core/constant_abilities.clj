(ns game.core.constant-abilities
  (:require [game.core.card-defs :refer [card-def]]
            [game.core.eid :refer [make-eid]]
            [game.utils :refer [same-card? to-keyword]]))

(defn register-constant-abilities
  [state card]
  (when-let [abilities (:constant-abilities (card-def card))]
    (swap! state update :constant-abilities
           #(apply conj % (for [ability abilities]
                            {
                             :type (:type ability)
                             :duration :persistent
                             :req (:req ability)
                             :effect (:effect ability)
                             :card card
                             }))))
  (:constant-abilities @state))

(defn create-floating-constant-ability
  [state card ability]
  (swap! state update :constant-abilities
         conj (assoc
                (select-keys ability [:type :duration :req :effect])
                :card card))
  (:constant-abilities @state))

(defn remove-floating-constant-ability
  [state duration]
  (swap! state assoc :constant-abilities
         (->> (:constant-abilities @state)
              (remove #(= duration (:duration %)))
              (into []))))

(defn unregister-constant-abilities
  [state card]
  (when (:constant-abilities (card-def card))
    (swap! state assoc :constant-abilities
           (->> (:constant-abilities @state)
                (remove #(same-card? (:card %) card))
                (into [])))))

(defn gather-constant-abilities
  [state side effect-type]
  (let [get-side #(-> % :card :side to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]
    (->> (:constant-abilities @state)
         (filter #(= effect-type (:type %)))
         (sort-by (complement is-active-player))
         (into []))))

(defn sum-constant-abilities
  "Filters and then calls and then sums the constant abilities of a given type.
  This doesn't call gather-constant-abilities directly as we don't want to call
  it repeatedly; the calling function should call it and pass the mods to this."
  [state side mods card]
  (let [eid (make-eid state)]
    (->> mods
         (filter #(if-not (:req %)
                    true
                    ((:req %) state side eid (:card %) [card])))
         (map #((:effect %) state side eid (:card %) [card]))
         (reduce +))))
