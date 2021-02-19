(ns game.core.subtypes
  (:require [medley.core :refer [find-first]]
            [game.core.board :refer [get-all-cards]]
            [game.core.card :refer [get-card]]
            [game.core.effects :refer [get-effects]]
            [game.core.update :refer [update!]]
            [game.utils :refer [remove-once server-card]]
            [jinteki.utils :refer [capitalize]]))

(defn has-subtypes?
  "Checks if the specified subtype is present in the card, ignoring case."
  [card subtype]
  (find-first #(= % subtype) (:subtypes card)))

(defn add-subtype
  [card subtype]
  (assert (or (string? subtype) (keyword? subtype)) "Given subtype must be a string")
  (let [subtype (capitalize (if (keyword? subtype) (name subtype) subtype))]
    (update card :subtypes #(into [] (conj % subtype)))))

(defn add-subtype!
  [state side card subtype]
  (update! state side (add-subtype card subtype)))

(defn remove-subtype
  [card subtype]
  (assert (or (string? subtype) (keyword? subtype)) "Given subtype must be a string")
  (let [subtype (if (keyword? subtype) (name subtype) subtype)]
    (update card :subtypes #(remove-once (fn [s] (= s subtype)) %))))

(defn remove-subtype!
  [state side card subtype]
  (update! state side (remove-subtype card subtype)))

(defn subtypes-for-card
  [state card]
  (let [printed-subtypes (:subtypes (server-card (:title card)))
        subtype-effects (get-effects state nil card :subtypes)]
    (println subtype-effects)
    (into [] (concat printed-subtypes subtype-effects))))

(defn update-subtypes-for-card
  [state side card]
  (let [card (get-card state card)
        old-subtypes (:subtypes card)
        new-subtypes (subtypes-for-card state card)
        changed? (not= old-subtypes new-subtypes)]
    (update! state side (assoc card :subtypes new-subtypes))
    changed?))

(defn update-all-subtypes
  ([state] (update-all-subtypes state nil))
  ([state _]
   (reduce
     (fn [changed? card]
       (or (update-subtypes-for-card state card)
           changed?))
     false
     (get-all-cards state))))
