(ns game.core.effects
  (:require [clj-uuid :as uuid]
            [game.core.card :refer [get-card]]
            [game.core.card-defs :refer [card-def]]
            [game.core.eid :refer [make-eid]]
            [game.utils :refer [same-card? to-keyword swap!*]]))

(defn register-static-abilities
  [state _ card]
  (when (:static-abilities (card-def card))
    (let [static-abilities (:static-abilities (card-def card))
          abilities (for [ability static-abilities]
                      (assoc
                        (select-keys ability [:type :req :value])
                        :duration :while-active
                        :card card
                        :uuid (uuid/v1)))]
      (swap!* state update :effects
             #(apply conj (into [] %) abilities))
      abilities)))

(defn unregister-static-abilities
  [state _ card]
  (swap!* state assoc :effects
         (->> (:effects @state)
              (remove #(and (same-card? card (:card %))
                            (= :while-active (:duration %))))
              (into []))))

(defn register-lingering-effect
  [state _ card ability]
  (let [ability (assoc
                  (select-keys ability [:type :duration :req :value])
                  :card card
                  :uuid (uuid/v1))]
    (swap!* state update :effects conj ability)
    ability))

(defn unregister-lingering-effects
  [state _ duration]
  (swap!* state assoc :effects
         (->> (:effects @state)
              (remove #(= duration (:duration %)))
              (into []))))

(defn unregister-effects-for-card
  ([state _ card] (unregister-effects-for-card state nil card identity))
  ([state _ card pred]
   (swap!* state assoc :effects
          (->> (:effects @state)
               (remove #(and (same-card? card (:card %))
                             (pred %)))
               (into [])))))

(defn gather-effects
  [state _ effect-type]
  (let [get-side #(-> % :card :side to-keyword)
        is-active-player #(= (:active-player @state) (get-side %))]
    (->> (:effects @state)
         (filter #(= effect-type (:type %)))
         (sort-by (complement is-active-player))
         (into []))))

(defn update-effect-card
  "Updates the effect map's :card with the result of `get-card`."
  [state ability]
  (update ability :card #(get-card state %)))

(defn effect-pred
  "Returns a function that returns the boolean result of the :req on the effect map.
  If the :req is an fn, it is called with the given state, side, eid, and targets,
  and the effect map's card. Otherwise, return true."
  [state side eid targets]
  (fn [{:keys [req card]}]
    (if (fn? req)
      (boolean (req state side eid card targets))
      true)))

(defn get-effect-maps
  "Returns the filtered effects for a given effect type. Updates the :card before
  filtering, so the :card might be nil."
  ([state side effect-type] (get-effect-maps state side (make-eid state) effect-type nil))
  ([state side eid effect-type] (get-effect-maps state side eid effect-type nil))
  ([state side eid effect-type targets]
   (->> (gather-effects state side effect-type)
        (map #(update-effect-card state %))
        (filterv (effect-pred state side eid targets)))))

(defn get-effect-value
  "Returns a function that returns the value of a given effect. If the :value is an fn,
  it is called with the given state, side, eid, and targets. Otherwise, return the raw
  value."
  ([state side] (get-effect-value state side (make-eid state) nil))
  ([state side eid] (get-effect-value state side eid nil))
  ([state side eid targets]
   (fn [{:keys [value card]}]
     (if (fn? value)
       (value state side eid card targets)
       value))))

(defn get-effects
  "Filters and then 'executes' the effects of a given type."
  ([state side effect-type] (get-effects state side effect-type nil nil))
  ([state side effect-type target] (get-effects state side effect-type target nil))
  ([state side effect-type target targets]
   (let [eid (make-eid state)
         targets (cons target targets)]
     (->> (get-effect-maps state side eid effect-type targets)
          (mapv (get-effect-value state side eid targets))))))

(defn sum-effects
  "Sums the results from get-effects."
  ([state side effect-type] (sum-effects state side effect-type nil nil))
  ([state side effect-type target] (sum-effects state side effect-type target nil))
  ([state side effect-type target targets]
   (->> (get-effects state side effect-type target targets)
        (filter number?)
        (reduce + 0))))

(defn any-effects
  "Check if any effects return true for pred"
  ([state side effect-type] (any-effects state side effect-type true? nil nil))
  ([state side effect-type pred] (any-effects state side effect-type pred nil nil))
  ([state side effect-type pred target] (any-effects state side effect-type pred target nil))
  ([state side effect-type pred target targets]
   (some pred (get-effects state side effect-type target targets))))
