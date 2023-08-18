(ns game.core.agendas
  (:require
    [game.core.board :refer [get-all-cards]]
    [game.core.card :refer [agenda? map->Card]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [sum-effects]]
    [game.core.eid :refer [make-eid]]
    [game.core.update :refer [update!]]))

(defn- advancement-requirement
  [state {:keys [advancementcost] :as card}]
  (when (agenda? card)
    (->> [advancementcost
          (when-let [advance-fn (:advancement-requirement (card-def card))]
            (advance-fn state :corp (make-eid state) card nil))
          (sum-effects state :corp :advancement-requirement card)]
         (reduce (fnil + 0 0))
         (max 0))))

(defn update-advancement-requirement
  "Recalculates the advancement requirement for the given agenda."
  ([state agenda] (update-advancement-requirement state nil agenda))
  ([state _ agenda]
   (let [prev-req (:current-advancement-requirement agenda)
         new-req (advancement-requirement state agenda)
         changed? (not= prev-req new-req)]
     (when changed?
       (update! state :corp (assoc agenda :current-advancement-requirement new-req)))
     changed?)))

(defn update-all-advancement-requirements
  ([state] (update-all-advancement-requirements state nil))
  ([state _]
   (reduce
     (fn [changed? agenda]
       (or (update-advancement-requirement state agenda)
           changed?))
     false
     (filter agenda? (get-all-cards state)))))

(defn agenda-points
  "Apply agenda-point modifications to calculate the number of points this card is worth
  to the given player."
  [state side card]
  (when (some? card)
    (let [base-points (:agendapoints card 0)
          points-fn (if (= side :corp)
                      (:agendapoints-corp (card-def card))
                      (:agendapoints-runner (card-def card)))]
      (if (fn? points-fn)
        (+ (points-fn state side nil card nil)
           (sum-effects state side :agenda-value card))
        (+ base-points
           (sum-effects state side :agenda-value card))))))

(defn- update-agenda-points-card
  [state side card]
  (let [prev-points (:current-points card)
        new-points (agenda-points state side card)
        changed? (not= prev-points new-points)]
    (when changed?
      (update! state side (assoc card :current-points new-points)))
    changed?))

(defn- sum-side-agenda-points
  [state side]
  (let [current-points (or (get-in @state [side :agenda-point]) 0)
        user-adjusted-points (sum-effects state side :user-agenda-points side)
        scored-points (->> (get-in @state [side :scored])
                           (keep :current-points)
                           (reduce + 0))
        total-points (+ user-adjusted-points scored-points)
        changed? (not= current-points total-points)]
    (when changed?
      (swap! state assoc-in [side :agenda-point] total-points))
    changed?))

(defn- update-side-agenda-points
  [state side]
  (let [card-points-changed?
        (reduce (fn [changed? agenda]
                  (or (update-agenda-points-card state side agenda)
                      changed?))
                false
                (get-in @state [side :scored]))]
    (or (sum-side-agenda-points state side)
        card-points-changed?)))

(defn update-all-agenda-points
  ([state] (update-all-agenda-points state nil))
  ([state _]
   (let [corp-changed? (update-side-agenda-points state :corp)
         runner-changed? (update-side-agenda-points state :runner)]
     (or corp-changed? runner-changed?))))
