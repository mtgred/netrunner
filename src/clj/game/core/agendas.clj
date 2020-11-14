(ns game.core.agendas
  (:require
    [game.core.board :refer [all-installed get-all-cards]]
    [game.core.card :refer [agenda?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [sum-effects]]
    [game.core.eid :refer [make-eid]]
    [game.core.update :refer [update!]]))

(defn get-advancement-requirement
  [card]
  (when (agenda? card)
    (or (:current-advancement-requirement card)
        (:advancementcost card))))

(defn- advancement-requirement
  [state {:keys [advancementcost] :as card}]
  (when (agenda? card)
    (->> [advancementcost
          (when-let [advance-fn (:advancement-requirement (card-def card))]
            (advance-fn state :corp (make-eid state) card nil))
          (sum-effects state :corp card :advancement-requirement)]
         (reduce (fnil + 0 0))
         (max 0))))

(defn update-advancement-requirement
  "Recalculates the advancement requirement for the given agenda."
  ([state agenda] (update-advancement-requirement state nil agenda))
  ([state _ agenda]
   (let [new-req (advancement-requirement state agenda)
         agenda (assoc agenda :current-advancement-requirement new-req)]
     (update! state :corp agenda))))

(defn update-all-advancement-requirements
  ([state] (update-all-advancement-requirements state nil))
  ([state _]
   (doseq [agenda (filter agenda? (get-all-cards state))]
     (update-advancement-requirement state agenda))))

(defn get-agenda-points
  [card]
  (or (:agendapoints card)
      (:current-points card)
      0))

(defn- agenda-points
  "Apply agenda-point modifications to calculate the number of points this card is worth
  to the given player."
  [state side card]
  (when (some? card)
    (let [base-points (:agendapoints card 0)
          as-agenda-points (:as-agenda-points card 0)
          points-fn (if (= side :corp)
                      (:agendapoints-corp (card-def card))
                      (:agendapoints-runner (card-def card)))]
      (if (fn? points-fn)
        (points-fn state side nil card nil)
        (+ base-points
           as-agenda-points
           (sum-effects state side card :agenda-value nil))))))

(defn- update-agenda-points-card
  [state side card]
  (update! state side (assoc card :current-points (agenda-points state side card))))

(defn- sum-side-agenda-points
  [state side]
  (let [user-adjusted-points (sum-effects state side side :user-agenda-points nil)
        scored-points (->> (get-in @state [side :scored])
                           (keep :current-points)
                           (reduce + 0))
        total-points (+ user-adjusted-points scored-points)]
    (swap! state assoc-in [side :agenda-point] total-points)))

(defn- update-side-agenda-points
  [state side]
  (doseq [card (get-in @state [side :scored])]
    (update-agenda-points-card state side card))
  (sum-side-agenda-points state side))

(defn update-all-agenda-points
  ([state] (update-all-agenda-points state nil))
  ([state _]
   (doseq [side [:corp :runner]]
     (update-side-agenda-points state side))))
