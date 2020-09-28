(ns game.core.agendas
  (:require
    [game.core.card :refer [agenda? get-card]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [sum-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.events :refer [trigger-event trigger-event-sync]]
    [game.core.initializing :refer [card-init deactivate]]
    [game.core.moving :refer [move trash]]
    [game.core.resolve-ability :refer [resolve-ability]]
    [game.core.say :refer [system-msg]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [check-winner]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [to-keyword]]))

;;; Agendas
(defn get-agenda-points
  "Apply agenda-point modifications to calculate the number of points this card is worth
  to the given player."
  [state side card]
  (let [base-points (:agendapoints card 0)
        as-agenda-points (:as-agenda-points card 0)
        points-fn (if (= side :corp)
                    (:agendapoints-corp (card-def card))
                    (:agendapoints-runner (card-def card)))]
    (if (fn? points-fn)
      (points-fn state side nil card nil)
      (+ base-points
         as-agenda-points
         (sum-effects state side card :agenda-value nil)))))

(defn advancement-cost-bonus
  "Applies an advancement requirement increase of n the next agenda whose advancement requirement
  is being calculated. (SanSan City Grid.)"
  [state _ n]
  (swap! state update-in [:bonus :advancement-cost] (fnil #(+ % n) 0)))

(defn advancement-cost
  [state side {:keys [advancementcost] :as card}]
  (when (some? advancementcost)
    (-> (if-let [costfun (:advancement-cost-bonus (card-def card))]
          (+ advancementcost (costfun state side (make-eid state) card nil))
          advancementcost)
        (+ (get-in @state [:bonus :advancement-cost] 0))
        (max 0))))

(defn update-advancement-cost
  "Recalculates the advancement requirement for the given agenda."
  [state side agenda]
  (swap! state update-in [:bonus] dissoc :advancement-cost)
  (trigger-event state side :pre-advancement-cost agenda)
  (update! state side (assoc agenda :current-cost (advancement-cost state side agenda))))

(defn update-all-advancement-costs
  [state side]
  (->> (get-in @state [:corp :servers])
       seq
       flatten
       (mapcat :content)
       (filter agenda?)
       (mapv #(update-advancement-cost state side %))))

(defn update-agenda-points-card
  [state side card]
  (update! state side (assoc card :current-points (get-agenda-points state side card))))

(defn sum-agenda-points
  [state side]
  (let [user-adjusted-points (sum-effects state side side :user-agenda-points nil)
        scored-points (->> (get-in @state [side :scored])
                           (keep :current-points)
                           (reduce + 0))
        total-points (+ user-adjusted-points scored-points)]
    (swap! state assoc-in [side :agenda-point] total-points)))

(defn update-agenda-points
  [state side]
  (doseq [card (get-in @state [side :scored])]
    (update-agenda-points-card state side card))
  (sum-agenda-points state side))

(defn update-all-agenda-points
  [state _]
  (doseq [side [:corp :runner]]
    (update-agenda-points state side)))

(defn as-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points."
  ([state side card n] (as-agenda state side (make-eid state) card n nil))
  ([state side eid card n] (as-agenda state side eid card n nil))
  ([state side eid card n {:keys [register-events force]}]
   (let [card (deactivate state side card)
         card (move state side (assoc card :agendapoints n) :scored {:force force})]
     (if register-events
       (wait-for (card-init state side card {:resolve-effect false})
                 (wait-for (resolve-ability state side (make-eid state eid) (:swapped (card-def card)) card nil)
                           (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
                                     (update-all-agenda-points state side)
                                     (check-winner state side)
                                     (effect-completed state side eid))))
       (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
                 (update-all-agenda-points state side)
                 (check-winner state side)
                 (effect-completed state side eid))))))

(defn forfeit
  "Forfeits the given agenda to the :rfg zone."
  ([state side card] (forfeit state side (make-eid state) card))
  ([state side eid card] (forfeit state side eid card {:msg true}))
  ([state side eid card args]
   ;; Remove all hosted cards first
   (doseq [h (:hosted card)]
     (trash state side
            (make-eid state)
            (update-in h [:zone] #(map to-keyword %))
            {:unpreventable true :suppress-event true}))
   (let [card (get-card state card)]
     (when (:msg args)
       (system-msg state side (str "forfeits " (:title card))))
     (move state (to-keyword (:side card)) card :rfg)
     (update-all-agenda-points state side)
     (check-winner state side)
     (trigger-event-sync state side eid (keyword (str (name side) "-forfeit-agenda")) card))))

