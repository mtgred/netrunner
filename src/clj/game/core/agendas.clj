(ns game.core.agendas
  (:require
    [game.core.card :refer [agenda? get-card]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [sum-effects]]
    [game.core.eid :refer [effect-completed make-eid eid-set-defaults]]
    [game.core.events :refer [card-as-handler trigger-event trigger-event-sync trigger-event-simult]]
    [game.core.flags :refer [can-advance? can-score?]]
    [game.core.initializing :refer [card-init deactivate]]
    [game.core.moving :refer [move remove-old-current trash]]
    [game.core.payment :refer [build-spend-msg pay]]
    [game.core.props :refer [add-prop set-prop]]
    [game.core.resolve-ability :refer [resolve-ability]]
    [game.core.say :refer [play-sfx system-msg]]
    [game.core.to-string :refer [card-str]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [check-winner]]
    [game.macros :refer [req wait-for]]
    [game.utils :refer [dissoc-in quantify to-keyword]]))

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

(defn advance
  "Advance a corp card that can be advanced.
   If you pass in a truthy value as the no-cost parameter, it will advance at no cost (for the card Success)."
  ([state side {:keys [card]}] (advance state side (make-eid state) card nil))
  ([state side card no-cost] (advance state side (make-eid state) card no-cost))
  ([state side eid card no-cost]
   (let [card (get-card state card)
         eid (eid-set-defaults eid :source nil :source-type :advance)]
     (when (can-advance? state side card)
       (wait-for (pay state side (make-eid state eid) card :click (if-not no-cost 1 0) :credit (if-not no-cost 1 0) {:action :corp-advance})
                 (when-let [cost-str async-result]
                   (let [spent   (build-spend-msg cost-str "advance")
                         card    (card-str state card)
                         message (str spent card)]
                     (system-msg state side message))
                   (update-advancement-cost state side card)
                   (add-prop state side (get-card state card) :advance-counter 1)
                   (play-sfx state side "click-advance")))))))

(defn score
  "Score an agenda. It trusts the card data passed to it."
  ([state side args] (score state side (make-eid state) args))
  ([state side eid args]
   (let [card (or (:card args) args)]
     (wait-for (trigger-event-simult state :corp :pre-agenda-scored nil card)
               (if (can-score? state side card)
                 ;; do not card-init necessarily. if card-def has :effect, wrap a fake event
                 (let [moved-card (move state :corp card :scored)
                       c (card-init state :corp moved-card {:resolve-effect false
                                                            :init-data true})
                       points (get-agenda-points state :corp c)]
                   (system-msg state :corp (str "scores " (:title c) " and gains " (quantify points "agenda point")))
                   (trigger-event-simult
                     state :corp eid :agenda-scored
                     {:first-ability {:async true
                                      :effect (req (when-let [current (first (get-in @state [:runner :current]))]
                                                     ;; This is to handle Employee Strike with damage IDs #2688
                                                     (when (:disable-id (card-def current))
                                                       (swap! state assoc-in [:corp :disable-id] true)))
                                                   (remove-old-current state side eid :runner))}
                      :card-abilities (card-as-handler c)
                      :after-active-player
                      {:effect (req (let [c (get-card state c)
                                          points (or (get-agenda-points state :corp c) points)]
                                      (set-prop state :corp (get-card state moved-card) :advance-counter 0)
                                      (swap! state update-in [:corp :register :scored-agenda] #(+ (or % 0) points))
                                      (swap! state dissoc-in [:corp :disable-id])
                                      (update-all-agenda-points state side)
                                      (check-winner state side)
                                      (play-sfx state side "agenda-score")))}}
                     c))
                 (effect-completed state side eid))))))
