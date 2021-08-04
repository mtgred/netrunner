(ns game.core.action-window
  (:require
   [game.core.card :refer [basic? corp? runner?]]
   [game.core.cost-fns :refer [card-ability-cost]]
   [game.core.eid :refer [make-eid register-effect-completed]]
   [game.core.engine :refer [can-trigger? dissoc-req resolve-ability]]
   [game.core.payment :refer [can-pay?]]
   [game.core.pipeline :refer [queue-step!]]
   [game.core.steps.active-step :refer [->ActiveStep]]
   [game.core.steps.phase-step :refer [->PhaseStep]]
   [game.core.steps.step :refer [complete!]]
   [game.macros :refer [continue-ability effect]]
   [jinteki.utils :refer [add-cost-to-label]]))

(defn make-action-eid
  [card idx]
  {:source card
   :source-type :ability
   :source-info {:ability-idx idx}
   :resolving-action true})

(defn allowed-basic-actions
  "Returns true if card is not a basic action card or the chosen ability is not advance/install/play"
  [card idx]
  (or (not (basic? card))
      (and (corp? card)
           (#{0 1 5 6} idx))
      (and (runner? card)
           (#{0 1 4 5} idx))))

(defn label-ability
  [state side card idx ability]
  (when (and (:action ability)
             (allowed-basic-actions card idx))
    (let [eid (make-action-eid card idx)
          cost (card-ability-cost state side ability card)]
      (when (and (can-pay? state side eid card nil cost)
                 (can-trigger? state side eid ability card))
        {:title (add-cost-to-label ability)
         :ability (-> ability
                      (dissoc-req)
                      (assoc :cost cost))
         :card card}))))

(defn generate-action-list
  [state side]
  (let [card (get-in @state [side :basic-action-card])]
    (keep-indexed (partial label-ability state side card) (:abilities card))))

(defn action-window-step []
  (->ActiveStep
    (fn [step state]
      (let [active-player (:active-player @state)
            clicks-left (get-in @state [active-player :click])
            new-eid (make-eid state)]
        (register-effect-completed
          state new-eid (fn [_] (complete! step)))
        (resolve-ability
          state active-player
          new-eid
          {:prompt (str "You have " clicks-left " [Click] left.")
           :choices (fn [& _] (generate-action-list state active-player))
           :async true
           :effect (effect (continue-ability (:ability context) (:card context) nil))}
          nil nil)
        false))))

(defn action-phase []
  (->PhaseStep
    :phase/action
    (fn [step state]
      (let [active-player (:active-player @state)
            clicks-left (get-in @state [active-player :click])]
        ;; if there's clicks left and we're not in a terminal,
        ;; load up another action window
        (if (and (pos? clicks-left)
                 (not (get-in @state [active-player :register :terminal])))
          (do (queue-step! state (action-window-step))
              false)
          (complete! step))))))
