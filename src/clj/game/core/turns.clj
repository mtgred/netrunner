(ns game.core.turns
  (:require
    [game.core.agendas :refer [update-all-advancement-requirements]]
    [game.core.action-window :refer [action-phase]]
    [game.core.board :refer [all-active all-active-installed all-installed]]
    [game.core.card :refer [facedown? get-card has-subtype? in-hand? installed?]]
    [game.core.drawing :refer [draw]]
    [game.core.effects :refer [unregister-floating-effects]]
    [game.core.eid :refer [effect-completed make-eid register-effect-completed]]
    [game.core.engine :refer [resolve-ability trigger-event trigger-event-simult unregister-floating-events]]
    [game.core.flags :refer [card-flag-fn? clear-turn-register!]]
    [game.core.gaining :refer [gain lose]]
    [game.core.hand-size :refer [hand-size]]
    [game.core.ice :refer [update-all-ice update-breaker-strength]]
    [game.core.moving :refer [move]]
    [game.core.pipeline :refer [queue-steps!]]
    [game.core.say :refer [system-msg]]
    [game.core.steps.active-step :refer [->ActiveStep]]
    [game.core.steps.phase-step :refer [->PhaseStep]]
    [game.core.steps.step :refer [complete! ->SimpleStep]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [flatline]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [abs dissoc-in quantify]]
    [clojure.string :as str]))

(defn- turn-message
  "Prints a message for the start or end of a turn, summarizing credits and cards in hand."
  [state side start-of-turn]
  (let [pre (if start-of-turn "started" "is ending")
        hand (if (= side :runner) "their Grip" "HQ")
        cards (count (get-in @state [side :hand]))
        credits (get-in @state [side :credit])
        text (str pre " their turn " (:turn @state) " with " credits " [Credit] and " cards " cards in " hand)]
    (system-msg state side text {:hr (not start-of-turn)})))

(defn end-phase-12 [& _])

(defn- handle-end-of-turn-discard
  [state side eid _]
  (let [cur-hand-size (count (get-in @state [side :hand]))
        max-hand-size (hand-size state side)]
    (cond (and (= side :runner) (neg? (hand-size state side)))
          (do (flatline state)
              (effect-completed state side eid))
          (> cur-hand-size max-hand-size)
          (continue-ability
            state side
            {:prompt (str "Discard down to " (quantify max-hand-size "card"))
             :choices {:card in-hand?
                       :max (- cur-hand-size (max (hand-size state side) 0))
                       :all true}
             :effect (req (system-msg state side
                                      (str "discards "
                                           (if (= :runner side)
                                             (str/join ", " (map :title targets))
                                             (quantify (count targets) "card"))
                                           " from " (if (= :runner side) "their Grip" "HQ")
                                           " at end of turn"))
                          (doseq [t targets]
                            (move state side t :discard))
                          (effect-completed state side eid))}
            nil nil)
          :else
          (effect-completed state side eid))))

(defn start-turn [& _])

(defn end-turn
  ([state side _] (end-turn state side (make-eid state) nil))
  ([state side eid _]
   (wait-for
     (handle-end-of-turn-discard state side nil)
     (turn-message state side false)
     (wait-for (trigger-event-simult state side (if (= side :runner) :runner-turn-ends :corp-turn-ends) nil nil)
               (trigger-event state side (if (= side :runner) :post-runner-turn-ends :post-corp-turn-ends))
               (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
               (unregister-floating-effects state side :end-of-turn)
               (unregister-floating-events state side :end-of-turn)
               (unregister-floating-effects state side :end-of-next-run)
               (unregister-floating-events state side :end-of-next-run)
               (unregister-floating-effects state side (if (= side :runner) :until-runner-turn-ends :until-corp-turn-ends))
               (unregister-floating-events state side (if (= side :runner) :until-runner-turn-ends :until-corp-turn-ends))
               (doseq [card (all-active-installed state :runner)]
                 ;; Clear :installed :this-turn as turn has ended
                 (when (= :this-turn (installed? card))
                   (update! state side (assoc (get-card state card) :installed true)))
                 ;; Remove all :turn strength from icebreakers.
                 ;; We do this even on the corp's turn in case the breaker is boosted due to Offer You Can't Refuse
                 (when (has-subtype? card "Icebreaker")
                   (update-breaker-strength state :runner (get-card state card))))
               (doseq [card (all-installed state :corp)]
                 ;; Clear :this-turn flags as turn has ended
                 (when (= :this-turn (installed? card))
                   (update! state side (assoc (get-card state card) :installed true)))
                 (when (= :this-turn (:rezzed card))
                   (update! state side (assoc (get-card state card) :rezzed true))))
               ;; Update strength of all ice every turn
               (update-all-ice state side)
               (swap! state assoc :end-turn true)
               (swap! state update-in [side :register] dissoc :cannot-draw)
               (swap! state update-in [side :register] dissoc :drawn-this-turn)
               (clear-turn-register! state)
               (when-let [extra-turns (get-in @state [side :extra-turns])]
                 (when (pos? extra-turns)
                   (start-turn state side nil)
                   (swap! state update-in [side :extra-turns] dec)
                   (system-msg state side (str/join ["will have " (quantify extra-turns "extra turn") " remaining."]))))
               (effect-completed state side eid)))))

(defn clear-new-from-installed
  [state active-player]
  (doseq [c (all-installed state active-player)
          :when (:new c)]
    (update! state active-player (dissoc c :new))))

(defn init-turn-state
  "Put new state info in :turn-state"
  [state]
  ;; Can't be inlined cuz we need state for both halves, and not an atom
  (update state assoc :turn-state (dissoc state :log :turn-state)))

(defn reset-state-for-turn
  [state]
  (let [last-turn-player (:active-player @state)
        active-player (if (= :corp last-turn-player) :runner :corp)]
    (clear-new-from-installed state active-player)
    (reset! state
            (-> @state
                (dissoc-in [:corp :undo-turn])
                (dissoc-in [:runner :undo-turn])
                (assoc-in [:corp :register] nil)
                (assoc-in [:runner :register] nil)
                (assoc :active-player active-player
                       :end-turn false
                       :per-turn nil
                       ; Don't clear :turn-events until the player clicks "Start Turn"
                       ; Fix for Hayley triggers
                       :turn-events nil)
                ; Turn counter only increments on corps turn
                (update :turn #(if (= :corp active-player) (inc %) %))
                (init-turn-state)))))

(defn gain-allotted-clicks
  [state side]
  (->SimpleStep
    (fn [_step]
      (let [default (get-in @state [side :click-per-turn])
            extra-clicks (get-in @state [side :extra-click-temp] 0)
            total (+ default extra-clicks)]
        (cond
          (pos? total) (gain state side :click total)
          (neg? total) (lose state side :click (abs total)))
        (swap! state dissoc-in [side :extra-click-temp])))))

(defn start-of-turn-paw
  [side]
  (->ActiveStep
    (fn [step state]
      (let [phase (if (= side :corp) :corp-phase-12 :runner-phase-12)
            start-cards (filter #(card-flag-fn? state side % phase true)
                                (distinct (concat (all-active state side)
                                                  (remove facedown? (all-installed state side)))))]
        (swap! state assoc phase true)
        (trigger-event state side phase nil)
        (if (empty? start-cards)
          (complete! step)
          (do (resolve-ability
                state side
                {:prompt (str "You may use " (str/join ", " (map :title start-cards))
                              (if (= side :corp)
                                " between the start of your turn and your mandatory draw."
                                " before taking your first click."))
                 :choices ["Done"]
                 :effect (req (complete! step))}
                nil nil)
              false))))))

(defn when-turn-begins-trigger
  "End phase 1.2 and trigger appropriate events for the player."
  [side]
  (->ActiveStep
    (fn [step state]
      (let [new-eid (make-eid state)]
        (register-effect-completed
          state new-eid (fn [_] (complete! step)))
        (trigger-event-simult state side new-eid (if (= side :corp) :corp-turn-begins :runner-turn-begins) nil nil)
        false))))

(defn pre-checkpoint-cleanup
  [state side]
  (->SimpleStep
    (fn [_]
      (unregister-floating-effects state side :start-of-turn)
      (unregister-floating-events state side :start-of-turn)
      (unregister-floating-effects state side (if (= side :corp) :until-corp-turn-begins :until-runner-turn-begins))
      (unregister-floating-events state side (if (= side :corp) :until-corp-turn-begins :until-runner-turn-begins)))))

(defn corp-mandatory-draw
  [side]
  (when (= side :corp)
    (->ActiveStep
      (fn [step state]
        (let [new-eid (make-eid state)]
          (register-effect-completed
            state new-eid (fn [_] (complete! step)))
          (system-msg state :corp "makes mandatory start of turn draw")
          (wait-for (draw state :corp 1 nil)
                    (trigger-event-simult state :corp new-eid :corp-mandatory-draw nil nil))
          false)))))

(defn pre-action-phase
  [state side]
  (->SimpleStep
    (fn [_]
      (swap! state dissoc (if (= side :corp) :corp-phase-12 :runner-phase-12))
      (update-all-advancement-requirements state))))

(defn start-of-turn-phase
  "* click allotment
  * PAW
  * recurring credits
  * “when your turn begins”
  * checkpoint"
  []
  (->PhaseStep
    :phase/start-of-turn
    (fn [step state]
      (let [side (:active-player @state)]
        (queue-steps!
          state
          [(gain-allotted-clicks state side)
           (start-of-turn-paw side)
           (->SimpleStep (fn [_] (turn-message state side true)))
           (when-turn-begins-trigger side)
           (pre-checkpoint-cleanup state side)
           (->SimpleStep (fn [_] (complete! step)))])
        false))))

(defn draw-phase
  "Runner doesn't have a 'draw phase' so we can skip it here"
  []
  (->PhaseStep
    :phase/draw
    (fn [step state]
      (let [side (:active-player @state)]
        (queue-steps!
          state
          [(corp-mandatory-draw side)
           (pre-action-phase state side)
           (->SimpleStep (fn [_] (complete! step)))])
        false))))

(defn discard-phase []
  (->SimpleStep
    (fn [_] true)))

(defn begin-turn
  [state]
  (reset-state-for-turn state)
  (queue-steps!
    state
    [(start-of-turn-phase)
     (draw-phase)
     (action-phase)
     ; (discard-phase)
     (->SimpleStep (fn [_] (begin-turn state)))]))
