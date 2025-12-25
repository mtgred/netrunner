(ns game.core.turns
  (:require
    [game.core.agendas :refer [update-all-advancement-requirements]]
    [game.core.board :refer [all-active all-active-installed all-installed all-installed-and-scored]]
    [game.core.card :refer [facedown? get-card has-subtype? in-hand? installed?]]
    [game.core.drawing :refer [draw]]
    [game.core.effects :refer [unregister-lingering-effects update-lingering-effect-durations any-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [checkpoint queue-event trigger-event trigger-event-simult unregister-floating-events update-floating-event-durations resolve-durations]]
    [game.core.flags :refer [card-flag-fn? clear-turn-register!]]
    [game.core.gaining :refer [gain lose]]
    [game.core.hand-size :refer [hand-size]]
    [game.core.ice :refer [update-all-ice update-breaker-strength]]
    [game.core.moving :refer [move]]
    [game.core.say :refer [system-msg]]
    [game.core.set-aside :refer [clean-set-aside!]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [flatline]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [dissoc-in enumerate-str quantify]]
    [jinteki.utils :refer [other-side]]
    [clojure.string :as string]))

(defn- turn-message
  "Prints a message for the start or end of a turn, summarizing credits and cards in hand."
  [state side start-of-turn]
  (let [pre (if start-of-turn "started" "is ending")
        hand (if (= side :runner) "[their] Grip" "HQ")
        cards (count (get-in @state [side :hand]))
        credits (get-in @state [side :credit])
        text (str pre " [their] turn " (:turn @state) " with " credits " [Credit] and " (quantify cards "card") " in " hand)]
    (system-msg state side text {:hr (not start-of-turn)})))

(defn end-phase-12
  "End phase 1.2 and trigger appropriate events for the player."
  ([state side _] (end-phase-12 state side (make-eid state) nil))
  ([state side eid _]
   (when (if (= side :corp) (:corp-phase-12 @state) (:runner-phase-12 @state))
     (turn-message state side true)
     (wait-for (trigger-event-simult state side (if (= side :corp) :corp-turn-begins :runner-turn-begins) nil nil)
               (resolve-durations state side :start-of-turn (if (= side :corp) :until-corp-turn-begins :until-runner-turn-begins))
               (if (= side :corp)
                 (do (update-lingering-effect-durations state side :until-next-corp-turn-begins :until-corp-turn-begins)
                     (update-floating-event-durations state side :until-next-corp-turn-begins :until-corp-turn-begins))
                 (do (update-lingering-effect-durations state side :until-next-runner-turn-begins :until-runner-turn-begins)
                     (update-floating-event-durations state side :until-next-runner-turn-begins :until-runner-turn-begins)))
               (if (= side :corp)
                 (do (system-msg state side "makes [their] mandatory start of turn draw")
                     (wait-for (draw state side 1 nil)
                               (trigger-event-simult state side eid :corp-mandatory-draw nil nil)))
                 (effect-completed state nil eid))
               (swap! state dissoc (if (= side :corp) :corp-phase-12 :runner-phase-12))
               (wait-for (trigger-event-simult state side (if (= side :corp) :post-corp-turn-begins :post-runner-turn-begins) nil nil)
                         (when (= side :corp)
                           (update-all-advancement-requirements state))
                         (effect-completed state side eid))))))

(defn phase-12-pass-priority
  ([state side _] (phase-12-pass-priority state side (make-eid state) nil))
  ([state side eid _]
   (cond
     (:corp-phase-12 @state)
     (do (swap! state assoc-in [:corp-phase-12 side] true)
         (if (and (get-in @state [:corp-phase-12 :corp])
                  (get-in @state [:corp-phase-12 :runner]))
           (end-phase-12 state :corp eid _)
           (do (system-msg state side "has no further action")
               (effect-completed state side eid))))
     (:runner-phase-12 @state)
     (do (swap! state assoc-in [:runner-phase-12 side] true)
         (if (and (get-in @state [:runner-phase-12 :corp])
                  (get-in @state [:runner-phase-12 :runner]))
           (end-phase-12 state :runner eid _)
           (do (system-msg state side "has no further action")
               (effect-completed state side eid))))
     :else nil)))

(defn start-turn
  "Start turn."
  [state side _]
  ;; note that it's possible for the front-end to send the "start-turn" command twice,
  ;; before it can be updated with the fact that the turn has started.
  (when-not (get-in @state [side :turn-started])
    ;; Don't clear :turn-events until the player clicks "Start Turn"
    ;; Fix for Hayley triggers
    (swap! state assoc :turn-events nil)
    (swap! state assoc-in [side :turn-started] true)
    ;; clear out last-revealed so cards don't stick around all game
    (swap! state assoc :last-revealed [])

    ;; Functions to set up state for undo-turn functionality
    (doseq [s [:runner :corp]] (swap! state dissoc-in [s :undo-turn]))
    (swap! state assoc :click-states [])
    (swap! state dissoc :paid-ability-state)
    (swap! state assoc :turn-state (dissoc @state :log :history :turn-state))

    (when (= side :corp)
      (swap! state update-in [:turn] inc))

    (doseq [c (filter :new (concat (all-installed-and-scored state side) (get-in @state [side :discard])))]
      (update! state side (dissoc c :new)))

    (swap! state assoc :active-player side :per-turn nil :end-turn false)
    (doseq [s [:runner :corp]]
      (swap! state assoc-in [s :register] nil))

    (let [phase (if (= side :corp) :corp-phase-12 :runner-phase-12)
          start-cards (filter #(card-flag-fn? state side % phase true)
                              (distinct (concat (all-active state side)
                                                (remove facedown? (all-installed state side)))))
          extra-clicks (get-in @state [side :extra-click-temp] 0)]
      (gain state side :click (get-in @state [side :click-per-turn]))
      (cond
        (neg? extra-clicks) (lose state side :click (abs extra-clicks))
        (pos? extra-clicks) (gain state side :click extra-clicks))
      (swap! state dissoc-in [side :extra-click-temp])
      (swap! state assoc phase {:active true})
      (trigger-event state side phase nil)
      (cond
        (get-in @state [(other-side side) :properties :force-phase-12-opponent])
        (do (toast state side
                   (str "players may use abilities "
                        (if (= side :corp)
                          " between the start of your turn and your mandatory draw"
                          " before you can take your first click"))
                   "info")
            (swap! state assoc-in [phase :requires-consent] true))
        (or (get-in @state [side :properties :force-phase-12-self]) (not-empty start-cards))
        (toast state side
               (str "You may use " (enumerate-str (map :title start-cards))
                    (if (= side :corp)
                      " between the start of your turn and your mandatory draw."
                      " before taking your first click."))
               "info")
        :else (end-phase-12 state side _)))))

(defn- handle-end-of-turn-discard
  [state side eid _]
  (let [cur-hand-size (count (get-in @state [side :hand]))
        max-hand-size (hand-size state side)]
    (cond (and (= side :runner) (neg? (hand-size state side)))
          (do (flatline state)
              (effect-completed state side eid))
          (any-effects state side :skip-discard)
          (do
            (system-msg state side (str "skips [their] discard step this turn"))
            (effect-completed state side eid))
          (> cur-hand-size max-hand-size)
          (continue-ability
            state side
            {:prompt (str "Discard down to " (quantify max-hand-size "card"))
             :choices {:card in-hand?
                       :max (- cur-hand-size (max (hand-size state side) 0))
                       :all true}
             :waiting-prompt true
             :async true
             :effect (req (system-msg state side
                                      (str "discards "
                                           (if (= :runner side)
                                             (enumerate-str (map :title targets))
                                             (quantify (count targets) "card"))
                                           " from " (if (= :runner side) "[their] Grip" "HQ")
                                           " at end of turn"))
                          (let [discard (seq (map #(move state side % :discard) targets))
                                ev (if (= side :runner) :runner-discard-to-hand-size :corp-discard-to-hand-size)]
                            (queue-event state ev {:cards discard})
                            (checkpoint state nil eid {:durations [ev]})))}
            nil nil)
          :else
          (effect-completed state side eid))))

(defn end-turn-continue
  ([state side _] (end-turn-continue state side (make-eid state) nil))
  ([state side eid _]
   (when (if (= :corp side) (:corp-post-discard @state) (:runner-post-discard @state))
     (swap! state dissoc :corp-post-discard :runner-post-discard)
     (turn-message state side false)
     (wait-for (trigger-event-simult state side (if (= side :runner) :runner-turn-ends :corp-turn-ends) nil nil)
               (trigger-event state side (if (= side :runner) :post-runner-turn-ends :post-corp-turn-ends))
               (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
               (resolve-durations state side :end-of-turn :end-of-next-run :end-of-run :end-of-encounter (if (= side :runner) :until-runner-turn-ends :until-corp-turn-ends))
               (if (= side :corp)
                 (do (update-lingering-effect-durations state side :until-next-corp-turn-ends :until-corp-turn-ends)
                     (update-floating-event-durations state side :until-next-corp-turn-ends :until-corp-turn-ends))
                 (do (update-lingering-effect-durations state side :until-next-runner-turn-ends :until-runner-turn-ends)
                     (update-floating-event-durations state side :until-next-runner-turn-ends :until-runner-turn-ends)))
               (swap! state assoc :end-turn true)
               (clean-set-aside! state side)
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
               (swap! state dissoc-in [side :register :cannot-draw])
               (swap! state dissoc-in [side :register :drawn-this-turn])
               (swap! state dissoc-in [side :turn-started])
               (swap! state assoc :mark nil)
               (clear-turn-register! state)
               (when-let [extra-turns (get-in @state [side :extra-turns])]
                 (when (pos? extra-turns)
                   (start-turn state side nil)
                   (swap! state update-in [side :extra-turns] dec)
                   (system-msg state side (string/join ["will have " (quantify extra-turns "extra turn") " remaining."]))))
               (effect-completed state side eid)))))

(defn post-discard-pass-priority
  ([state side _] (post-discard-pass-priority state side (make-eid state) nil))
  ([state side eid _]
   (cond
     (:corp-post-discard @state)
     (do (swap! state assoc-in [:corp-post-discard side] true)
         (if (and (get-in @state [:corp-post-discard :corp])
                  (get-in @state [:corp-post-discard :runner]))
           (end-turn-continue state :corp eid _)
           (do (system-msg state side "has no further action")
               (effect-completed state side eid))))
     (:runner-post-discard @state)
     (do (swap! state assoc-in [:runner-post-discard side] true)
         (if (and (get-in @state [:runner-post-discard :corp])
                  (get-in @state [:runner-post-discard :runner]))
           (end-turn-continue state :runner eid _)
           (do (system-msg state side "has no further action")
               (effect-completed state side eid))))
     :else nil)))

(defn end-turn
  ([state side _] (end-turn state side (make-eid state) nil))
  ([state side eid _]
   (wait-for
     (trigger-event-simult state side (if (= side :runner) :runner-action-phase-ends :corp-action-phase-ends) nil nil)
     (swap! state dissoc :paid-ability-state)
     (wait-for
       (handle-end-of-turn-discard state side nil)
       (let [phase (if (= side :corp) :corp-post-discard :runner-post-discard)]
         (swap! state assoc phase {:active true})
         (cond
           (get-in @state [(other-side side) :properties :force-post-discard-opponent])
           (do (toast state side
                      "players may use abilities between the discard phase and the turn ends phase"
                      "info")
               (swap! state assoc-in [phase :requires-consent] true))
           (get-in @state [side :properties :force-post-discard-self])
           (toast state side
                  "players may use abilities between the discard phase and the turn ends phase"
                  "info")
           :else (end-turn-continue state side eid _)))))))
