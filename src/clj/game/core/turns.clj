(ns game.core.turns
  (:require
    [game.core.agendas :refer [update-all-advancement-requirements]]
    [game.core.board :refer [all-active all-active-installed all-installed all-installed-and-scored]]
    [game.core.card :refer [facedown? get-card has-subtype? in-hand? installed?]]
    [game.core.drawing :refer [draw]]
    [game.core.effects :refer [unregister-lingering-effects any-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [trigger-event trigger-event-simult unregister-floating-events]]
    [game.core.flags :refer [card-flag-fn? clear-turn-register!]]
    [game.core.gaining :refer [gain lose]]
    [game.core.hand-size :refer [hand-size]]
    [game.core.ice :refer [update-all-ice update-breaker-strength]]
    [game.core.moving :refer [move]]
    [game.core.say :refer [system-msg]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [flatline]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [dissoc-in enumerate-str quantify]]
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
   (turn-message state side true)
   (wait-for (trigger-event-simult state side (if (= side :corp) :corp-turn-begins :runner-turn-begins) nil nil)
             (unregister-lingering-effects state side :start-of-turn)
             (unregister-floating-events state side :start-of-turn)
             (unregister-lingering-effects state side (if (= side :corp) :until-corp-turn-begins :until-runner-turn-begins))
             (unregister-floating-events state side (if (= side :corp) :until-corp-turn-begins :until-runner-turn-begins))
             (if (= side :corp)
               (do (system-msg state side "makes [their] mandatory start of turn draw")
                   (wait-for (draw state side 1 nil)
                             (trigger-event-simult state side eid :corp-mandatory-draw nil nil)))
               (effect-completed state nil eid))
             (swap! state dissoc (if (= side :corp) :corp-phase-12 :runner-phase-12))
             (when (= side :corp)
               (update-all-advancement-requirements state)))))

(defn start-turn
  "Start turn."
  [state side _]
  ; Don't clear :turn-events until the player clicks "Start Turn"
  ; Fix for Hayley triggers
  (swap! state assoc :turn-events nil)

  ; Functions to set up state for undo-turn functionality
  (doseq [s [:runner :corp]] (swap! state dissoc-in [s :undo-turn]))
  (swap! state assoc :click-states [])
  (swap! state assoc :turn-state (dissoc @state :log :turn-state))

  (when (= side :corp)
    (swap! state update-in [:turn] inc))

  (doseq [c (filter :new (all-installed-and-scored state side))]
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
    (swap! state assoc phase true)
    (trigger-event state side phase nil)
    (if (not-empty start-cards)
      (toast state side
             (str "You may use " (enumerate-str (map :title start-cards))
                  (if (= side :corp)
                    " between the start of your turn and your mandatory draw."
                    " before taking your first click."))
             "info")
      (end-phase-12 state side _))))

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
             :effect (req (system-msg state side
                                      (str "discards "
                                           (if (= :runner side)
                                             (enumerate-str (map :title targets))
                                             (quantify (count targets) "card"))
                                           " from " (if (= :runner side) "[their] Grip" "HQ")
                                           " at end of turn"))
                          (doseq [t targets]
                            (move state side t :discard))
                          (effect-completed state side eid))}
            nil nil)
          :else
          (effect-completed state side eid))))

(defn end-turn
  ([state side _] (end-turn state side (make-eid state) nil))
  ([state side eid _]
   (wait-for
     (handle-end-of-turn-discard state side nil)
     (turn-message state side false)
     (wait-for (trigger-event-simult state side (if (= side :runner) :runner-turn-ends :corp-turn-ends) nil nil)
               (trigger-event state side (if (= side :runner) :post-runner-turn-ends :post-corp-turn-ends))
               (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
               (unregister-lingering-effects state side :end-of-turn)
               (unregister-floating-events state side :end-of-turn)
               (unregister-lingering-effects state side :end-of-next-run)
               (unregister-floating-events state side :end-of-next-run)
               (unregister-lingering-effects state side (if (= side :runner) :until-runner-turn-ends :until-corp-turn-ends))
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
               (swap! state assoc :mark nil)
               (clear-turn-register! state)
               (when-let [extra-turns (get-in @state [side :extra-turns])]
                 (when (pos? extra-turns)
                   (start-turn state side nil)
                   (swap! state update-in [side :extra-turns] dec)
                   (system-msg state side (string/join ["will have " (quantify extra-turns "extra turn") " remaining."]))))
               (effect-completed state side eid)))))
