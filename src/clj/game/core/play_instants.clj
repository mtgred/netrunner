(ns game.core.play-instants
  (:require
    [game.core.card :refer [get-card has-subtype?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [play-additional-cost-bonus play-cost]]
    [game.core.effects :refer [unregister-constant-effects]]
    [game.core.eid :refer [effect-completed eid-set-defaults make-eid make-result]]
    [game.core.engine :refer [pay resolve-ability should-trigger? trigger-event-sync unregister-events]]
    [game.core.flags :refer [can-run?]]
    [game.core.gaining :refer [lose]]
    [game.core.initializing :refer [card-init]]
    [game.core.moving :refer [move remove-old-current trash]]
    [game.core.payment :refer [build-spend-msg merge-costs]]
    [game.core.say :refer [play-sfx system-msg]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [same-card?]]))

(defn- fake-move
  [state side eid c _]
  (move state side c :rfg)
  (effect-completed state side eid))

(defn- current-handler
  [state side eid card]
  (if (has-subtype? card "Current")
    (wait-for (remove-old-current state side :corp)
              (wait-for (remove-old-current state side :runner)
                        (let [c (some #(when (same-card? % card) %) (get-in @state [side :play-area]))
                              c (move state side c :current)]
                          (effect-completed state side (make-result eid c)))))
    (effect-completed state side (make-result eid card))))

;;; Playing cards.
(defn- complete-play-instant
  "Completes the play of the event / operation that the player can play for"
  [state side eid {:keys [title] :as card} payment-str ignore-cost]
  (let [play-msg (if ignore-cost
                   "play "
                   (build-spend-msg payment-str "play"))]
    (system-msg state side (str play-msg title (when ignore-cost " at no cost")))
    (play-sfx state side "play-instant")
    ;; Need to await trashing the existing current
    (wait-for
      (current-handler state side card)
      ;; Select the "on the table" version of the card
      (let [card async-result
            cdef (card-def card)]
        (when card
          (card-init state side (if (:rfg-instead-of-trashing cdef)
                                  (assoc card :rfg-instead-of-trashing true)
                                  card)
                     {:resolve-effect false :init-data true}))
        (let [card (get-card state card)]
          (wait-for (trigger-event-sync state side (if (= side :corp) :play-operation :play-event) card)
                    ;; Resolve ability, removing :req as that has already been checked
                    (wait-for (resolve-ability state side (dissoc cdef :req :cost :additional-cost) card nil)
                              (let [c (some #(when (same-card? card %) %) (get-in @state [side :play-area]))
                                    trash-after-resolving (:trash-after-resolving cdef true)
                                    zone (if (:rfg-instead-of-trashing c) :rfg :discard)]
                                (if (and c trash-after-resolving)
                                  (let [trash-or-move (if (= zone :rfg) fake-move trash)]
                                    (wait-for (trash-or-move state side c {:unpreventable true})
                                              (unregister-events state side card)
                                              (unregister-constant-effects state side card)
                                              (when (= zone :rfg)
                                                (system-msg state side
                                                            (str "removes " (:title c) " from the game instead of trashing it")))
                                              (when (has-subtype? card "Terminal")
                                                (lose state side :click (-> @state side :click))
                                                (swap! state assoc-in [:corp :register :terminal] true))
                                              (effect-completed state side eid)))
                                  (do (when (has-subtype? card "Terminal")
                                        (lose state side :click (-> @state side :click))
                                        (swap! state assoc-in [:corp :register :terminal] true))
                                      (effect-completed state side eid)))))))))))

(defn play-instant
  "Plays an Event or Operation."
  ([state side eid card {:keys [targets ignore-cost base-cost no-additional-cost]}]
   (let [eid (eid-set-defaults eid :source nil :source-type :play)
         cdef (card-def card)
         cost (play-cost state side card)
         additional-costs (play-additional-cost-bonus state side card)
         costs (merge-costs
                 [(when-not ignore-cost
                    [base-cost [:credit cost]])
                  (when (and (has-subtype? card "Triple")
                             (not no-additional-cost))
                    [:click 2])
                  (when (and (has-subtype? card "Double")
                             (not no-additional-cost)
                             (not (get-in @state [side :register :double-ignore-additional])))
                    [:click 1])
                  (when-not (and no-additional-cost ignore-cost)
                    [additional-costs])])
         eid (if-not eid (make-eid state) eid)]
     ;; ensure the instant can be played
     (if (and ;; req is satisfied
              (should-trigger? state side eid card targets cdef)
              ;; The zone isn't locked
              (empty? (get-in @state [side :locked (-> card :zone first)]))
              ;; This is a current, and currents can be played
              (not (and (has-subtype? card "Current")
                        (get-in @state [side :register :cannot-play-current])))
              ;; This is a run event or makes a run, and running is allowed
              (not (and (or (:makes-run cdef)
                            (has-subtype? card "Run"))
                        (not (can-run? state :runner))))
              ;; if priority, have not spent a click
              (not (and (has-subtype? card "Priority")
                        (get-in @state [side :register :spent-click]))))
       ;; Wait on pay to finish before triggering instant-effect
       (let [original-zone (:zone card)
             moved-card (move state side (assoc card :seen true) :play-area)]
         ;; Only mark the register once costs have been paid and card has been moved
         (when (has-subtype? card "Run")
           (swap! state assoc-in [:runner :register :click-type] :run))
         (wait-for (pay state side (make-eid state eid) moved-card costs {:action :play-instant})
                   (if-let [payment-str (:msg async-result)]
                     (complete-play-instant state side eid moved-card payment-str ignore-cost)
                     ;; could not pay the card's price; put it back and mark the effect as being over.
                     (do
                       (move state side moved-card original-zone)
                       (effect-completed state side eid)))))
       ;; card's req or other effects was not satisfied; mark the effect as being over.
       (effect-completed state side eid)))))
