(ns game.core.play-instants
  (:require
    [game.core.card :refer [get-card get-zone has-subtype?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.cost-fns :refer [play-additional-cost-bonus play-cost]]
    [game.core.effects :refer [unregister-static-abilities]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid]]
    [game.core.engine :refer [checkpoint dissoc-req merge-costs-paid pay queue-event resolve-ability should-trigger? unregister-events]]
    [game.core.flags :refer [can-run? zone-locked?]]
    [game.core.gaining :refer [lose]]
    [game.core.initializing :refer [card-init]]
    [game.core.moving :refer [move trash]]
    [game.core.payment :refer [build-spend-msg-suffix can-pay? merge-costs ->c]]
    [game.core.revealing :refer [reveal]]
    [game.core.say :refer [play-sfx system-msg implementation-msg]]
    [game.core.update :refer [update!]]
    [game.macros :refer [continue-ability msg req wait-for]]
    [game.utils :refer [same-card? to-keyword]]))

(defn async-rfg
  ([state side eid card] (async-rfg state side eid card nil))
  ([state side eid card _]
   (let [card (move state (to-keyword (:side card)) card :rfg)]
     (complete-with-result state side eid card))))

(defn- current-handler
  [state _ card]
  (if (has-subtype? card "Current")
    (move state (to-keyword (:side card)) card :current)
    card))

;;; Playing cards.
(defn- complete-play-instant
  "Completes the play of the event / operation that the player can play for"
  [state side eid {:keys [title] :as card} payment-str ignore-cost]
  (system-msg state side {:type :play :cost payment-str :card title :ignore-cost ignore-cost})
  (implementation-msg state card)
  (if-let [sfx (:play-sound (card-def card))]
    (play-sfx state side sfx)
    (play-sfx state side "play-instant"))
  ;; Select the "on the table" version of the card
  (let [card (current-handler state side card)
        cdef (-> (:on-play (card-def card))
                 (dissoc :cost :additional-cost)
                 (dissoc-req))
        card (card-init state side
                        (if (:rfg-instead-of-trashing cdef)
                          (assoc card :rfg-instead-of-trashing true)
                          card)
                        ;; :resolve-effect is true as a temporary solution to allow Direct Access to blank IDs
                        {:resolve-effect true :init-data true})
        play-event (if (= side :corp) :play-operation :play-event)
        resolved-event (if (= side :corp) :play-operation-resolved :play-event-resolved)]
    (queue-event state play-event {:card card :event play-event})
    (swap! state update-in [:stats side :cards-played :play-instant] (fnil inc 0))
    (wait-for (checkpoint state nil (make-eid state eid) {:duration play-event})
              (wait-for (resolve-ability state side (make-eid state eid) cdef card nil)
                        (let [c (some #(when (same-card? card %) %) (get-in @state [side :play-area]))
                              trash-after-resolving (:trash-after-resolving cdef true)
                              zone (if (:rfg-instead-of-trashing c) :rfg :discard)]
                          (if (and c trash-after-resolving)
                            (let [trash-or-move (if (= zone :rfg) async-rfg trash)]
                              (wait-for (trash-or-move state side c {:unpreventable true})
                                        (unregister-events state side card)
                                        (unregister-static-abilities state side card)
                                        (when (= zone :rfg)
                                          (system-msg state side
                                                      (str "removes " (:title c) " from the game instead of trashing it")))
                                        (when (has-subtype? card "Terminal")
                                          (lose state side :click (-> @state side :click))
                                          (swap! state assoc-in [:corp :register :terminal] true))
                                        ;; this is explicit support for nuvem,
                                        ;; which wants 'after the op finishes resolving' as an event
                                        (queue-event state resolved-event {:card card :event resolved-event})
                                        (checkpoint state nil eid {:duration resolved-event})))
                            (do (when (has-subtype? card "Terminal")
                                  (lose state side :click (-> @state side :click))
                                  (swap! state assoc-in [:corp :register :terminal] true))
                                (queue-event state resolved-event {:card card :event resolved-event})
                                (checkpoint state nil eid {:duration resolved-event}))))))))

(defn play-instant-costs
  [state side card {:keys [ignore-cost base-cost no-additional-cost cached-costs cost-bonus]}]
  (or cached-costs
      (let [cost (play-cost state side card {:cost-bonus cost-bonus})
            additional-costs (play-additional-cost-bonus state side card)
            costs (merge-costs
                    [(when-not ignore-cost
                       [base-cost (->c :credit cost)])
                     (when (and (has-subtype? card "Triple")
                                (not no-additional-cost))
                       (->c :click 2))
                     (when (and (has-subtype? card "Double")
                                (not no-additional-cost)
                                (not (get-in @state [side :register :double-ignore-additional])))
                       (->c :click 1))
                     (when-not (or no-additional-cost ignore-cost)
                       [additional-costs])])]
        costs)))

(defn- can-decline-instant?
  ([state side eid card {:keys [ignore-cost no-additional-cost]}]
   (and (not no-additional-cost)
        (not ignore-cost)
        (or (has-subtype? card "Double")
            (has-subtype? card "Triple")
            (seq (play-additional-cost-bonus state side card))))))

(defn can-play-instant?
  ([state side eid card] (can-play-instant? state side eid card nil))
  ([state side eid card {:keys [targets silent] :as args}]
   (let [eid (assoc eid :source-type :play)
         on-play (or (:on-play (card-def card)) {})
         costs (play-instant-costs state side card args)]
     (and ;; card still exists
       (get-card state card)
       ;; req is satisfied
       (should-trigger? state side eid card targets on-play)
       ;; can pay all costs
       (can-pay? state side eid card nil costs)
       ;; The zone isn't locked
       (not (zone-locked? state side (first (get-zone card))))
       ;; This is a current, and currents can be played
       (not (and (has-subtype? card "Current")
                 (get-in @state [side :register :cannot-play-current])))
       ;; This is a run event or makes a run, and running is allowed
       (not (and (or (:makes-run (card-def card))
                     (has-subtype? card "Run"))
                 (not (can-run? state :runner silent))))
       ;; if priority, have not spent a click
       (not (and (has-subtype? card "Priority")
                 (get-in @state [side :register :spent-click])))
       ;; explicitly return true
       true))))

(defn continue-play-instant
  [state side eid card costs {:keys [ignore-cost] :as args}]
  (let [original-zone (:zone card)
        moved-card (move state side (assoc card :seen true) :play-area)]
    (wait-for (pay state side (make-eid state (assoc eid :action :play-instant)) moved-card costs)
              (let [payment-str (:msg async-result)
                    cost-paid (merge-costs-paid (:cost-paid eid) (:cost-paid async-result))
                    eid (assoc eid :cost-paid cost-paid :source-type :ability)]
                (if payment-str
                  (complete-play-instant state side eid moved-card payment-str ignore-cost)
                  ;; could not pay the card's price; put it back and mark the effect as being over.
                  (let [returned-card (move state side moved-card original-zone)]
                    (continue-ability
                      state side
                      {:msg (msg "reveal that they are unable to play " (:title card))
                       :cost (when (:base-cost args) [(:base-cost args)])
                       :async true
                       :effect (req (update! state :runner (-> returned-card
                                               (dissoc :seen)
                                               (assoc
                                                 :cid (:cid card)
                                                 :previous-zone (:previous-zone card))))
                                    (reveal state side eid card))}
                      card nil)))))))

(defn play-instant
  "Plays an Event or Operation."
  ([state side eid card] (play-instant state side eid card nil))
  ([state side eid card args]
   (let [eid (assoc eid :source card :source-type :play)
         costs (play-instant-costs state side card (dissoc args :cached-costs))]
     ;; ensure the instant can be played
     (if (can-play-instant? state side eid card (assoc args :cached-costs costs))
       ;; Wait on pay to finish before triggering instant-effect
       (if (and (can-decline-instant? state side eid card args) (not (:base-cost args)))
         (continue-ability
           state side
           {:optional
            {:prompt (str "Pay the additional costs to play " (:title card) "?")
             :yes-ability {:async true
                           :req (req (can-pay? state side eid (get-card state card) nil costs))
                           :effect (req (continue-play-instant state side (assoc eid :source card :source-type :play) card costs args))}
             :no-ability {:cost (when (:base-cost args) [(:base-cost args)])
                          :async true
                          :effect (req (reveal state side eid card)) ;;TODO - use reveal-explicit later?
                          :msg (msg "reveal " (:title card) ", and refuse to pay the additional cost to play " (:title card))}}}
           card nil)
         (continue-play-instant state side eid card costs args))
       (continue-ability
         state side
         {:msg (msg "reveal that they are unable to play " (:title card))
          :cost (when (:base-cost args) [(:base-cost args)])
          :async true
          ;;TODO - use reveal-explicit later?
          :effect (req (reveal state side eid card))}
         card nil)))))
