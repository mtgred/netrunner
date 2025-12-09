(ns game.core.damage
  (:require
    [game.core.card :refer [get-title]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid]]
    [game.core.engine :refer [checkpoint queue-event trigger-event trigger-event-simult]]
    [game.core.moving :refer [trash-cards get-trash-event]]
    [game.core.prevention :refer [resolve-damage-prevention]]
    [game.core.prompt-state :refer [add-to-prompt-queue remove-from-prompt-queue]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg n-last-logs]]
    [game.core.winning :refer [flatline]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [dissoc-in enumerate-cards enumerate-str side-str]]
    [jinteki.utils :refer [str->int]]
    [taoensso.timbre :as timbre]))

(defn damage-name [damage-type]
  (case damage-type
    :net "net"
    :meat "meat"
    :core "core"
    :brain "core"
    "[UNKNOWN DAMAGE TYPE]"))

(defn enable-runner-damage-choice
  [state _]
  (swap! state assoc-in [:damage :damage-choose-runner] true))

(defn enable-corp-damage-choice
  [state _]
  (swap! state assoc-in [:damage :damage-choose-corp] true))

(defn runner-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-runner]))

(defn corp-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-corp]))

(defn chosen-damage
  [state _ & targets]
  (swap! state update-in [:damage :chosen-damage] #(apply conj % (flatten targets))))

(defn- get-chosen-damage
  [state]
  (get-in @state [:damage :chosen-damage]))

(defn- damage-choice-priority
  "Determines which side gets to act if either or both have the ability to choose cards for damage.
  Currently just for Chronos Protocol vs Titanium Ribs"
  [state]
  (let [active-player (get-in @state [:active-player])]
    (when (and (corp-can-choose-damage? state) (runner-can-choose-damage? state))
      (if (= active-player :corp)
        (swap! state update-in [:damage] dissoc :damage-choose-runner)
        (swap! state update-in [:damage] dissoc :damage-choose-corp)))))

(defn- resolve-damage
  "Resolves the attempt to do n damage, now that both sides have acted to boost or
  prevent damage."
  [state side eid dmg-type n {:keys [card cause suppress-checkpoint]}]
  (swap! state dissoc-in [:damage :chosen-damage])
  (damage-choice-priority state)
  (wait-for (trigger-event-simult state side :pre-resolve-damage nil dmg-type side n)
            (if (not (pos? n))
              (do ;; shouldn't be possible, should be handled before getting here
                (timbre/error (str "attempted to resolve 0 damage: \n" (n-last-logs state 5) "\n"))
                (effect-completed state side eid))
              (let [hand (get-in @state [:runner :hand])
                    chosen-cards (seq (get-chosen-damage state))
                    chosen-cids (into #{} (map :cid chosen-cards))
                    leftovers (remove #(contains? chosen-cids (:cid %)) hand)
                    cards-trashed (->> (shuffle leftovers)
                                       (take (- n (count chosen-cards)))
                                       (concat chosen-cards))]
                (when (= dmg-type :brain)
                  (swap! state update-in [:runner :brain-damage] #(+ % n)))
                (when-let [trashed-msg (enumerate-cards cards-trashed :sorted)]
                  (system-msg state :runner (str "trashes " trashed-msg " due to " (damage-name dmg-type) " damage"))
                  (swap! state update-in [:stats :corp :damage :all] (fnil + 0) n)
                  (swap! state update-in [:stats :corp :damage dmg-type] (fnil + 0) n)
                  (if (< (count hand) n)
                    (do (flatline state)
                        (trigger-event state side :win {:winner :corp})
                        (trash-cards state side eid cards-trashed {:unpreventable true}))
                    (wait-for (trash-cards state side cards-trashed {:unpreventable true
                                                                     :cause dmg-type
                                                                     :suppress-checkpoint true
                                                                     :suppress-event true})
                              (queue-event state :damage {:amount n
                                                          :card card
                                                          :damage-type dmg-type
                                                          :from-side side
                                                          :cause cause
                                                          :cards-trashed cards-trashed})
                              (if suppress-checkpoint
                                (complete-with-result state side eid cards-trashed)
                                (let [trash-event (get-trash-event side false)
                                      args {:durations [:damage trash-event]}]
                                  (wait-for (checkpoint state nil (make-eid state eid) args)
                                            (complete-with-result state side eid cards-trashed)))))))))))

(defn damage
  "Attempts to deal n damage of the given type to the runner. Starts the
  prevention/boosting process and eventually resolves the damage."
  ([state side eid type n] (damage state side eid type n nil))
  ([state side eid type n {:keys [unpreventable card suppress-checkpoint] :as args}]
   (wait-for (resolve-damage-prevention state side type n args)
             (let [{:keys [remaining type source-card]} async-result]
               (if (pos? remaining)
                 (resolve-damage state side eid type remaining (assoc args :card source-card))
                 (do (queue-event state :all-damage-was-prevented {:side side
                                                                   :type type
                                                                   :cause-card source-card})
                     (if suppress-checkpoint
                       (effect-completed state side eid)
                       (checkpoint state side eid))))))))
