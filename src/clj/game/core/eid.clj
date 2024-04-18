(ns game.core.eid
  (:require
    [medley.core :refer [find-first]]
    [game.core.card :refer [basic-action?]]
    [game.core.prompt-state :refer [remove-from-prompt-queue]]))

(defn make-eid
  ([state] (make-eid state nil))
  ([state existing-eid]
   (assoc existing-eid :eid (:eid (swap! state update :eid inc)))))

(defn get-ability-targets
  [eid]
  (get-in eid [:source-info :ability-targets 0]))

(defn is-basic-advance-action?
  [eid]
  (and (basic-action? (:source eid))
       (= 4 (get-in eid [:source-info :ability-idx]))))

(defn register-effect-completed
  [state eid effect]
  (if (get-in @state [:effect-completed (:eid eid)])
    (throw (ex-info "Eid has already been registered" eid))
    (swap! state assoc-in [:effect-completed (:eid eid)] effect)))

(defn clear-eid-wait-prompt
  [state side eid]
  (when-let [prompt (find-first #(and (= (:eid eid) (:eid (:eid %)))
                                      (= :waiting (:prompt-type %)))
                                (get-in @state [side :prompt]))]
    (remove-from-prompt-queue state side prompt)))

(defn effect-completed
  [state _ eid]
  (doseq [side [:corp :runner]]
    (clear-eid-wait-prompt state side eid))
  (when-let [handler (get-in @state [:effect-completed (:eid eid)])]
    (handler eid)
    (swap! state update :effect-completed dissoc (:eid eid)))
  nil)

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  nil)
