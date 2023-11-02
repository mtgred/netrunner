(ns game.core.eid
  (:require
    [medley.core :refer [find-first]]
    [game.core.card :refer [basic-action?]]
    [game.core.prompt-state :refer [remove-from-prompt-queue]]))

(defn make-eid
  ([state] (make-eid state nil))
  ([state existing-eid]
   (assoc existing-eid :eid (:eid (swap! state update :eid inc)))))

(defn eid-set-defaults
  "Set default values for fields in the `eid` if they are not already set."
  [eid & args]
  (let
    [remove-fn (fn [[k _]]
                 (contains? eid k))
     kvs (remove remove-fn (partition 2 args))]
    (if (not-empty kvs)
      (apply assoc eid (flatten kvs))
      eid)))

(defn get-ability-targets
  [eid]
  (get-in eid [:source-info :ability-targets]))

(defn is-basic-advance-action?
  [eid]
  (and (basic-action? (:source eid))
       (= 4 (get-in eid [:source-info :ability-idx]))))

(defn register-effect-completed
  [state eid effect]
  (prn (:eid eid) effect)
  (if (get-in @state [:effect-completed (:eid eid)])
    (throw (ex-info "Eid has already been registered" eid))
    (swap! state assoc-in [:effect-completed (:eid eid)] effect))
  (prn :newly-set-eid-effect (:eid eid) (get-in @state [:effect-completed (:eid eid)]))
  )

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
  #_(assert (nil? (get-in @state [:effect-completed :next-eid]))
        "There is no currently paused eid")
  (prn :effect-completed (:eid eid)
       (some? (get-in @state [:effect-completed (:eid eid)])))
  (when (get-in @state [:effect-completed (:eid eid)])
    (prn (:eid eid))
    (swap! state update-in [:effect-completed :next-eid] #(cons eid %)))
  (prn :next-eid??? (get-in @state [:effect-completed :next-eid]))
  #_(when-let [handler (get-in @state [:effect-completed (:eid eid)])]
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

(defn state-continue
  [state]
  (prn :state-continue)
  (loop [[next-eid] (get-in @state [:effect-completed :next-eid])
         handler (get-in @state [:effect-completed (:eid next-eid)])]
    (prn :next-eid next-eid
         :handler handler)
    (when-not next-eid
      (prn (:effect-completed @state)))
    (when next-eid
      (swap! state update-in [:effect-completed :next-eid] next)
      (when handler
        (handler next-eid))
      (swap! state update :effect-completed dissoc (:eid next-eid))
      (recur (get-in @state [:effect-completed :next-eid])
             (get-in @state [:effect-completed (:eid next-eid)])))))
