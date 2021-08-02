(ns game.core.eid
  (:require
    [medley.core :refer [find-first]]
    [game.core.prompt-state :refer [remove-from-prompt-queue]]))

(defn make-eid
  ([state] (make-eid state nil))
  ([state existing-eid]
   (assoc existing-eid :eid (:eid (swap! state update :eid inc)))))

(defn make-action-eid
  [state card]
  (assoc (make-eid state) :source card :source-type :action))

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

(defn register-effect-completed
  [state eid effect]
  (if (get-in @state [:effect-completed (:eid eid)])
    (throw (Exception. (str "Eid has already been registered")))
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
  (if-let [handler (get-in @state [:effect-completed (:eid eid)])]
    (let [results (handler eid)]
      (swap! state update :effect-completed dissoc (:eid eid))
      (when (= (:eid eid) (get-in @state [:resolving-action :eid]))
        (swap! state dissoc :resolving-action))
      results)
    (when (= (:eid eid) (get-in @state [:resolving-action :eid]))
      (swap! state dissoc :resolving-action))))

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  result)
