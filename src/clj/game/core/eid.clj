(ns game.core.eid)

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

(defn register-effect-completed
  [state eid effect]
  (if (get-in @state [:effect-completed (:eid eid)])
    (throw (Exception. (str "Eid has alreasy been registered")))
    (swap! state assoc-in [:effect-completed (:eid eid)] effect)))

(defn clear-eid-wait-prompt
  [state side eid]
  (when-let [prompts (remove #(and (= (:eid eid) (:eid (:eid %)))
                                   (= :waiting (:prompt-type %)))
                             (get-in @state [side :prompt]))]
    (swap! state assoc-in [side :prompt] (doall prompts))))

(defn effect-completed
  [state _ eid]
  (doseq [side [:corp :runner]]
    (clear-eid-wait-prompt state side eid))
  (when-let [handler (get-in @state [:effect-completed (:eid eid)])]
    (let [results (handler eid)]
      (swap! state update :effect-completed dissoc (:eid eid))
      results)))

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  result)
