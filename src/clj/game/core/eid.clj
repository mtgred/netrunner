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
  [state _ eid effect]
  (swap! state update-in [:effect-completed (:eid eid)] conj effect))

(defn effect-completed
  [state side eid]
  (let [results
        (reduce (fn [result handler]
                  (conj result (handler state side eid)))
                []
                (get-in @state [:effect-completed (:eid eid)]))]
    (swap! state update-in [:effect-completed] dissoc (:eid eid))
    results))

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  result)
