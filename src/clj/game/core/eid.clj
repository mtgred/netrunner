(ns game.core.eid)

(defn make-eid
  ([state] (make-eid state nil))
  ([state {:keys [source source-type source-info]}]
   (merge {:eid (:eid (swap! state update-in [:eid] inc))}
          (when source {:source source})
          (when source-type {:source-type source-type})
          (when source-info {:source-info source-info}))))

(defn eid-set-defaults
  "Set default values for fields in the `eid` if they are not already set."
  [eid & args]
  (let
    [remove-fn (fn [[k v]]
                 (contains? eid k))
     kvs (remove remove-fn (partition 2 args))]
    (if (not-empty kvs)
      (apply assoc eid (flatten kvs))
      eid)))

(defn register-effect-completed
  [state side eid effect]
  (swap! state update-in [:effect-completed (:eid eid)] #(conj % effect)))

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
