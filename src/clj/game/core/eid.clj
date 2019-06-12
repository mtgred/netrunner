(ns game.core.eid)

(defn make-eid
  ([state] (make-eid state nil))
  ([state {:keys [source source-type]}]
   (merge {:eid (:eid (swap! state update-in [:eid] inc))}
          (when source {:source source})
          (when source-type {:source-type source-type}))))

(defn register-effect-completed
  [state side eid effect]
  (swap! state update-in [:effect-completed (:eid eid)] #(conj % effect)))

(defn effect-completed
  [state side eid]
  (doseq [handler (get-in @state [:effect-completed (:eid eid)])]
    (handler state side eid))
  (swap! state update-in [:effect-completed] dissoc (:eid eid)))

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument.
  Helper function for cost-handler"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  result)
