(ns game.core.pipeline
  (:require
    [cond-plus.core :refer [cond+]]
    [game.core.step :refer [completed? continue! SimpleStep?]]))

(defn get-current-step
  [state]
  (first (get-in @state [:gp :pipeline])))

(defn queue-step!
  [state step]
  (swap! state update-in [:gp :queue] conj (SimpleStep? step))
  state)

(defn drop-current-step!
  [state]
  (swap! state update-in [:gp :pipeline] #(into [] (next %)))
  state)

(defn update-pipeline!
  [state]
  (as-> (:gp @state) gp
    (assoc gp
           :pipeline (into [] (concat (:queue gp) (:pipeline gp)))
           :queue [])
    (swap! state assoc :gp gp))
  state)

(defn continue-gp!
  [state]
  (update-pipeline! state)
  (let [step (get-current-step state)]
    (cond+
      ;; Exit when there are no more steps
      [(not step)
       true]
      ;; If the current step is done, drop and move to the next
      [(or (completed? step)
           (true? (continue! step state))
           (completed? step))
       (drop-current-step! state)
       (recur state)]
      ;; If continue-fn is false and queue is empty, we're waiting
      ;; on the user for input
      [(zero? (count (get-in @state [:gp :queue])))
       false]
      [:else
       (recur state)])))
