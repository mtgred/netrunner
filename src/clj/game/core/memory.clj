(ns game.core.memory
  (:require
    [game.core.card :refer [get-card program? virus-program?]]
    [game.core.effects :refer [gather-effects sum-effects]]
    [game.core.eid :refer [make-eid]]
    [game.core.toasts :refer [toast]]
    ))

(defn- sum-available-memory
  [state]
  (+ (or (get-in @state [:runner :memory :base]) 0)
     (sum-effects state :runner nil :user-available-mu nil)
     (sum-effects state :runner nil :available-mu nil)))

(defn- sum-non-virus-programs-mu
  [state]
  (let [eid (make-eid state)]
    (->> (gather-effects state :runner :used-mu)
         (keep #(when-let [card (get-card state (:card %))]
                  (assoc % :card card)))
         (remove #(virus-program? (:card %)))
         (filter #(if-not (:req %)
                    true
                    ((:req %) state :runner eid (:card %) nil)))
         (map #(if-not (fn? (:value %))
                           (:value %)
                           ((:value %) state :runner eid (:card %) nil)))
         (filter number?)
         (reduce +))))

(defn- sum-available-virus-memory
  [state]
  (sum-effects state :runner nil :available-virus-mu nil))

(defn- sum-virus-programs-mu
  [state]
  (let [eid (make-eid state)]
    (->> (gather-effects state :runner :used-mu)
         (map #(assoc % :card (get-card state (:card %))))
         (filter #(virus-program? (:card %)))
         (filter #(if-not (:req %)
                    true
                    ((:req %) state :runner eid (:card %) nil)))
         (map #(if-not (fn? (:value %))
                           (:value %)
                           ((:value %) state :runner eid (:card %) nil)))
         (filter number?)
         (reduce +))))

(defn available-mu
  "Returns the available MU the runner has"
  [state]
  (- (get-in @state [:runner :memory :available] 0)
     (get-in @state [:runner :memory :used] 0)))

(defn update-mu
  ([state] (update-mu state nil))
  ([state _]
   (let [
         ;; non-virus memory
         available-memory (sum-available-memory state)
         used-memory (sum-non-virus-programs-mu state)
         ;; virus memory
         available-virus-memory (sum-available-virus-memory state)
         used-virus-memory (sum-virus-programs-mu state)
         ;; if this is negative, there's more virus programs than available virus-specific MU
         virus-memory-diff (- available-virus-memory used-virus-memory)
         ;; total available memory (both non-virus and virus)
         total-available (+ available-memory
                            (cond (pos? virus-memory-diff) available-virus-memory
                                  (neg? virus-memory-diff) 0
                                  :else 0))
         ;; total used memory (both non-virus and virus)
         total-used (+ used-memory
                       (cond (pos? virus-memory-diff) used-virus-memory
                             (neg? virus-memory-diff) (- virus-memory-diff)
                             :else 0))
         new-memory {:available-virus available-virus-memory
                     :used-virus used-virus-memory
                     :available total-available
                     :used total-used}
         old-memory (select-keys (get-in @state [:runner :memory]) [:available-virus :used-virus :available :used])
         changed? (not= old-memory new-memory)
         ]
     (when (neg? total-used)
       (toast state :runner "You have exceeded your memory units!"))
     (when changed?
       (swap! state update-in [:runner :memory] merge new-memory))
     changed?)))

(defn mu+
  [value]
  {:type :available-mu
   :value value})

(defn virus-mu+
  [value]
  {:type :available-virus-mu
   :value value})
