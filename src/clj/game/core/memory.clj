(ns game.core.memory
  (:require
    [game.core.card :refer [get-card has-subtype? program? virus-program?]]
    [game.core.effects :refer [get-effects get-effect-maps get-effect-value sum-effects]]
    [game.core.eid :refer [make-eid]]
    [game.core.toasts :refer [toast]]
    [cond-plus.core :refer [cond+]]))

(defn available-mu
  "Returns the available MU the runner has"
  ([state] (available-mu state nil))
  ([state _]
   (let [memory (get-in @state [:runner :memory])]
     (- (reduce + (or (:available memory) 0)
                (keep :available (vals (:only-for memory))))
        (or (:used memory) 0)))))

(defn- get-available-mu
  "Returns a list of vec pairs: [mu-type value]"
  [state]
  (concat [[:regular (or (get-in @state [:runner :memory :base]) 0)]]
          (get-effects state :runner nil :user-available-mu)
          (get-effects state :runner nil :available-mu)))

(def type-preds
  {:caissa #(has-subtype? % "Caïssa")
   :virus virus-program?})

(defn- merge-available-memory
  [mu-list]
  (reduce
    (fn [acc [mu-type amount]]
      (update acc mu-type (fnil + 0) amount))
    (zipmap (conj (keys type-preds) :regular) (repeat 0))
    mu-list))

(defn- merge-used-memory
  [state used-mu-effects]
  (let [effect-value-fn (get-effect-value state :runner)]
    (reduce
      (fn [acc effect]
        (loop [type-preds type-preds]
          (let [[mu-type pred] (first type-preds)]
            (cond+
              [(nil? mu-type)
               (update acc :regular (fnil + 0 0) (effect-value-fn effect))]
              [(pred (:card effect))
               (update acc mu-type (fnil + 0 0) (effect-value-fn effect))]
              [:else
               (recur (next type-preds))]))))
      (zipmap (conj (keys type-preds) :regular) (repeat 0))
      used-mu-effects)))

(defn subtract-used-from-available
  [available-mu used-mu]
  (reduce
    (fn [total-used mu-type]
      (let [available (or (get available-mu mu-type) 0)
            used (or (get used-mu mu-type) 0)
            diff (- available used)]
        (+ total-used
           ;; the specific memory "overflowed" and we want to add
           ;; the overflow to the total used
           (if (neg? diff)
             (- diff)
             0))))
    (or (:regular used-mu) 0)
    (keys type-preds)))

(defn build-new-mu
  [state]
  (let [mu-list (get-available-mu state)
        available-mu (merge-available-memory mu-list)

        used-mu-effects (get-effect-maps state :runner :used-mu)
        used-mu (merge-used-memory state used-mu-effects)

        only-for (into {} (for [mu-type (keys type-preds)]
                            [mu-type {:available (get available-mu mu-type)
                                      :used (get used-mu mu-type)}]))
        total-available (:regular available-mu)
        total-used (subtract-used-from-available available-mu used-mu)]
    {:only-for only-for
     :available total-available
     :used total-used}))

(defn update-mu
  ([state] (update-mu state nil))
  ([state _]
   (let [old-mu (select-keys (get-in @state [:runner :memory]) [:available :used :only-for])
         new-mu (build-new-mu state)
         changed? (not= old-mu new-mu)]
     (when changed?
       (when (neg? (:used new-mu))
         (toast state :runner "You have exceeded your memory units!"))
       (swap! state update-in [:runner :memory] merge new-mu))
     changed?)))

(defn mu+
  "For use in :constant-effects and register-floating-effect.
  Returns an effect map for :available-mu.
  Takes either the mu value or a :req 5-fn and the value.
  If :value is a function, it must return [:regular N] where N is a number."
  ([value] (mu+ (constantly true) value))
  ([req value]
   {:type :available-mu
    :req req
    :value (cond+
             [(or (vector? value) (fn? value)) value]
             [(number? value) [:regular value]]
             [:else (throw (Exception. (str "mu+ needs a vector, number, or function: " value)))])}))

(defn virus-mu+
  "For use in :constant-effects and register-floating-effect.
  Returns an effect map for :available-mu
  Takes either the mu value or a :req 5-fn and the value.
  If :value is a function, it must return [:virus N] where N is a number."
  ([value] (virus-mu+ (constantly true) value))
  ([req value] (mu+ req (cond+
                          [(or (vector? value) (fn? value)) value]
                          [(number? value) [:virus value]]
                          [:else (throw (Exception. (str "virus-mu+ needs a vector, number, or function: " value)))]))))

(defn caissa-mu+
  "For use in :constant-effects and register-floating-effect.
  Returns an effect map for :available-mu.
  Takes either the mu value or a :req 5-fn and the value.
  If :value is a function, it must return [:caissa N] where N is a number."
  ([value] (caissa-mu+ (constantly true) value))
  ([req value] (mu+ req (cond+
                          [(or (vector? value) (fn? value)) value]
                          [(number? value) [:caissa value]]
                          [:else (throw (Exception. (str "caissa-mu+ needs a vector, number, or function: " value)))]))))
