(ns game.core.memory
  (:require
   [cond-plus.core :refer [cond+]]
   [game.core.card :refer [has-subtype? virus-program? program?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.eid :refer [make-eid]]
   [game.core.effects :refer [get-effect-maps get-effect-value get-effects is-disabled-reg? register-lingering-effect]]
   [game.core.toasts :refer [toast]]))

(defn mu+
  "For use in :static-abilities and register-lingering-effect.
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
  "For use in :static-abilities and register-lingering-effect.
  Returns an effect map for :available-mu
  Takes either the mu value or a :req 5-fn and the value.
  If :value is a function, it must return [:virus N] where N is a number."
  ([value] (virus-mu+ (constantly true) value))
  ([req value] (mu+ req (cond+
                          [(or (vector? value) (fn? value)) value]
                          [(number? value) [:virus value]]
                          [:else (throw (Exception. (str "virus-mu+ needs a vector, number, or function: " value)))]))))

(defn caissa-mu+
  "For use in :static-abilities and register-lingering-effect.
  Returns an effect map for :available-mu.
  Takes either the mu value or a :req 5-fn and the value.
  If :value is a function, it must return [:caissa N] where N is a number."
  ([value] (caissa-mu+ (constantly true) value))
  ([req value] (mu+ req (cond+
                          [(or (vector? value) (fn? value)) value]
                          [(number? value) [:caissa value]]
                          [:else (throw (Exception. (str "caissa-mu+ needs a vector, number, or function: " value)))]))))

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
          (get-effects state :runner :user-available-mu)
          (get-effects state :runner :available-mu)))

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
  "Convert the list of used-mu-effects to a map saying how much each type is used:
  1. Create a zero'd map of the mu-type keywords
  2. For each of the used mu effects, check which of the mu types it might use
  3. Increment relevant mu type"
  [state used-mu-effects]
  (let [effect-value-fn (get-effect-value state :runner)
        initial-used-mu (zipmap (conj (keys type-preds) :regular) (repeat 0))]
    (reduce
      (fn [acc effect]
        (loop [type-preds type-preds]
          (let [[mu-type pred] (first type-preds)]
            (cond+
              ;; If we've looped through and mu-type is now `nil`, increment regular
              [(nil? mu-type)
               (update acc :regular (fnil + 0 0) (effect-value-fn effect))]
              ;; Otherwise, check the pred against the card
              [(pred (:card effect))
               (update acc mu-type (fnil + 0 0) (effect-value-fn effect))]
              ;; Loop it up
              [:else
               (recur (next type-preds))]))))
      initial-used-mu
      used-mu-effects)))

(defn combine-used-mu
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
        total-used (combine-used-mu available-mu used-mu)]
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
       (when (neg? (- (:available new-mu) (:used new-mu)))
         (toast state :runner "You have exceeded your memory units!"))
       (swap! state update-in [:runner :memory] merge new-mu))
     changed?)))

(defn some-mu-effect?
  [state card]
  (let [ab (first (filter #(= (:type %) :used-mu) (:static-abilities (card-def card))))
        abreq (:req ab)
        abval (:value ab)]
    (if (and ab (or (nil? abreq) (abreq state :runner nil card nil)))
      (abval state :runner nil card nil)
      0)))

(defn expected-mu
  [state card]
  (if (program? card)
    (+ (:memoryunits card) (some-mu-effect? state card))
    0))

(defn sufficient-mu?
  "Will installing this card put the runner over their memory limit?"
  [state card]
  (when (program? card)
    (let [mu-cost (expected-mu state card)
          mu-list (get-available-mu state)
          available-mu (merge-available-memory mu-list)
          used-mu-effects (conj (get-effect-maps state :runner :used-mu)
                                {:type :used-mu
                                 :duration :while-active
                                 :card card
                                 :value mu-cost})
          used-mu (merge-used-memory state used-mu-effects)
          total-available (:regular available-mu)
          total-used (combine-used-mu available-mu used-mu)]
      (<= 0 (- total-available total-used)))))

(defn init-mu-cost
  "(re) establish lingering effect of program using up memory"
  [state card]
  (register-lingering-effect
    state :runner card
    {:type :used-mu
     :duration :while-active
     :value (:memoryunits card)})
  (update-mu state))
