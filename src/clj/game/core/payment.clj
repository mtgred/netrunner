(ns game.core.payment
  (:require
    [clojure.string :as string]
    [game.core.board :refer [all-active-installed]]
    [game.core.card :refer [ice?]]
    [game.core.eid :refer [make-eid]]
    [game.core.effects :refer [any-effects]]
    [game.core.toasts :refer [toast]]
    [jinteki.utils :refer [capitalize]]))

(defn ->c
  ([type] (->c type 1))
  ([type n] (->c type n nil))
  ([type n {:keys [additional stealth maximum offset] :as args}]
   {:cost/type type
    :cost/amount n
    :cost/additional (boolean additional)
    :cost/stealth stealth
    :cost/maximum maximum
    :cost/offset offset
    :cost/args (not-empty (dissoc args :stealth :additional :maximum :offset))}))

(defmulti value :cost/type)
(defmethod value :default [_] 0)
(defmulti stealth-value :cost/type)
(defmethod stealth-value :default [_] 0)
(defmulti label :cost/type)
(defn- payable-dispatch [cost _state _side _eid _card] (:cost/type cost))
(defmulti payable? #'payable-dispatch)
(defn- handler-dispatch [cost _state _side _eid _card] (:cost/type cost))
(defmulti handler #'handler-dispatch)

(defn group-costs
  [costs]
  (->> costs
       (group-by #(let [cost-name (:cost/type %)]
                    ;; Don't group :x-credits
                    (if (= :x-credits cost-name)
                      (gensym)
                      cost-name)))
       (vals)))

(defn merge-cost-impl
  "Reducing function for merging costs of the same type, respecting stealth requirements."
  [acc cur]
  (let [acc-stealth (:cost/stealth acc)
        cur-stealth (:cost/stealth cur)]
    (->c (:cost/type cur)
         (+ (:cost/amount acc 0) (:cost/amount cur 0))
         (conj {:additional (:cost/additional cur)
                :maximum (:cost/maximum cur)
                :offset (:cost/offset cur)
                :stealth (cond
                           (or (= :all-stealth acc-stealth)
                               (= :all-stealth cur-stealth))
                           :all-stealth
                           (or acc-stealth cur-stealth)
                           (+ (or acc-stealth 0) (or cur-stealth 0)))}
               (:cost/args acc)
               (:cost/args cur)))))

(defn- display-cost-ranks
  [cost]
  (case (:cost/type cost)
    :click 1
    :lose-click 2
    :credit 3
    (:trash-can :remove-from-game) 4
    ; :else
    5))

(defn- impl-cost-ranks
  [cost]
  (case (:cost/type cost)
    :click 1
    :lose-click 2
    :credit 3
    (:advancement :power :virus) 4
    (:trash-can :remove-from-game) 5
    ; :else
    6))

(defn merge-costs
  "Combines disparate costs into a single cost per type."
  ([costs] (merge-costs costs false))
  ([costs remove-zero-credit-cost]
   (let [costs (filterv some? (flatten [costs]))
         {real false additional true} (group-by :cost/additional costs)
         real (group-costs real)
         additional (group-costs additional)]
     (->> (concat real additional)
          (keep #(reduce merge-cost-impl nil %))
          (remove #(if remove-zero-credit-cost
                     (and (= :credit (:cost/type %))
                          (zero? (:cost/amount %)))
                     false))
          (sort-by impl-cost-ranks)
          (into [])))))

(comment
  (= [(->c :click 4) (->c :credit 2)] (merge-costs [[(->c :click 1)] [(->c :click 3)] [(->c :credit 1)] [(->c :credit 1)]]))
  (= [(->c :click 4) (->c :credit 2)] (merge-costs [[(->c :credit 1)] [(->c :credit 1)] [(->c :click 1)] [(->c :click 3)]])))

(defn- any-effect-stops-pay?
  "Checks installed cards to see if payment type is being prevented by an active card"
  [state side cost]
  (let [kw-cost (keyword (str "cannot-pay-" (name (:cost/type cost))))]
    (any-effects state side kw-cost true? {:amount (:cost/amount cost)})))

(defn can-pay?
  "Returns nil if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  ([state side title args] (can-pay? state side (make-eid state) nil title args))
  ([state side eid card title & args]
   (let [remove-zero-credit-cost (and (= (:source-type eid) :corp-install)
                                      (not (ice? card)))
         costs (merge-costs (filter some? args) remove-zero-credit-cost)]
     (if (every? #(and (not (any-effect-stops-pay? state side %))
                       (payable? % state side eid card))
                 costs)
       costs
       (do (when title (toast state side (str "Unable to pay for " title ".")))
           nil)))))

(defn cost-targets
  [eid cost-type]
  (get-in eid [:cost-paid cost-type :paid/targets]))

(defn cost-target
  [eid cost-type]
  (first (cost-targets eid cost-type)))

(defn cost-value
  [eid cost-type]
  (get-in eid [:cost-paid cost-type :paid/value]))

(defn x-cost-value
  [eid]
  (get-in eid [:cost-paid :x-credits :paid/x-value] 0))

;; the function `pay` is defined in resolve-ability because they're all intermingled
;; fuck the restriction against circular dependencies, for real

;; cost labels and messages
(defn build-cost-label
  "Gets the complete cost-label for specified costs"
  [costs]
  (let [cost-string
        (->> (merge-costs costs)
             (sort-by display-cost-ranks)
             (map label)
             (interpose ", ")
             (apply str))]
    (when (not (string/blank? cost-string))
      (capitalize cost-string))))

(defn add-cost-label-to-ability
  ([ability] (add-cost-label-to-ability ability (:cost ability)))
  ([ability cost]
   (assoc ability :cost-label
          (build-cost-label (if-let [fake-cost (:fake-cost ability)]
                              (merge-costs cost fake-cost)
                              cost)))))

(comment
  (= "[Click][Click][Click][Click], 1 [Credits], suffer 1 net damage"
     (build-cost-label [(->c :click 1) (->c :click 3) (->c :net 1) (->c :credit 1)])))

(defn cost->string
  "Converts a cost to a string for printing"
  [cost]
  (if (:cost/type cost)
    (when (not (neg? (value cost)))
      (let [cost-type (:cost/type cost)
            cost-string (label cost)]
        (cond
          (#{:click :lose-click} cost-type) (str "spend " cost-string)
          (= :credit cost-type) (str "pay " cost-string)
          :else cost-string)))
    (cost->string (first cost))))

(defn build-cost-string
  "Gets the complete cost-str for specified costs"
  [costs]
  (let [cost-string
        (->> (merge-costs costs)
             (filter some?)
             (map cost->string)
             (interpose " and ")
             (apply str))]
    (when (not (string/blank? cost-string))
      (capitalize cost-string))))

(defn build-spend-msg
  "Constructs the spend message for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg cost-str verb nil))
  ([cost-str verb verb2]
   (if (string/blank? cost-str)
     (str (or verb2 (str verb "s")) " ")
     (str cost-str " to " verb " "))))
