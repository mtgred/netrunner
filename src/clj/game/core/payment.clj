(ns game.core.payment
  (:require
    [clojure.string :as string]
    [game.core.board :refer [all-active-installed]]
    [game.core.card :refer [ice?]]
    [game.core.eid :refer [make-eid]]
    [game.core.flags :refer [card-flag?]]
    [game.core.toasts :refer [toast]]
    [jinteki.utils :refer [capitalize]]))

(defmulti cost-name (fn [[cost-type _]] cost-type))
(defmulti value (fn [[cost-type _]] cost-type))
(defmulti stealth-value (fn [[cost-type]] cost-type))
(defmulti label (fn [[cost-type _]] cost-type))
(defmulti payable? (fn [[cost-type] & _] cost-type))
(defmulti handler (fn [[cost-type] & _] cost-type))

(defn- add-default-to-costs
  "Transform a sequence of cost vectors (with any amount of nesting) into a
  sequence of conforming cost vectors: keyword and 1 or more numbers"
  [costs]
  (loop [[cost-type & rest-costs] (filter #(or (keyword? %) (number? %)) (flatten costs))
         result []]
    (if-not cost-type
      result
      (let [[quantities remainder] (split-with number? rest-costs)
            quantities (or (not-empty quantities) [1])
            new-cost (into [] (cons cost-type quantities))]
        (recur remainder (conj result new-cost))))))

(defn- cost-ranks
  [[cost-type _]]
  (case cost-type
    :click 1
    :lose-click 2
    :credit 3
    (:trash-can :remove-from-game) 4
    5))

(defn merge-cost-impl
  "Reducing function for merging costs of the same type, respecting stealth requirements."
  [[_ value-acc stealth-acc] [cost-type cost-value stealth-value]]
  (->> [cost-type
        ;; Using the multi-method value here in case a cost-type
        ;; shouldn't have more than 1 (e.g. :trash-can)
        (value [cost-type ((fnil + 0 0) value-acc cost-value)])
        (when (or stealth-acc stealth-value)
          ((fnil + 0 0) stealth-acc stealth-value))]
       ;; Remove nil stealth costs
       (filterv some?)))

(defn group-costs
  [costs]
  (->> costs
       (group-by #(let [cost-name (cost-name %)]
                    ;; Don't group :x-credits
                    (if (= :x-credits cost-name)
                      (gensym)
                      cost-name)))
       (vals)))

(defn merge-costs
  "Combines disparate costs into a single cost per type."
  ([costs] (merge-costs costs false))
  ([costs remove-zero-credit-cost]
   (->> (add-default-to-costs costs)
        (group-costs)
        (map #(reduce merge-cost-impl [] %))
        (remove #(if remove-zero-credit-cost
                   (and (= :credit (cost-name %))
                        (zero? (value %)))
                   false))
        (sort-by cost-ranks)
        (into []))))

(comment
  (= [[:click 4] [:credit 2]] (merge-costs [[:click 1] [:click 3] [:credit 1] [:credit 1]]))
  (= [[:click 4] [:credit 2]] (merge-costs [[:credit 1] [:credit 1] [:click 1] [:click 3]])))

(defn- flag-stops-pay?
  "Checks installed cards to see if payment type is prevented by a flag"
  [state side cost]
  (let [flag (keyword (str "cannot-pay-" (name (cost-name cost))))]
    (some #(card-flag? % flag true) (all-active-installed state side))))

(defn can-pay?
  "Returns nil if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  ([state side title args] (can-pay? state side (make-eid state) nil title args))
  ([state side eid card title & args]
   (let [remove-zero-credit-cost (and (= (:source-type eid) :corp-install)
                                      (not (ice? card)))
         cost-req (when (and (:abilities (:source eid))
                             (:ability-idx (:source-info eid)))
                    (:cost-req (nth (:abilities (:source eid)) (:ability-idx (:source-info eid)) nil)))
         cost-filter (if (fn? cost-req) cost-req identity)
         costs (cost-filter (merge-costs (remove #(or (nil? %) (map? %)) args) remove-zero-credit-cost))]
     (if (every? #(and (not (flag-stops-pay? state side %))
                       (payable? % state side eid card))
                 costs)
       costs
       (do (when title (toast state side (str "Unable to pay for " title ".")))
           nil)))))

(defn cost-targets
  [eid cost-type]
  (get-in eid [:cost-paid cost-type :targets]))

(defn cost-target
  [eid cost-type]
  (first (cost-targets eid cost-type)))

(defn cost-value
  [eid cost-type]
  (get-in eid [:cost-paid cost-type :value]))

;; the function `pay` is defined in resolve-ability because they're all intermingled
;; fuck the restriction against circular dependencies, for real

;; cost labels and messages
(defn build-cost-label
  "Gets the complete cost-label for specified costs"
  [costs]
  (let [cost-string
        (->> (merge-costs costs)
             (map label)
             (interpose ", ")
             (apply str))]
    (when (not (string/blank? cost-string))
      (capitalize cost-string))))

(defn add-cost-label-to-ability
  ([ability] (add-cost-label-to-ability ability (:cost ability)))
  ([ability cost]
   (assoc ability :cost-label
          (build-cost-label (if (:trash-icon ability)
                              (conj cost [:trash-can])
                              cost)))))

(comment
  (= "[Click][Click][Click][Click], 1 [Credits], suffer 1 net damage"
     (build-cost-label [[:click 1] [:click 3] [:net 1] [:credit 1]])))

(defn cost->string
  "Converts a cost to a string for printing"
  [cost]
  (when (not (neg? (value cost)))
    (let [cost-type (cost-name cost)
          cost-string (label cost)]
      (cond
        (#{:click :lose-click} cost-type) (str "spend " cost-string)
        (= :credit cost-type) (str "pay " cost-string)
        :else cost-string))))

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
