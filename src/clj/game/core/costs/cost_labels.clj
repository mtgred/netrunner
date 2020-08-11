(in-ns 'game.core)

;; cost labels and messages
(defn build-cost-label
  "Gets the complete cost-label for specified costs"
  [costs]
  (let [cost-string
        (->> (merge-and-convert-costs costs)
             (map label)
             (interpose ", ")
             (apply str))]
    (when (not (string/blank? cost-string))
      (capitalize cost-string))))

(defn add-cost-label-to-ability
  ;; TODO: This name fucking sucks. Too explicit, too wordy, very unclear
  ([ability] (add-cost-label-to-ability ability (:cost ability)))
  ([ability cost]
   (assoc ability :cost-label
          (build-cost-label (if (:trash-icon ability)
                              (conj cost [:trash])
                              cost)))))

(comment
  (= "[Click][Click][Click][Click], 1 [Credits], suffer 1 net damage"
     (build-cost-label [[:click 1] [:click 3] [:net 1] [:credit 1]])))

(defn cost->string
  "Converts a cost (amount attribute pair) to a string for printing"
  [cost]
  (when (not (neg? (value cost)))
    (let [cost-type (cost-name cost)
          cost-string (label cost)]
      (cond
        (= :click cost-type) (str "spend " cost-string)
        (= :credit cost-type) (str "pay " cost-string)
        :else cost-string))))

(defn build-cost-string
  "Gets the complete cost-str for specified costs"
  [costs]
  (let [cost-string
        (->> (merge-and-convert-costs costs)
             (filter some?)
             (map cost->string)
             (interpose " and ")
             (apply str))]
    (when (not (string/blank? cost-string))
      (capitalize cost-string))))

(defn- add-default-to-costs
  "Take a sequence of costs (nested or otherwise) and add a default value of 1
  to any that don't include a value (normally with :forfeit)."
  [costs]
  (->> costs
       flatten
       ;; Padding is needed when :default is the final cost in the list or all items are :default
       (partition 2 1 '(1))
       (reduce
         (fn [acc [cost-type qty]]
           ;; the possibilities are:
           ;; Usable:
           ;; * (:type qty) -> a normal cost (or :default is in final postion, so is padded)
           ;; * (:type :type) -> :default isn't the final cost
           ;; Unusable:
           ;; * (qty :type) -> normal part of moving one at a time
           ;; * (qty qty) -> a quantity-less cost was used earlier, so this can be ignored
           (cond
             (and (keyword? cost-type)
                  (number? qty))
             (conj acc [cost-type qty])
             (and (keyword? cost-type)
                  (keyword? qty))
             (conj acc [cost-type 1])
             :else
             acc))
         [])))

(defn cost-ranks
  [[cost-type _]]
  (case cost-type
    :click 1
    :credit 2
    (:trash :remove-from-game) 3
    4))

(defn merge-costs
  "Combines disparate costs into a single cost per type. For use outside of the pay system."
  ([costs] (merge-costs costs false))
  ([costs remove-zero-credit-cost]
   (->> (add-default-to-costs costs)
        (group-by first)
        vals
        (map (fn [cost-pairs]
               (reduce
                 (fn [[ct acc] [cost-type value]]
                   [cost-type ((fnil + 0 0) acc value)])
                 []
                 cost-pairs)))
        (remove #(if remove-zero-credit-cost
                   (and (= :credit (first %))
                        (zero? (second %)))
                   false))
        (sort-by cost-ranks)
        (into []))))

(comment
  (= [[:click 4] [:credit 2]] (merge-costs [[:click 1] [:click 3] [:credit 1] [:credit 1]]))
  (= [[:click 4] [:credit 2]] (merge-costs [[:credit 1] [:credit 1] [:click 1] [:click 3]])))

(defn build-spend-msg
  "Constructs the spend message for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg cost-str verb nil))
  ([cost-str verb verb2]
   (if (string/blank? cost-str)
     (str (or verb2 (str verb "s")) " ")
     (str cost-str " to " verb " "))))
