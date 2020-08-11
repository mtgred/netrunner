(in-ns 'game.core)

(defn create-cost
  [cost-kw qty]
  (let [constructor (get cost-records cost-kw)]
    (constructor qty)))

(defn convert-to-cost-records
  "Take a sequence of keyword costs (nested or not) and turn them into cost records.
  For use only within merge-and-convert-costs."
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
             (conj acc (create-cost cost-type qty))
             (and (keyword? cost-type)
                  (keyword? qty))
             (conj acc (create-cost cost-type 1))
             :else
             acc))
         [])))

(defn merge-and-convert-costs
  "Combines disparate costs into a single cost per type. For use only with can-pay?."
  ([costs] (merge-and-convert-costs costs false))
  ([costs remove-zero-credit-cost]
   (->> (convert-to-cost-records costs)
        (group-by cost-name)
        (map (fn [[map-key map-vals]]
               (reduce
                 (fn [acc instance]
                   (create-cost (cost-name acc) (+ (value acc) (value instance))))
                 map-vals)))
        (remove #(if remove-zero-credit-cost
                   (and (= :credit (cost-name %))
                        (zero? (value %)))
                   false))
        (sort-by rank)
        (into []))))

(defn- flag-stops-pay?
  "Checks installed cards to see if payment type is prevented by a flag"
  [state side cost]
  (let [flag (keyword (str "cannot-pay-" (name (cost-name cost))))]
    (some #(card-flag? % flag true) (all-active-installed state side))))

(defn can-pay?
  "Returns false if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  ([state side title args] (can-pay? state side (make-eid state) nil title args))
  ([state side eid card title & args]
   (let [remove-zero-credit-cost (and (= (:source-type eid) :corp-install)
                                      (not (ice? card)))
         costs (merge-and-convert-costs (remove #(or (nil? %) (map? %)) args) remove-zero-credit-cost)]
     (if (every? #(and (not (flag-stops-pay? state side %))
                       (payable? % state side eid card))
                 costs)
       costs
       (when title
         (toast state side (str "Unable to pay for " title "."))
         false)))))

(defn- pay-next
  [state side eid costs card actions msgs]
  (if (empty? costs)
    (complete-with-result state side eid msgs)
    (wait-for (handler (first costs) state side (make-eid state eid) card actions)
              (pay-next state side eid (rest costs) card actions (conj msgs async-result)))))

(defn sentence-join
  [strings]
  (if (<= (count strings) 2)
    (join " and " strings)
    (str (apply str (interpose ", " (butlast strings))) ", and " (last strings))))

(defn pay
  "Same as pay, but awaitable."
  [state side eid card & args]
  (let [args (flatten args)
        raw-costs (remove map? args)
        actions (filter map? args)]
    (if-let [costs (can-pay? state side eid card (:title card) raw-costs)]
      (wait-for (pay-next state side (make-eid state eid) costs card actions [])
                (complete-with-result state side eid (->> async-result
                                                          (filter some?)
                                                          sentence-join)))
      (complete-with-result state side eid nil))))
