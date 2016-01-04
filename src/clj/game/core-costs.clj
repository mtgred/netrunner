(in-ns 'game.core)

(defn deduce
  "Deduct the value from the player's attribute."
  [state side [attr value]]
  (swap! state update-in [side attr] (if (= attr :memory)
                                       #(- % value) ;; memoryunits may be negative
                                       #(max 0 (- % value))))
  (when (and (= attr :credit) (= side :runner) (get-in @state [:runner :run-credit]))
    (swap! state update-in [:runner :run-credit] #(max 0 (- % value))))
  (when-let [cost-name (cost-names value attr)]
    cost-name))

(declare toast)

(defn can-pay?
  "Returns nil if the player cannot pay the cost args, or a truthy map otherwise."
  [state side & args]
  (let [costs (merge-costs (remove #(or (nil? %) (= % [:forfeit])) args))
        forfeit-cost (some #{[:forfeit] :forfeit} args)
        scored (get-in @state [side :scored])]
    (if (and (every? #(or (>= (- (get-in @state [side (first %)]) (last %)) 0)
                          (= (first %) :memory)) ;; memoryunits may be negative
                     costs)
             (or (not forfeit-cost) (not (empty? scored))))
      {:costs costs, :forfeit-cost forfeit-cost, :scored scored}
      (do (toast state side (str "Unable to pay " costs)) false))))

(defn pay
  "Deducts each cost from the player."
  [state side card & args]
  (when-let [{:keys [costs forfeit-cost scored]} (apply can-pay? state side args)]
    (when forfeit-cost
      (if (= (count scored) 1)
        (forfeit state side (first scored))
        (prompt! state side card "Choose an Agenda to forfeit" scored
                 {:effect (effect (forfeit target))})))
    (->> costs (map #(do
                      (when (= (first %) :click)
                        (trigger-event state side (if (= side :corp) :corp-spent-click :runner-spent-click) nil)
                        (swap! state assoc-in [side :register :spent-click] true))
                      (deduce state side %)))
         (filter some?)
         (interpose " and ")
         (apply str))))

(defn gain [state side & args]
  (doseq [r (partition 2 args)]
    (swap! state update-in [side (first r)] #(+ (or % 0) (last r)))))

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (trigger-event state side (if (= side :corp) :corp-loss :runner-loss) r)
    (if (= (last r) :all)
      (swap! state assoc-in [side (first r)] 0)
      (deduce state side r))))

(defn rez-cost-bonus [state side n]
  (swap! state update-in [:bonus :cost] (fnil #(+ % n) 0)))

(defn rez-cost [state side {:keys [cost] :as card}]
  (if (nil? cost)
    nil
    (-> (if-let [rezfun (:rez-cost-bonus (card-def card))]
          (+ cost (rezfun state side card nil))
          cost)
        (+ (or (get-in @state [:bonus :cost]) 0))
        (max 0))))

(defn trash-cost-bonus [state side n]
  (swap! state update-in [:bonus :trash] (fnil #(+ % n) 0)))

(defn trash-cost [state side {:keys [trash] :as card}]
  (if (nil? trash)
    nil
    (-> trash
        (+ (or (get-in @state [:bonus :trash]) 0))
        (max 0))))

(defn install-cost-bonus [state side n]
  (swap! state update-in [:bonus :install-cost] #(merge-costs (concat % n))))

(defn install-cost [state side card all-cost]
  (vec (map #(if (keyword? %) % (max % 0))
            (-> (concat (get-in @state [:bonus :install-cost]) all-cost
                        (when-let [instfun (:install-cost-bonus (card-def card))] (instfun state side card nil)))
                merge-costs flatten))))

(defn modified-install-cost
  "Returns the number of credits required to install the given card, after modification effects including
  the argument 'additional', which is a vector of costs like [:credit -1]. Allows cards like
  Street Peddler to pre-calculate install costs without actually triggering the install."
  ([state side card] (modified-install-cost state side card nil))
  ([state side card additional]
   (trigger-event state side :pre-install card)
   (let [cost (install-cost state side card (merge-costs (concat additional [:credit (:cost card)])))]
     (swap! state update-in [:bonus] dissoc :install-cost)
     cost)))
