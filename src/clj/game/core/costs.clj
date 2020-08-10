(in-ns 'game.core)

(declare forfeit prompt! damage mill is-scored? system-msg
         unknown->kw discard-from-hand card-str trash trash-cards
         all-installed-runner-type pick-credit-providing-cards all-active
         eligible-pay-credit-cards lose-tags number-of-virus-counters
         pick-virus-counters-to-spend)

(defn- deduct
  "Deduct the value from the player's attribute."
  [state side [attr value]]
  (cond
    ;; value is a map, should be :base, :mod, etc.
    (map? value)
    (doseq [[subattr value] value]
      (swap! state update-in [side attr subattr] (if (#{:mod :used} subattr)
                                                   ;; Modifications and mu used may be negative
                                                   ;; mu used is for easier implementation of the 0-mu hosting things
                                                   #(- % value)
                                                   (sub->0 value))))

    ;; values that expect map, if passed a number use default subattr of :mod
    (#{:hand-size :memory} attr)
    (deduct state side [attr {:mod value}])

    ;; default case for tags and bad-publicity is `:base`
    (#{:tag :bad-publicity} attr)
    (deduct state side [attr {:base value}])

    :else
    (do (swap! state update-in [side attr] (if (= attr :agenda-point)
                                             ;; Agenda points may be negative
                                             #(- % value)
                                             (sub->0 value)))
        (when (and (= attr :credit)
                   (= side :runner)
                   (pos? (get-in @state [:runner :run-credit] 0)))
          (swap! state update-in [:runner :run-credit] (sub->0 value))))))

(defn gain [state side & args]
  (doseq [[cost-type amount] (partition 2 args)]
    (cond
      ;; amount is a map, merge-update map
      (map? amount)
      (doseq [[subtype amount] amount]
        (swap! state update-in [side cost-type subtype] (safe-inc-n amount))
        (swap! state update-in [:stats side :gain cost-type subtype] (fnil + 0) amount))

      ;; Default cases for the types that expect a map
      (#{:hand-size :memory} cost-type)
      (gain state side cost-type {:mod amount})

      ;; Default case for tags and bad publicity is `:base`
      (#{:tag :bad-publicity} cost-type)
      (gain state side cost-type {:base amount})

      ;; Else assume amount is a number and try to increment cost-type by it.
      :else
      (do (swap! state update-in [side cost-type] (safe-inc-n amount))
          (swap! state update-in [:stats side :gain cost-type] (fnil + 0 0) amount)))
    (trigger-event state side (if (= side :corp) :corp-gain :runner-gain) [cost-type amount])))

(defn lose [state side & args]
  (doseq [[cost-type amount] (partition 2 args)]
    (if (= amount :all)
      (do (swap! state update-in [:stats side :lose cost-type] (fnil + 0) (get-in @state [side cost-type]))
          (swap! state assoc-in [side cost-type] 0))
      (do (when (number? amount)
            (swap! state update-in [:stats side :lose cost-type] (fnil + 0) amount))
          (deduct state side [cost-type amount])))
    (trigger-event state side (if (= side :corp) :corp-lose :runner-lose) [cost-type amount])))

(defn gain-credits
  "Utility function for triggering events"
  ([state side amount] (gain-credits state side (make-eid state) amount nil))
  ([state side amount args] (gain-credits state side (make-eid state) amount args))
  ([state side eid amount args]
   (if (and amount
            (pos? amount))
     (do (gain state side :credit amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-credit-gain :runner-credit-gain) args))
     (effect-completed state side eid))))

(defn lose-credits
  "Utility function for triggering events"
  ([state side amount] (lose-credits state side (make-eid state) amount nil))
  ([state side amount args] (lose-credits state side (make-eid state) amount args))
  ([state side eid amount args]
   (if (and amount
            (or (= :all amount)
                (pos? amount)))
     (do (lose state side :credit amount)
         (trigger-event-sync state side eid (if (= :corp side) :corp-credit-loss :runner-credit-loss) args))
     (effect-completed state side eid))))

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
  "Combines disparate costs into a single cost per type."
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

(defn- flag-stops-pay?
  "Checks installed cards to see if payment type is prevented by a flag"
  [state side cost-type]
  (let [flag (keyword (str "cannot-pay-" (name cost-type)))]
    (some #(card-flag? % flag true) (all-active-installed state side))))

(defn total-available-credits
  [state side eid card]
  (+ (get-in @state [side :credit])
     (->> (eligible-pay-credit-cards state side eid card)
          (map #(+ (get-counters % :recurring)
                   (get-counters % :credit)
                   (-> (card-def %) :interactions :pay-credits ((fn [x] (:custom-amount x 0))))))
          (reduce +))))

(defn build-spend-msg
  "Constructs the spend message for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg cost-str verb nil))
  ([cost-str verb verb2]
   (if (or (not (instance? String cost-str))
           (= "" cost-str))
     (str (or verb2 (str verb "s")) " ")
     (str cost-str " to " verb " "))))

;; Cost generation functions
(defn play-cost
  "Combines all relevant effects and costs to play a given card"
  ([state side card] (play-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [playfun (:play-cost-bonus (card-def card))]
             (playfun state side (make-eid state) card nil))
           (sum-effects state side card :play-cost)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn play-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side card :play-additional-cost))))

(defn rez-cost
  "Combines all rez effects and costs into a single number, not a cost vector"
  ([state side card] (rez-cost state side card nil))
  ([state side {:keys [cost] :as card} {:keys [cost-bonus]}]
   (when-not (nil? cost)
     (->> [cost
           (or cost-bonus 0)
           (when-let [rezfun (:rez-cost-bonus (card-def card))]
             (rezfun state side (make-eid state) card nil))
           (sum-effects state side card :rez-cost)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn rez-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side card :rez-additional-cost))))

(defn trash-cost
  "Returns the number of credits required to trash the given card."
  ([state side card] (trash-cost state side card nil))
  ([state side {:keys [trash] :as card} {:keys [cost-bonus]}]
   (when-not (nil? trash)
     (->> [trash
           (or cost-bonus 0)
           (when-let [trashfun (:trash-cost-bonus (card-def card))]
             (trashfun state side (make-eid state) card nil))
           (sum-effects state side card :trash-cost)]
          (reduce (fnil + 0 0))
          (max 0)))))

(defn install-cost
  "Returns the number of credits required to install the given card."
  ([state side card] (install-cost state side card nil nil))
  ([state side card args] (install-cost state side card args nil))
  ([state side card {:keys [cost-bonus]} & targets]
   (->> [(when (runner? card)
           (:cost card))
         (or cost-bonus 0)
         (when-let [instfun (:install-cost-bonus (card-def card))]
           (instfun state side (make-eid state) card nil))
         (sum-effects state side card :install-cost targets)]
        (reduce (fnil + 0 0))
        (max 0))))

(defn install-additional-cost-bonus
  [state side card]
  (merge-costs
    (concat (:additional-cost card)
            (:additional-cost (card-def card))
            (get-effects state side card :install-additional-cost))))

(defn ignore-install-cost?
  [state side card]
  (any-effects state side :ignore-install-cost true? card))

(defn run-cost
  "Get a list of all costs required to run a server."
  ([state side card] (run-cost state side card nil nil))
  ([state side card args] (run-cost state side card args nil))
  ([state side card {:keys [cost-bonus]} & targets]
   (->> [(or cost-bonus 0)
         (sum-effects state side card :run-cost targets)]
        (reduce (fnil + 0 0))
        (max 0))))

(defn run-additional-cost-bonus
  ([state side card] (run-additional-cost-bonus state side card nil))
  ([state side card & targets]
   (merge-costs
     (get-effects state side card :run-additional-cost targets))))

(defn has-trash-ability?
  [card]
  (let [abilities (:abilities (card-def card))
        events (:events (card-def card))]
    (or (some :trash-icon (concat abilities events))
        (some #(= :trash (first %))
              (->> abilities
                   (map :cost)
                   (map merge-costs)
                   (apply concat))))))

(defn card-ability-cost
  "Returns a list of all costs (printed and additional) required to use a given ability"
  ([state side ability card] (card-ability-cost state side ability card nil nil))
  ([state side ability card targets] (card-ability-cost state side ability card targets nil))
  ([state side ability card targets {:keys [cost-bonus] :as args}]
   (concat (:cost ability)
           (:additional-cost ability)
           (get-effects state side card :card-ability-additional-cost (flatten [ability targets])))))

(defn break-sub-ability-cost
  ([state side ability card] (break-sub-ability-cost state side ability card nil nil))
  ([state side ability card targets] (break-sub-ability-cost state side ability card targets nil))
  ([state side ability card targets {:keys [cost-bonus] :as args}]
   (concat (:cost ability)
           (:additional-cost ability)
           (get-effects state side card :break-sub-additional-cost (flatten [ability targets])))))

(defn jack-out-cost
  ([state side] (jack-out-cost state side nil))
  ([state side args]
   (get-effects state side nil :jack-out-additional-cost args)))
