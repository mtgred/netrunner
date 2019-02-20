(in-ns 'game.core)

(declare forfeit prompt! toast damage mill installed? is-type? is-scored? system-msg
         facedown? make-result)

(defn deduct
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

    ;; default case for `:tag` is `:base`
    (#{:tag} attr)
    (deduct state side [attr {:base value}])

    :else
    (do (swap! state update-in [side attr] (if (= attr :agenda-point)
                                             ;; Agenda points may be negative
                                             #(- % value)
                                             (sub->0 value)))
        (when (and (= attr :credit)
                   (= side :runner)
                   (pos? (get-in @state [:runner :run-credit] 0)))
          (swap! state update-in [:runner :run-credit] (sub->0 value)))))
  (when-let [cost-name (cost-names value attr)]
    cost-name))

(defn flag-stops-pay?
  "Checks installed cards to see if payment type is prevented by a flag"
  [state side type]
  (let [flag (keyword (str "cannot-pay-" (name type)))]
    (some #(card-flag? % flag true) (all-active-installed state side))))

(defn toast-msg-helper
  "Creates a toast message for given cost and title if applicable"
  [state side cost]
  (let [cost-type (first cost)
        amount (last cost)
        computer-says-no "Unable to pay"]
    (cond

      (flag-stops-pay? state side cost-type)
      computer-says-no

      (not (or (#{:memory :net-damage} cost-type)
               (and (= cost-type :forfeit) (>= (- (count (get-in @state [side :scored])) amount) 0))
               (and (= cost-type :mill) (>= (- (count (get-in @state [side :deck])) amount) 0))
               (and (= cost-type :tag) (>= (- (get-in @state [:runner :tag :base]) amount) 0))
               (and (= cost-type :ice) (>= (- (count (filter (every-pred rezzed? ice?) (all-installed state :corp))) amount) 0))
               (and (= cost-type :hardware) (>= (- (count (get-in @state [:runner :rig :hardware])) amount) 0))
               (and (= cost-type :program) (>= (- (count (get-in @state [:runner :rig :program])) amount) 0))
               (and (= cost-type :resource) (>= (- (count (get-in @state [:runner :rig :resource])) amount) 0))
               (and (= cost-type :connection) (>= (- (count (filter #(has-subtype? % "Connection")
                                                                    (all-active-installed state :runner))) amount) 0))
               (and (= cost-type :shuffle-installed-to-stack) (>= (- (count (all-installed state :runner)) amount) 0))
               (>= (- (get-in @state [side cost-type] -1) amount) 0)))
      computer-says-no)))

(defn can-pay?
  "Returns false if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  [state side title & args]
  (let [costs (merge-costs (remove #(or (nil? %) (map? %)) args))
        cost-msg (some #(toast-msg-helper state side %) costs)]
    ;; no cost message - hence can pay
    (if-not cost-msg
      costs
      (when title (toast state side (str cost-msg " for " title ".")) false))))

(defn can-pay-with-recurring?
  "Returns true if the player can pay the cost factoring in available recurring credits"
  [state side cost]
  (>= (+ (- (get-in @state [side :credit] -1) cost)
         (->> (all-installed state side)
              (map #(+ (get-counters % :recurring)
                       (get-counters % :credit)))
              (reduce +)))
      0))

(defn pay-forfeit
  "Forfeit agenda as part of paying for a card or ability
  Amount is always 1 but can be extend if we ever need more than a single forfeit"
  ;; If multiples needed in future likely prompt-select needs work to take a function
  ;; instead of an ability
  [state side eid card n]
  (let [cost-name (cost-names n :forfeit)]
    (continue-ability state side
                      {:prompt "Choose an Agenda to forfeit"
                       :async true
                       :choices {:max n
                                 :req #(is-scored? state side %)}
                       :effect (req (wait-for (forfeit state side target)
                                              (effect-completed state side (make-result eid cost-name))))}
                      card nil)
    cost-name))

(defn pay-trash
  "Trash a card as part of paying for a card or ability"
  ;; If multiples needed in future likely prompt-select needs work to take a function
  ;; instead of an ability
  ([state side eid card type amount select-fn] (pay-trash state side eid card type amount select-fn nil))
  ([state side eid card type amount select-fn args]
   (let [cost-name (cost-names amount type)]
     (continue-ability state side
                       {:prompt (str "Choose a " type " to trash")
                        :choices {:max amount
                                  :req select-fn}
                        :async true
                        :effect (req (wait-for (trash state side target (merge args {:unpreventable true}))
                                               (effect-completed state side (make-result eid cost-name))))}
                       card nil)
     cost-name)))

(defn pay-damage
  "Suffer a damage as part of paying for a card or ability"
  [state side eid type amount]
  (let [cost-name (cost-names amount type)]
    (damage state side eid type amount {:unpreventable true})
    cost-name))

(defn pay-shuffle-installed-to-stack
  "Shuffle installed runner card(s) into the stack as part of paying for a card or ability"
  [state side eid card amount]
  (let [cost-name (cost-names amount :shuffle-installed-to-stack)]
    (continue-ability state :runner
                    {:prompt (str "Choose " amount " " (pluralize "card" amount) " to shuffle into the stack")
                     :choices {:max amount
                               :all true
                               :req #(and (installed? %) (= (:side %) "Runner"))}
                     :async true
                     :effect (req
                               (doseq [c targets]
                                 (move state :runner c :deck))
                               (system-msg state :runner
                                           (str "shuffles " (join ", " (map :title targets))
                                                " into their stack"))
                               (shuffle! state :runner :deck)
                               (effect-completed state side (make-result eid cost-name)))}
                    card nil)
    cost-name))

(defn- complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument.
  Helper function for cost-handler"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  result)

(defn- cost-handler
  "Calls the relevant function for a cost depending on the keyword passed in"
  ([state side card action costs cost] (cost-handler state side (make-eid state) card action costs cost))
  ([state side eid card action costs cost]
   (case (first cost)
     :click (let [a (first (keep :action action))]
              (when (not= a :steal-cost)
                ;; do not create an undo state if click is being spent due to a steal cost (eg. Ikawah Project)
                (swap! state assoc :click-state (dissoc @state :log)))
              (trigger-event state side
                             (if (= side :corp) :corp-spent-click :runner-spent-click)
                             a (:click (into {} costs)))
              (swap! state assoc-in [side :register :spent-click] true)
              (complete-with-result state side eid (deduct state side cost)))
     :forfeit (pay-forfeit state side eid card (second cost))
     :hardware (pay-trash state side eid card "piece of hardware" (second cost) (every-pred installed? #(is-type? % :hardware) (complement facedown?)))
     :program (pay-trash state side eid card "program" (second cost) (every-pred installed? #(is-type? % :program) (complement facedown?)))
     :resource (pay-trash state side eid card "resource" (second cost) (every-pred installed? #(is-type? % :resource) (complement facedown?)))

     ;; Connection
     :connection (pay-trash state side eid card "connection" (second cost) (every-pred installed? #(has-subtype? % "Connection") (complement facedown?)))

     ;; Rezzed ICE
     :ice (pay-trash state :corp eid card "rezzed ICE" (second cost) (every-pred rezzed? ice?) {:cause :ability-cost :keep-server-alive true})

     :tag (complete-with-result state side eid (deduct state :runner cost))
     :net-damage (pay-damage state side eid :net (second cost))
     :mill (complete-with-result state side eid (mill state side (second cost)))

     ;; Shuffle installed runner cards into the stack (eg Degree Mill)
     :shuffle-installed-to-stack (pay-shuffle-installed-to-stack state side eid card (second cost))

     ;; Else
     (let [[type amount] cost]
       (swap! state update-in [:stats side :spent type] (fnil + 0) amount)
       (complete-with-result state side eid (deduct state side cost))))))

(defn pay
  "Deducts each cost from the player.
  args format as follows with each being optional ([:click 1 :credit 0] [:forfeit] {:action :corp-click-credit})
  The map with :action was added for Jeeves so we can log what each click was used on"
  [state side card & args]
  (let [raw-costs (not-empty (remove map? args))
        action (not-empty (filter map? args))]
    (when-let [costs (apply can-pay? state side (:title card) raw-costs)]
        (->> costs
             (map (partial cost-handler state side (make-eid state) card action costs))
             (filter some?)
             (interpose " and ")
             (apply str)))))

(defn- pay-sync-next
  [state side eid costs card action msgs]
  (if (empty? costs)
    (effect-completed state side (make-result eid msgs))
    (wait-for (cost-handler state side card action costs (first costs))
              (pay-sync-next state side eid (next costs) card action (conj msgs async-result)))))

(defn pay-sync
  "Same as pay, but awaitable. "
  [state side eid card & args]
  (let [raw-costs (not-empty (remove map? args))
        action (not-empty (filter map? args))]
    (if-let [costs (apply can-pay? state side (:title card) raw-costs)]
      (wait-for (pay-sync-next state side costs card action [])
                (effect-completed state side
                                  (make-result eid (->> async-result
                                                        (filter some?)
                                                        (join " and ")))))
      (effect-completed state side (make-result eid nil)))))

(defn gain [state side & args]
  (doseq [[type amount] (partition 2 args)]
    (cond
      ;; amount is a map, merge-update map
      (map? amount)
      (doseq [[subtype amount] amount]
        (swap! state update-in [side type subtype] (safe-inc-n amount))
        (swap! state update-in [:stats side :gain type subtype] (fnil + 0) amount))

      ;; Default cases for the types that expect a map
      (#{:hand-size :memory} type)
      (gain state side type {:mod amount})

      ;; Default case for tag is `:base`
      (#{:tag} type)
      (gain state side type {:base amount})

      ;; Else assume amount is a number and try to increment type by it.
      :else
      (do (swap! state update-in [side type] (safe-inc-n amount))
          (swap! state update-in [:stats side :gain type] (fnil + 0 0) amount)))))

(defn lose [state side & args]
  (doseq [r (partition 2 args)]
    (trigger-event state side (if (= side :corp) :corp-loss :runner-loss) r)
    (if (= (last r) :all)
      (do (swap! state assoc-in [side (first r)] 0)
          (swap! state update-in [:stats side :lose (first r)] (fnil + 0) (get-in @state [side (first r)])))
      (do (when (number? (second r))
            (swap! state update-in [:stats side :lose (first r)] (fnil + 0) (second r)))
          (deduct state side r)))))

(defn gain-credits
  "Utility function for triggering events"
  [state side amount & args]
  (when (and amount
             (pos? amount))
    (gain state side :credit amount)
    (let [kw (keyword (str (name side) "-credit-gain"))]
      (apply trigger-event-sync state side (make-eid state) kw args))))

(defn lose-credits
  "Utility function for triggering events"
  [state side amount & args]
  (when (and amount
             (or (= :all amount)
                 (pos? amount)))
    (lose state side :credit amount)
    (let [kw (keyword (str (name side) "-credit-loss"))]
      (apply trigger-event-sync state side (make-eid state) kw args))))

(defn play-cost-bonus [state side costs]
  (swap! state update-in [:bonus :play-cost] #(merge-costs (concat % costs))))

(defn play-cost [state side card all-cost]
  (vec (map #(if (keyword? %) % (max % 0))
            (-> (concat all-cost (get-in @state [:bonus :play-cost])
                        (when-let [playfun (:play-cost-bonus (card-def card))]
                          (playfun state side (make-eid state) card nil)))
                merge-costs flatten))))

(defn rez-cost-bonus [state side n]
  (swap! state update-in [:bonus :cost] (fnil #(+ % n) 0)))

(defn get-rez-cost-bonus [state side]
  (get-in @state [:bonus :cost] 0))

(defn rez-cost [state side {:keys [cost] :as card}]
  (when-not (nil? cost)
    (-> (if-let [rezfun (:rez-cost-bonus (card-def card))]
          (+ cost (rezfun state side (make-eid state) card nil))
          cost)
        (+ (get-rez-cost-bonus state side))
        (max 0))))

(defn run-cost-bonus [state side n]
  (swap! state update-in [:bonus :run-cost] #(merge-costs (concat % n))))

(defn click-run-cost-bonus [state side & n]
  (swap! state update-in [:bonus :click-run-cost] #(merge-costs (concat % n))))

(defn trash-cost-bonus [state side n]
  (swap! state update-in [:bonus :trash] (fnil #(+ % n) 0)))

(defn trash-cost [state side {:keys [trash] :as card}]
  (when-not (nil? trash)
    (-> (if-let [trashfun (:trash-cost-bonus (card-def card))]
          (+ trash (trashfun state side (make-eid state) card nil))
          trash)
        (+ (get-in @state [:bonus :trash] 0))
        (max 0))))

(defn modified-trash-cost
  "Returns the numbe of credits required to trash the given card, after modification effects.
  Allows cards like Product Recall to pre-calculate trash costs without manually triggering the effects."
  [state side card]
  (swap! state update-in [:bonus] dissoc :trash)
  (trigger-event state side :pre-trash card)
  (let [tcost (trash-cost state side card)]
    (swap! state update-in [:bonus] dissoc :trash)
    tcost))

(defn install-cost-bonus [state side n]
  (swap! state update-in [:bonus :install-cost] #(merge-costs (concat % n))))

(defn ignore-install-cost [state side b]
  (swap! state assoc-in [:bonus :ignore-install-cost] b))

(defn ignore-install-cost? [state side]
  (get-in @state [:bonus :ignore-install-cost]))

(defn clear-install-cost-bonus [state side]
  (swap! state update-in [:bonus] dissoc :install-cost)
  (swap! state update-in [:bonus] dissoc :ignore-install-cost))

(defn install-cost
  [state side card all-cost]
  (vec (map #(if (keyword? %) % (max % 0))
            (-> (concat all-cost
                        (get-in @state [:bonus :install-cost])
                        (when-let [instfun (:install-cost-bonus (card-def card))]
                          (instfun state side (make-eid state) card nil)))
                merge-costs
                flatten))))

(defn modified-install-cost
  "Returns the number of credits required to install the given card, after modification effects including
  the argument 'additional', which is a vector of costs like [:credit -1]. Allows cards like
  Street Peddler to pre-calculate install costs without actually triggering the install."
  ([state side card] (modified-install-cost state side card nil))
  ([state side card additional]
   (trigger-event state side :pre-install card)
   (let [cost (install-cost state side card (merge-costs (concat additional [:credit (:cost card)])))]
     (swap! state update-in [:bonus] dissoc :install-cost)
     (swap! state update-in [:bonus] dissoc :ignore-install-cost)
     cost)))
