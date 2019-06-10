(in-ns 'game.core)

(declare forfeit prompt! toast damage mill installed? is-type? is-scored? system-msg
         facedown? make-result unknown->kw discard-from-hand card-str trash trash-cards
         complete-with-result all-installed-runner-type pick-credit-providing-cards all-active
         eligible-pay-credit-cards corp? runner? in-hand?)

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
          (swap! state update-in [:runner :run-credit] (sub->0 value)))
        (cond
          (= :credit attr)
          (str "pays " value " [Credits]")
          (= :click attr)
          (str "spends " (->> "[Click]" repeat (take value) (apply str)))))))

(defn flag-stops-pay?
  "Checks installed cards to see if payment type is prevented by a flag"
  [state side type]
  (let [flag (keyword (str "cannot-pay-" (name type)))]
    (some #(card-flag? % flag true) (all-active-installed state side))))

(defn total-available-credits
  [state side eid card]
  (+ (get-in @state [side :credit])
     (->> (eligible-pay-credit-cards state side eid card)
          (map #(+ (get-counters % :recurring)
                   (get-counters % :credit)))
          (reduce +))))

(defn toast-msg-helper
  "Creates a toast message for given cost and title if applicable"
  [state side eid card cost]
  (let [cost-type (first cost)
        amount (last cost)
        computer-says-no "Unable to pay"]
    (cond

      (flag-stops-pay? state side cost-type)
      computer-says-no

      (not (or (and (#{:memory :net :meat :brain} cost-type) (<= amount (count (get-in @state [:runner :hand]))))
               (and (= cost-type :forfeit) (<= 0 (- (count (get-in @state [side :scored])) amount)))
               (and (= cost-type :mill) (<= 0 (- (count (get-in @state [side :deck])) amount)))
               (and (= cost-type :trash-from-hand) (<= 0 (- (count (get-in @state [side :hand])) amount)))
               (and (= cost-type :randomly-trash-from-hand) (<= 0 (- (count (get-in @state [side :hand])) amount)))
               (and (= cost-type :tag) (<= 0 (- (get-in @state [:runner :tag :base]) amount)))
               (and (= cost-type :ice) (<= 0 (- (count (filter (every-pred rezzed? ice?) (all-installed state :corp))) amount)))
               (and (= cost-type :hardware) (<= 0 (- (count (all-installed-runner-type state :hardware)) amount)))
               (and (= cost-type :program) (<= 0 (- (count (all-installed-runner-type state :program)) amount)))
               (and (= cost-type :resource) (<= 0 (- (count (all-installed-runner-type state :resource)) amount)))
               (and (= cost-type :connection)
                    (<= 0 (- (count (filter #(has-subtype? % "Connection") (all-active-installed state :runner))) amount)))
               (and (= cost-type :shuffle-installed-to-stack) (<= 0 (- (count (all-installed state :runner)) amount)))
               (and (= cost-type :click) (<= 0 (- (get-in @state [side :click]) amount)))
               (and (= cost-type :credit)
                    (or (<= 0 (- (get-in @state [side :credit]) amount))
                        (<= 0 (- (total-available-credits state side eid card) amount))))))
      computer-says-no)))

(defn add-default-to-costs
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

(defn merge-costs
  "Combines disparate costs into a single cost per type, except for damage.
  Damage is not merged as it needs to be individual."
  [costs]
  (let [clean-costs (add-default-to-costs costs)
        partition-fn (juxt remove filter)
        [plain-costs damage-costs] (partition-fn
                                     #(#{:net :meat :brain} (first %))
                                     clean-costs)
        reduce-fn (fn [cost-map [cost-type value]]
                    (update cost-map cost-type (fnil + 0 0) value))]
    (mapv vec (concat (reduce reduce-fn {} plain-costs) damage-costs))))

(defn can-pay?
  "Returns false if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  ([state side title args] (can-pay? state side (make-eid state) nil title args))
  ([state side eid card title & args]
   (let [costs (merge-costs (remove #(or (nil? %) (map? %)) args))
         cost-msg (some #(toast-msg-helper state side eid card %) costs)]
     ;; no cost message - hence can pay
     (if-not cost-msg
       costs
       (when title (toast state side (str cost-msg " for " title ".")) false)))))

(defn- complete-with-result
  "Calls `effect-complete` with `make-result` and also returns the argument.
  Helper function for cost-handler"
  [state side eid result]
  (effect-completed state side (make-result eid result))
  result)

(defn cost-names
  "Converts a cost (value attribute pair) to a string for printing"
  [attr value]
  (when (and (number? value)
             (pos? value))
    (case attr
      :credit (str "Pay " value " [Credits]")
      :click (str "Spend " (->> "[Click]" repeat (take value) (apply str)))
      :forfeit (str "Forfeit " (quantify value "Agenda"))
      :hardware (str "Trash " (quantify value "installed hardware" ""))
      :program (str "Trash " (quantify value "installed program"))
      :resource (str "Trash " (quantify value "installed resource"))
      :connection (str "Trash " (quantify value "installed connection resource"))
      :ice (str "Trash " (quantify value "installed rezzed ICE" ""))
      :shuffle-installed-to-stack (str "Shuffle " (quantify value "installed card") " into the stack")
      :net (str "Suffer " (quantify value "net damage" ""))
      :meat (str "Suffer " (quantify value "meat damage" ""))
      :brain (str "Suffer " (quantify value "brain damage" ""))
      :mill (str "Trash " (quantify value "card") " from the top of your deck")
      :discard (str "Trash " (quantify value "card") " randomly from your hand")
      (str "Pay " (quantify value (name attr))))))

(defn build-cost-str
  "Gets the complete cost-str for specified costs"
  [costs]
  (->> costs
       (map #(apply cost-names %))
       (filter some?)
       (interpose " and ")
       (apply str)))

(defn build-spend-msg
  "Constructs the spend message for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg cost-str verb nil))
  ([cost-str verb verb2]
   (if (or (not (instance? String cost-str))
           (= "" cost-str))
     (str (or verb2 (str verb "s")) " ")
     (str cost-str " to " verb " "))))

(defn pay-forfeit
  "Forfeit agenda as part of paying for a card or ability
  Amount is always 1 but can be extend if we ever need more than a single forfeit"
  ;; If multiples needed in future likely prompt-select needs work to take a function
  ;; instead of an ability
  [state side eid card amount]
  (continue-ability state side
                    {:prompt "Choose an Agenda to forfeit"
                     :async true
                     :choices {:max amount
                               :req #(is-scored? state side %)}
                     :effect (req (wait-for (forfeit state side target {:msg false})
                                            (complete-with-result
                                              state side eid
                                              (str "forfeits " (quantify amount "agenda")
                                                   " (" (:title target) ")"))))}
                    card nil))

(defn pay-trash
  "Trash a card as part of paying for a card or ability"
  ([state side eid card card-type amount select-fn] (pay-trash state side eid card card-type amount select-fn nil))
  ([state side eid card card-type amount select-fn args]
   (continue-ability state side
                     {:prompt (str "Choose " (quantify amount card-type) " to trash")
                      :choices {:all true
                                :max amount
                                :req select-fn}
                      :async true
                      :effect (req (wait-for (trash-cards state side targets (merge args {:unpreventable true}))
                                             (complete-with-result
                                               state side eid
                                               (str "trashes " (quantify amount (or (:plural args) card-type))
                                                    " (" (join ", " (map #(card-str state %) targets)) ")"))))}
                     card nil)))

(defn pay-damage
  "Suffer a damage as part of paying for a card or ability"
  [state side eid dmg-type amount]
  (wait-for (damage state side dmg-type amount {:unpreventable true})
            (complete-with-result
              state side eid
              (str "suffers " amount " " (name dmg-type) " damage"))))

(defn pay-shuffle-installed-to-stack
  "Shuffle installed runner cards into the stack as part of paying for a card or ability"
  [state side eid card amount]
  (continue-ability state :runner
                    {:prompt (str "Choose " (quantify amount "card") " to shuffle into the stack")
                     :choices {:max amount
                               :all true
                               :req #(and (installed? %)
                                          (= (:side %) "Runner"))}
                     :async true
                     :effect (req (doseq [c targets]
                                    (move state :runner c :deck))
                                  (shuffle! state :runner :deck)
                                  (complete-with-result
                                    state side eid
                                    (str "shuffles " (quantify amount "card")
                                         " (" (join ", " (map :title targets)) ")"
                                         " into their stack")))}
                    card nil))

(defn pay-mill
  [state side eid amount]
  (let [cards (take amount (get-in @state [side :deck]))]
    (wait-for (trash-cards state side cards {:unpreventable true :seen false})
              (complete-with-result
                state side eid
                (str "trashes " (quantify amount "card") " from the top of "
                     (if (= :corp side) "R&D" "the stack"))))))

(defn pay-trash-from-hand
  "Randomly trash a card from hand as part of a cost"
  [state side eid amount]
  (let [select-fn #(and ((if (= :corp side) corp? runner?) %)
                        (in-hand? %))
        hand (if (= :corp side) "HQ" "their grip")]
    (continue-ability state side
                      {:prompt (str "Choose " (quantify amount (str "card in " hand)) " to trash")
                       :choices {:all true
                                 :max amount
                                 :req select-fn}
                       :async true
                       :effect (req (wait-for (trash-cards state side targets {:unpreventable true :seen false})
                                              (complete-with-result
                                                state side eid
                                                (str "trashes " (quantify amount "card")
                                                     " (" (join ", " (map #(card-str state %) targets)) ")"
                                                     " from " hand))))}
                      nil nil)))

(defn pay-randomly-trash-from-hand
  "Randomly trash a card from hand as part of a cost"
  [state side eid amount]
  (let [cards (take amount (shuffle (get-in @state [side :hand])))]
    (wait-for (trash-cards state side cards {:unpreventable true :seen false})
              (complete-with-result
                state side eid
                (str "trashes " (quantify amount "card") " randomly from "
                     (if (= :corp side) "HQ" "the grip"))))))

(defn all-active-pay-credit-cards
  [state side eid card]
  (filter #(when-let [pc (-> % card-def :interactions :pay-credits)]
             (if (:req pc)
               ((:req pc) state side eid % [card])
               true))
          (all-active state side)))

(defn eligible-pay-credit-cards
  [state side eid card]
  (filter
    #(case (-> % card-def :interactions :pay-credits :type)
       :recurring
       (pos? (get-counters (get-card state %) :recurring))
       :credit
       (pos? (get-counters (get-card state %) :credit))
       :custom
       ((-> % card-def :interactions :pay-credits :req) state side eid % [card]))
    (all-active-pay-credit-cards state side eid card)))

(defn pay-credits
  [state side eid card amount]
  (let [provider-func #(eligible-pay-credit-cards state side eid card)]
    (if (and (pos? amount)
             (pos? (count (provider-func))))
      (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid amount) card nil)
                (swap! state update-in [:stats side :spent :credit] (fnil + 0) amount)
                (complete-with-result state side eid (str "pays " (:msg async-result))))
      (do
        (swap! state update-in [:stats side :spent :credit] (fnil + 0) amount)
        (complete-with-result state side eid (deduct state side [:credit amount]))))))

(defn- cost-handler
  "Calls the relevant function for a cost depending on the keyword passed in"
  ([state side card action costs cost] (cost-handler state side (make-eid state) card action costs cost))
  ([state side eid card action costs [cost-type amount]]
   (case cost-type
     :click (let [a (first (keep :action action))]
              (when (not= a :steal-cost)
                ;; do not create an undo state if click is being spent due to a steal cost (eg. Ikawah Project)
                (swap! state assoc :click-state (dissoc @state :log)))
              (trigger-event state side
                             (if (= side :corp) :corp-spent-click :runner-spent-click)
                             a (:click (into {} costs)))
              (swap! state assoc-in [side :register :spent-click] true)
              (complete-with-result state side eid (deduct state side [cost-type amount])))

     :forfeit (pay-forfeit state side eid card amount)

     ;; Trash installed cards
     :hardware (pay-trash state side eid card "piece of hardware" amount
                          (every-pred installed? hardware? (complement facedown?))
                          {:plural "pieces of hardware"})
     :program (pay-trash state side eid card "program" amount
                         (every-pred installed? program? (complement facedown?)))
     :resource (pay-trash state side eid card "resource" amount
                          (every-pred installed? resource? (complement facedown?)))
     :connection (pay-trash state side eid card "connection" amount
                            (every-pred installed? #(has-subtype? % "Connection") (complement facedown?)))
     :ice (pay-trash state :corp eid card "rezzed ICE" amount
                     (every-pred rezzed? ice?)
                     {:cause :ability-cost
                      :keep-server-alive true
                      :plural "rezzed ICE"})

     ;; Suffer damage
     :net (pay-damage state side eid :net amount)
     :meat (pay-damage state side eid :meat amount)
     :brain (pay-damage state side eid :brain amount)

     ;; Discard cards from deck or hand
     :mill (pay-mill state side eid amount)
     :trash-from-hand (pay-trash-from-hand state side eid amount)
     :randomly-trash-from-hand (pay-randomly-trash-from-hand state side eid amount)

     ;; Shuffle installed runner cards into the stack (eg Degree Mill)
     :shuffle-installed-to-stack (pay-shuffle-installed-to-stack state side eid card amount)

     ;; Pay credits
     :credit (pay-credits state side eid card amount)

     ;; Else
     (do
       (swap! state update-in [:stats side :spent cost-type] (fnil + 0) amount)
       (complete-with-result state side eid (deduct state side [cost-type amount]))))))

(defn pay
  "Deducts each cost from the player.
  args format as follows with each being optional ([:click 1 :credit 0] [:forfeit] {:action :corp-click-credit})
  The map with :action was added for Jeeves so we can log what each click was used on"
  [state side card & args]
  (let [raw-costs (not-empty (remove map? args))
        action (not-empty (filter map? args))]
    (when-let [costs (can-pay? state side (:title card) raw-costs)]
        (->> costs
             (map (partial cost-handler state side (make-eid state) card action costs))
             (filter some?)
             (interpose " and ")
             (apply str)))))

(defn- pay-sync-next
  [state side eid costs card action msgs]
  (if (empty? costs)
    (effect-completed state side (make-result eid msgs))
    (wait-for (cost-handler state side (make-eid state eid) card action costs (first costs))
              (pay-sync-next state side eid (next costs) card action (conj msgs async-result)))))

(defn pay-sync
  "Same as pay, but awaitable."
  [state side eid card & args]
  (let [raw-costs (not-empty (remove map? args))
        action (not-empty (filter map? args))]
    (if-let [costs (can-pay? state side eid card (:title card) raw-costs)]
      (wait-for (pay-sync-next state side (make-eid state eid) costs card action [])
                (complete-with-result state side eid (->> async-result
                                                          (filter some?)
                                                          (join " and "))))
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
                merge-costs
                flatten))))

(defn rez-cost-bonus [state side n]
  (swap! state update-in [:bonus :cost] (fnil #(+ % n) 0)))

(defn get-rez-cost-bonus [state side]
  (get-in @state [:bonus :cost] 0))

(defn rez-additional-cost-bonus
  [state side n]
  (swap! state update-in [:bonus :rez :additional-cost] #(merge-costs (concat % n))))

(defn get-rez-additional-cost-bonus
  [state side]
  (get-in @state [:bonus :rez :additional-cost]))

(defn rez-cost [state side {:keys [cost] :as card}]
  (when-not (nil? cost)
    (-> (if-let [rezfun (:rez-cost-bonus (card-def card))]
          (+ cost (rezfun state side (make-eid state) card nil))
          cost)
        (+ (get-rez-cost-bonus state side))
        (max 0))))

(defn run-additional-cost-bonus
  [state side & n]
  (swap! state update-in [:bonus :run-cost :additional-cost] #(merge-costs (concat % n))))

(defn run-costs
  "Get a list of all costs required to run a server, including additional costs. If card is :click-run, assume run is made by spending a click, and include the assumed click in the cost list."
  [state server card]
  (let [server (unknown->kw server)
        click-run-cost (when (= card :click-run) [:click 1])
        global-costs (get-in @state [:bonus :run-cost :additional-cost])
        server-costs (get-in @state [:corp :servers server :additional-cost])]
    (merge-costs (concat click-run-cost global-costs server-costs))))

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
