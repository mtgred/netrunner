(in-ns 'game.core)

(declare forfeit prompt! damage mill is-scored? system-msg
         unknown->kw discard-from-hand card-str trash trash-cards
         all-installed-runner-type pick-credit-providing-cards all-active
         eligible-pay-credit-cards lose-tags)

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
          (swap! state update-in [:runner :run-credit] (sub->0 value)))
        (cond
          (= :credit attr)
          (str "pays " value " [Credits]")
          (= :click attr)
          (str "spends " (->> "[Click]" repeat (take value) (apply str)))))))

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

(defn merge-costs
  "Combines disparate costs into a single cost per type, except for damage.
  Damage is not merged as it needs to be individual."
  ([costs] (merge-costs costs false))
  ([costs remove-zero-credit-cost]
   (let [clean-costs (add-default-to-costs costs)
         plain-costs (remove #(#{:net :meat :brain} (first %)) clean-costs)
         damage-costs (filter #(#{:net :meat :brain} (first %)) clean-costs)
         reduce-fn (fn [cost-map [cost-type value]]
                     (update cost-map cost-type (fnil + 0 0) value))
         remove-fn (if remove-zero-credit-cost
                     #(and (= :credit (first %))
                           (zero? (second %)))
                     (fn [x] false))]
     (remove remove-fn (mapv vec (concat (reduce reduce-fn {} plain-costs) damage-costs))))))

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
                   (get-counters % :credit)))
          (reduce +))))

(defn- can-pay-impl
  [state side eid card [cost-type amount]]
  (if (flag-stops-pay? state side cost-type)
    false
    (case cost-type
      :memory true
      :credit (or (<= 0 (- (get-in @state [side :credit]) amount))
                  (<= 0 (- (total-available-credits state side eid card) amount)))
      :click (<= 0 (- (get-in @state [side :click]) amount))
      :trash (installed? (get-card state card))
      :forfeit (<= 0 (- (count (get-in @state [side :scored])) amount))
      :forfeit-self (is-scored? state side (get-card state card))
      ; Can't use count-tags as we can't remove additional tags
      :tag (<= 0 (- (get-in @state [:runner :tag :base] 0) amount))
      :return-to-hand (active? (get-card state card))
      :remove-from-game (active? (get-card state card))
      :rfg-program (<= 0 (- (count (all-installed-runner-type state :program)) amount))
      :installed (<= 0 (- (count (all-installed state side)) amount))
      :hardware (<= 0 (- (count (all-installed-runner-type state :hardware)) amount))
      :program (<= 0 (- (count (all-installed-runner-type state :program)) amount))
      :resource (<= 0 (- (count (all-installed-runner-type state :resource)) amount))
      :connection (<= 0 (- (count (filter #(has-subtype? % "Connection") (all-active-installed state :runner))) amount))
      :ice (<= 0 (- (count (filter (every-pred rezzed? ice?) (all-installed state :corp))) amount))
      (:net :meat :brain) (<= amount (count (get-in @state [:runner :hand])))
      :trash-from-deck (<= 0 (- (count (get-in @state [side :deck])) amount))
      (:trash-from-hand :randomly-trash-from-hand) (<= 0 (- (count (get-in @state [side :hand])) amount))
      :trash-program-from-grip (<= 0 (- (count (filter program? (get-in @state [:runner :hand]))) amount))
      :trash-entire-hand true
      :shuffle-installed-to-stack (<= 0 (- (count (all-installed state :runner)) amount))
      :any-agenda-counter (<= 0 (- (reduce + (map #(get-counters % :agenda) (get-in @state [:corp :scored]))) amount))
      (:advancement :agenda :power :virus) (<= 0 (- (get-counters card cost-type) amount))
      ;; default to cannot afford
      false)))

(defn can-pay?
  "Returns false if the player cannot pay the cost args, or a truthy map otherwise.
  If title is specified a toast will be generated if the player is unable to pay
  explaining which cost they were unable to pay."
  ([state side title args] (can-pay? state side (make-eid state) nil title args))
  ([state side eid card title & args]
   (let [remove-zero-credit-cost (and (= (:source-type eid) :corp-install)
                                      (not (ice? card)))
         costs (merge-costs (remove #(or (nil? %) (map? %)) args) remove-zero-credit-cost)]
     (if (every? #(can-pay-impl state side eid card %) costs)
       costs
       (when title
         (toast state side (str "Unable to pay for " title "."))
         false)))))

(defn build-spend-msg
  "Constructs the spend message for specified cost-str and verb(s)."
  ([cost-str verb] (build-spend-msg cost-str verb nil))
  ([cost-str verb verb2]
   (if (or (not (instance? String cost-str))
           (= "" cost-str))
     (str (or verb2 (str verb "s")) " ")
     (str cost-str " to " verb " "))))

(defn cost->label
  [[cost-type amount]]
  (when (and (number? amount)
             (not (neg? amount)))
    (case cost-type
      :credit (str amount " [Credits]")
      :click (->> "[Click]" repeat (take amount) (apply str))
      :trash "[trash]"
      :forfeit (str "forfeit " (quantify amount "Agenda"))
      :forfeit-self "forfeit this Agenda"
      :tag (str "remove " (quantify amount "tag"))
      :return-to-hand "return this card to your hand"
      :remove-from-game "remove this card from the game"
      :rfg-program (str "remove " (quantify amount "installed program") " from the game")
      :installed (str "trash " (quantify amount "installed card"))
      :hardware (str "trash " (quantify amount "installed hardware" ""))
      :program (str "trash " (quantify amount "installed program"))
      :resource (str "trash " (quantify amount "installed resource"))
      :connection (str "trash " (quantify amount "installed connection resource"))
      :ice (str "trash " (quantify amount "installed rezzed ICE" ""))
      :trash-from-deck (str "trash " (quantify amount "card") " from the top of your deck")
      :trash-from-hand (str "trash " (quantify amount "card") " from your hand")
      :randomly-trash-from-hand (str "trash " (quantify amount "card") " randomly from your hand")
      :trash-entire-hand "trash all cards in your hand"
      :trash-program-from-grip "trash a program in your graip"
      (:net :meat :brain) (str "suffer " (quantify amount (str (name cost-type) " damage") ""))
      (:advancement :agenda :power :virus) (if (< 1 amount)
                                             (quantify amount (str "hosted " (name cost-type) " counter"))
                                             (str "hosted " (name cost-type) " counter"))
      :shuffle-installed-to-stack (str "shuffle " (quantify amount "installed card") " into the stack")
      :any-agenda-counter "any agenda counter"
      (str (quantify amount (name cost-type))))))

(defn build-cost-label
  "Gets the complete cost-str for specified costs"
  [costs]
  (let [cost-string
        (->> costs
             merge-costs
             (map cost->label)
             (filter some?)
             (interpose ", ")
             (apply str))]
    (when (not (string/blank? cost-string))
      (capitalize cost-string))))

(defn make-label
  "Looks into an ability for :label, if it doesn't find it, capitalizes :msg instead."
  [ability]
  (let [label (or (:label ability)
                  (and (string? (:msg ability))
                       (capitalize (:msg ability)))
                  "")
        cost (:cost ability)]
    (cond
      (and (seq cost)
           (not (string/blank? label)))
      (str (build-cost-label cost) ": " (capitalize label))
      (not (string/blank? label))
      (capitalize label)
      :else
      label)))

(defn cost->string
  "Converts a cost (amount attribute pair) to a string for printing"
  [[cost-type amount]]
  (when (and (number? amount)
             (not (neg? amount)))
    (let [cost-string (cost->label [cost-type amount])]
      (cond
        (= :click cost-type) (str "spend " cost-string)
        (= :credit cost-type) (str "pay " cost-string)
        :else cost-string))))

(defn build-cost-string
  "Gets the complete cost-str for specified costs"
  ([costs] (build-cost-string costs cost->string))
  ([costs f]
   (let [cost-string
         (->> costs
              merge-costs
              (map f)
              (filter some?)
              (interpose " and ")
              (apply str))]
     (when (not (string/blank? cost-string))
       (capitalize cost-string)))))

;; Cost Handler functions

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

(defn pay-clicks
  [state side eid action costs cost-type amount]
  (let [a (first (keep :action action))]
    (when (not= a :steal-cost)
      ;; do not create an undo state if click is being spent due to a steal cost (eg. Ikawah Project)
      (swap! state assoc :click-state (dissoc @state :log)))
    (trigger-event state side
                   (if (= side :corp) :corp-spent-click :runner-spent-click)
                   a (:click (into {} costs)))
    (swap! state assoc-in [side :register :spent-click] true)
    (complete-with-result state side eid (deduct state side [cost-type amount]))))

(defn pay-trash
  "[Trash] cost as part of an ability"
  [state side eid card amount]
  (wait-for (trash state side card {:cause :ability-cost
                                    :unpreventable true})
            (complete-with-result state side eid (str "trashes " (:title card)))))

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

(defn pay-forfeit-self
  "Forfeit an agenda as part of paying for the ability on that agenda. (False Lead)"
  [state side eid card]
  (wait-for (forfeit state side card {:msg false})
            (complete-with-result
              state side eid
              (str "forfeits " (:title card)))))

(defn pay-tags
  "Removes a tag from the runner. (Keegan Lane)"
  [state side eid card amount]
  (wait-for (lose-tags state side amount)
            (complete-with-result
              state side eid
              (str "removes " (quantify amount "tag")))))

(defn pay-return-to-hand
  "Returns an installed card to the player's hand."
  [state side eid card]
  (move state side card :hand)
  (complete-with-result
    state side eid
    (str "returns " (:title card)
         " to " (if (= :corp side) "HQ" "their grip"))))

(defn pay-remove-from-game
  "Remove an installed card from the game."
  [state side eid card]
  (move state side card :rfg)
  (complete-with-result state side eid (str "removes " (:title card) " from the game")))

(defn pay-rfg-installed
  "Remove a card of a given kind from the game"
  [state side eid card card-type amount select-fn]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify amount card-type) " to remove from the game")
     :choices {:all true
               :max amount
               :req select-fn}
     :async true
     :effect (req (doseq [t targets]
                    (move state side (assoc-in t [:persistent :from-cid] (:cid card)) :rfg))
                  (complete-with-result
                    state side eid
                    (str "removes " (quantify amount card-type)
                         " from the game"
                         " (" (join ", " (map #(card-str state %) targets)) ")")))}
    card nil))

(defn pay-trash-installed
  "Trash a card as part of paying for a card or ability"
  ([state side eid card card-type amount select-fn] (pay-trash-installed state side eid card card-type amount select-fn nil))
  ([state side eid card card-type amount select-fn args]
   (continue-ability state side
                     {:prompt (str "Choose " (quantify amount card-type) " to trash")
                      :choices {:all true
                                :max amount
                                :req select-fn}
                      :async true
                      :priority 11
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

(defn pay-trash-from-deck
  [state side eid amount]
  (let [cards (take amount (get-in @state [side :deck]))]
    (wait-for (trash-cards state side cards {:unpreventable true :seen false})
              (complete-with-result
                state side eid
                (str "trashes " (quantify amount "card") " from the top of "
                     (if (= :corp side) "R&D" "the stack"))))))

(defn pay-trash-from-hand
  "Trash a card from hand as part of a cost"
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

(defn pay-trash-entire-hand
  [state side eid]
  (let [cards (get-in @state [side :hand])]
    (wait-for (trash-cards state side cards {:unpreventable true})
              (complete-with-result
                state side eid
                (str "trashes all (" (count cards) ") cards in "
                    (if (= :runner side) "their grip" "HQ")
                    (when (and (= :runner side)
                               (pos? (count cards)))
                      (str " (" (map :title cards) ")")))))))

(defn pay-trash-program-from-grip
  [state side eid amount]
  (continue-ability
    state side
    {:prompt "Choose a program to trash from your grip"
     :async true
     :choices {:all true
               :max amount
               :req #(and (program? %)
                          (in-hand? %))}
     :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                            (complete-with-result
                              state side eid
                              (str "trashes " (quantify amount "card")
                                   " (" (join ", " (map :title targets)) ")"
                                   " from the grip"))))}
    nil nil))

(defn pay-shuffle-installed-to-stack
  "Shuffle installed runner cards into the stack as part of paying for a card or ability"
  [state side eid card amount]
  (continue-ability state :runner
                    {:prompt (str "Choose " (quantify amount "card") " to shuffle into the stack")
                     :choices {:max amount
                               :all true
                               :req #(and (installed? %)
                                          (runner? %))}
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

(defn pay-any-agenda-counter
  [state side eid]
  (continue-ability
    state side
    {:prompt "Select an agenda with a counter"
     :choices {:req #(and (agenda? %)
                          (is-scored? state side %)
                          (pos? (get-counters % :agenda)))}
     :effect (effect (add-counter target :agenda -1)
                     (trigger-event :agenda-counter-spent (get-card state target))
                     (complete-with-result
                       eid (str "spends an agenda counter from on " (:title target))))}
    nil nil))

(defn pay-counter
  [state side eid card counter amount]
  (update! state side
           (if (= counter :advancement)
             (update card :advance-counter - amount)
             (update-in card [:counter counter] - amount)))
  (when (agenda? card)
    (trigger-event state side :agenda-counter-spent (get-card state card)))
  (complete-with-result
    state side eid
    (str "spends "
         (if (< 1 amount)
           (quantify amount (str "hosted " (name counter) " counter"))
           (str "a hosted " (name counter) " counter"))
         " from on " (:title card))))

(defn- cost-handler
  "Calls the relevant function for a cost depending on the keyword passed in"
  ([state side card action costs cost] (cost-handler state side (make-eid state) card action costs cost))
  ([state side eid card action costs [cost-type amount]]
   (case cost-type
     ; Symbols
     :credit (pay-credits state side eid card amount)
     :click (pay-clicks state side eid action costs cost-type amount)
     :trash (pay-trash state side eid card amount)

     ; Forfeit an agenda
     :forfeit (pay-forfeit state side eid card amount)
     :forfeit-self (pay-forfeit-self state side eid card)

     ; Remove a tag from the runner
     :tag (pay-tags state side eid card amount)

     ; Return installed card to hand
     :return-to-hand (pay-return-to-hand state side eid card)

     ; Remove card from the game
     :remove-from-game (pay-remove-from-game state side eid card)
     :rfg-program (pay-rfg-installed state side eid card "installed program" amount
                                     (every-pred installed? program? (complement facedown?)))

     ;; Trash installed cards
     :installed (pay-trash-installed state side eid card "installed card" amount runner?)
     :hardware (pay-trash-installed state side eid card "piece of hardware" amount
                                    (every-pred installed? hardware? (complement facedown?))
                                    {:plural "pieces of hardware"})
     :program (pay-trash-installed state side eid card "program" amount
                                   (every-pred installed? program? (complement facedown?)))
     :resource (pay-trash-installed state side eid card "resource" amount
                                    (every-pred installed? resource? (complement facedown?)))
     :connection (pay-trash-installed state side eid card "connection" amount
                                      (every-pred installed? #(has-subtype? % "Connection") (complement facedown?)))
     :ice (pay-trash-installed state :corp eid card "rezzed ICE" amount
                               (every-pred rezzed? ice?)
                               {:keep-server-alive true
                                :plural "rezzed ICE"})

     ;; Suffer damage
     :net (pay-damage state side eid :net amount)
     :meat (pay-damage state side eid :meat amount)
     :brain (pay-damage state side eid :brain amount)

     ;; Discard cards from deck or hand
     :trash-from-deck (pay-trash-from-deck state side eid amount)
     :trash-from-hand (pay-trash-from-hand state side eid amount)
     :randomly-trash-from-hand (pay-randomly-trash-from-hand state side eid amount)
     :trash-entire-hand (pay-trash-entire-hand state side eid)
     :trash-program-from-grip (pay-trash-program-from-grip state side eid amount)

     ;; Shuffle installed runner cards into the stack (eg Degree Mill)
     :shuffle-installed-to-stack (pay-shuffle-installed-to-stack state side eid card amount)

     ;; Spend an agenda counter on another card
     :any-agenda-counter (pay-any-agenda-counter state side eid)

     ;; Counter costs
     :advancement (pay-counter state side eid card :advancement amount)
     :agenda (pay-counter state side eid card :agenda amount)
     :power (pay-counter state side eid card :power amount)
     :virus (pay-counter state side eid card :virus amount)

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

      ;; Default case for tags and bad publicity is `:base`
      (#{:tag :bad-publicity} type)
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
  "Get a list of all costs required to run a server, including additional costs. If click-run, run is made by spending a click, and include the assumed click in the cost list."
  [state server {:keys [click-run]}]
  (let [server (unknown->kw server)
        click-run-cost (when click-run [:click 1])
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
  (swap! state update-in [:bonus] dissoc :install-cost :ignore-install-cost))

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
