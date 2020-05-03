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
      (do (swap! state assoc-in [side cost-type] 0)
          (swap! state update-in [:stats side :lose cost-type] (fnil + 0) (get-in @state [side cost-type])))
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
     (mapv vec (remove remove-fn (concat (reduce reduce-fn {} plain-costs) damage-costs))))))

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
      :trash-hardware-from-hand (<= 0 (- (count (filter hardware? (get-in @state [:runner :hand]))) amount))
      :trash-program-from-hand (<= 0 (- (count (filter program? (get-in @state [:runner :hand]))) amount))
      :trash-resource-from-hand (<= 0 (- (count (filter resource? (get-in @state [:runner :hand]))) amount))
      :trash-entire-hand true
      :shuffle-installed-to-stack (<= 0 (- (count (all-installed state :runner)) amount))
      :add-installed-to-bottom-of-deck (<= 0 (- (count (all-installed state side)) amount))
      :any-agenda-counter (<= 0 (- (reduce + (map #(get-counters % :agenda) (get-in @state [:corp :scored]))) amount))
      (:advancement :agenda :power) (<= 0 (- (get-counters card cost-type) amount))
      (:virus :any-virus-counter) (or (<= 0 (- (get-counters card :virus) amount))
                                      (<= 0 (- (number-of-virus-counters state) amount)))
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
      :trash-hardware-from-hand (str "trash " (quantify amount "piece") " of hardware in your hand")
      :trash-program-from-hand (str "trash " (quantify amount "program") " in your hand")
      :trash-resource-from-hand (str "trash " (quantify amount "resource") " in your hand")
      (:net :meat :brain) (str "suffer " (quantify amount (str (name cost-type) " damage") ""))
      :shuffle-installed-to-stack (str "shuffle " (quantify amount "installed card") " into the stack")
      :add-installed-to-bottom-of-deck (str "add " (quantify amount "installed card") " to the bottom of the stack")
      :any-agenda-counter "any agenda counter"
      :any-virus-counter (str "any " (quantify amount "virus counter"))
      (:advancement :agenda :power :virus) (if (< 1 amount)
                                             (quantify amount (str "hosted " (name cost-type) " counter"))
                                             (str "hosted " (name cost-type) " counter"))
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
  "All credit-based costs should go through this function"
  [state side eid card amount]
  (let [provider-func #(eligible-pay-credit-cards state side eid card)]
    (cond
      (and (pos? amount)
           (pos? (count (provider-func))))
      (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid amount) card nil)
                (swap! state update-in [:stats side :spent :credit] (fnil + 0) amount)
                (complete-with-result state side eid (str "pays " (:msg async-result))))
      (pos? amount)
      (do (lose state side :credit amount)
          (complete-with-result state side eid (str "pays " amount " [Credits]")))
      :else
      (complete-with-result state side eid (str "pays 0 [Credits]")))))

(defn pay-clicks
  [state side eid actions costs cost-type amount]
  (let [a (keep :action actions)]
    (when (not (some #{:steal-cost :bioroid-cost} a))
      ;; do not create an undo state if click is being spent due to a steal cost (eg. Ikawah Project)
      (swap! state assoc :click-state (dissoc @state :log)))
    (lose state side :click amount)
    (wait-for (trigger-event-sync state side (make-eid state eid)
                                  (if (= side :corp) :corp-spent-click :runner-spent-click)
                                  a (:click (into {} costs)))
              (swap! state assoc-in [side :register :spent-click] true)
              (complete-with-result state side eid (str "spends " (->> "[Click]" repeat (take amount) (apply str)))))))

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
                               :card #(is-scored? state side %)}
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
               :card select-fn}
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
                                :card select-fn}
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

(defn pay-trash-from-deck
  [state side eid amount]
  (wait-for (mill state side side amount)
            (complete-with-result
              state side eid
              (str "trashes " (quantify amount "card") " from the top of "
                   (if (= :corp side) "R&D" "the stack")))))

(defn pay-trash-from-hand
  "Trash a card from hand as part of a cost"
  [state side eid amount]
  (let [select-fn #(and ((if (= :corp side) corp? runner?) %)
                        (in-hand? %))
        prompt-hand (if (= :corp side) "HQ" "your grip")
        hand (if (= :corp side) "HQ" "their grip")]
    (continue-ability state side
                      {:prompt (str "Choose " (quantify amount "card") " in " prompt-hand " to trash")
                       :choices {:all true
                                 :max amount
                                 :card select-fn}
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
  (wait-for (discard-from-hand state side side amount)
            (complete-with-result
              state side eid
              (str "trashes " (quantify amount "card") " randomly from "
                   (if (= :corp side) "HQ" "the grip")))))

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
                      (str " (" (join ", " (map :title cards)) ")")))))))

(defn pay-trash-type-from-hand
  [state side eid card-type amount]
  (continue-ability
    state side
    {:prompt (str "Choose a " card-type " to trash from your grip")
     :async true
     :choices {:all true
               :max amount
               :card #(and (is-type? % (capitalize card-type))
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
  [state side eid amount]
  (continue-ability state :runner
                    {:prompt (str "Choose " (quantify amount "card") " to shuffle into the stack")
                     :choices {:max amount
                               :all true
                               :card #(and (installed? %)
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
                    nil nil))

(defn pay-move-installed-to-deck
  "Trash a card as part of paying for a card or ability"
  ([state side eid card-type amount select-fn] (pay-move-installed-to-deck state side eid card-type amount select-fn nil))
  ([state side eid card-type amount select-fn args]
   (let [args (merge {:front true} args)
         location (if (:front args) "top" "bottom")
         deck (if (= :corp side) "R&D" "the stack")]
     (continue-ability state side
                       {:prompt (str "Choose " (quantify amount (if card-type card-type "card") ) " to move to the " location " of " deck)
                        :choices {:all true
                                  :max amount
                                  :card select-fn}
                        :async true
                        :effect (req (doseq [c targets]
                                       (move state side target :deck (select-keys args [:front])))
                                     (complete-with-result
                                       state side eid
                                       (str "adds " (quantify amount (or (:plural args) card-type))
                                            " to the " location
                                            " of " deck
                                            " (" (join ", " (map #(card-str state %) targets)) ")")))}
                       nil nil))))

(defn pay-any-agenda-counter
  [state side eid amount]
  (continue-ability
    state side
    {:prompt "Select an agenda with a counter"
     :choices {:card #(and (agenda? %)
                           (is-scored? state side %)
                           (pos? (get-counters % :agenda)))}
     :effect (effect (add-counter target :agenda -1)
                     (trigger-event :agenda-counter-spent (get-card state target))
                     (complete-with-result
                       eid (str "spends an agenda counter from on " (:title target))))}
    nil nil))

(defn pay-any-virus-counter
  "Spending virus counters from any card (Yusuf and Musaazi)"
  [state side eid amount]
  (wait-for (resolve-ability state side (pick-virus-counters-to-spend amount) nil nil)
            (complete-with-result state side eid (str "spends " (:msg async-result)))))

(defn pay-counter
  [state side eid card counter amount]
  (update! state side
           (if (= counter :advancement)
             (update card :advance-counter - amount)
             (update-in card [:counter counter] - amount)))
  (wait-for (trigger-event-sync state side
                                (if (agenda? card)
                                  :agenda-counter-spent
                                  :counter-added)
                                (get-card state card))
            (complete-with-result
              state side eid
              (str "spends "
                   (if (< 1 amount)
                     (quantify amount (str "hosted " (name counter) " counter"))
                     (str "a hosted " (name counter) " counter"))
                   " from on " (:title card)))))

(defn- cost-handler
  "Calls the relevant function for a cost depending on the keyword passed in"
  ([state side card actions costs cost] (cost-handler state side (make-eid state) card actions costs cost))
  ([state side eid card actions costs [cost-type amount]]
   (case cost-type
     ; Symbols
     :credit (pay-credits state side eid card amount)
     :click (pay-clicks state side eid actions costs cost-type amount)
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
     (:net :meat :brain)
     (pay-damage state side eid cost-type amount)

     ;; Discard cards from deck or hand
     :trash-from-deck (pay-trash-from-deck state side eid amount)
     :trash-from-hand (pay-trash-from-hand state side eid amount)
     :randomly-trash-from-hand (pay-randomly-trash-from-hand state side eid amount)
     :trash-entire-hand (pay-trash-entire-hand state side eid)

     ;; Trash cards of specific types from hand
     :trash-hardware-from-hand (pay-trash-type-from-hand state side eid "hardware" amount)
     :trash-program-from-hand (pay-trash-type-from-hand state side eid "program" amount)
     :trash-resource-from-hand (pay-trash-type-from-hand state side eid "resource" amount)

     ;; Shuffle installed runner cards into the stack (eg Degree Mill)
     :shuffle-installed-to-stack (pay-shuffle-installed-to-stack state side eid amount)

     ;; Move installed cards to the deck
     :add-installed-to-bottom-of-deck
     (pay-move-installed-to-deck state side eid nil amount
                                 (every-pred installed?)
                                 {:front false})

     ;; Spend a counter on another card
     :any-agenda-counter (pay-any-agenda-counter state side eid amount)
     :any-virus-counter (pay-any-virus-counter state side eid amount)

     ;; Counter costs
     (:advancement :agenda :power :virus) (pay-counter state side eid card cost-type amount)

     ;; Else
     (do
       (swap! state update-in [:stats side :spent cost-type] (fnil + 0) amount)
       (complete-with-result state side eid (deduct state side [cost-type amount]))))))

(defn pay
  "Deducts each cost from the player.
  args format as follows with each being optional ([:click 1 :credit 0] [:forfeit] {:action :corp-click-credit})
  The map with :action was added for Jeeves so we can log what each click was used on"
  [state side card & args]
  (let [args (flatten args)
        raw-costs (remove map? args)
        actions (filter map? args)
        eid (make-eid state)]
    (when-let [costs (can-pay? state side (:title card) raw-costs)]
        (->> costs
             (map (partial cost-handler state side eid card actions costs))
             (filter some?)
             (interpose " and ")
             (apply str)))))

(defn- pay-sync-next
  [state side eid costs card actions msgs]
  (if (empty? costs)
    (complete-with-result state side eid msgs)
    (wait-for (cost-handler state side (make-eid state eid) card actions costs (first costs))
              (pay-sync-next state side eid (rest costs) card actions (conj msgs async-result)))))

(defn pay-sync
  "Same as pay, but awaitable."
  [state side eid card & args]
  (let [args (flatten args)
        raw-costs (remove map? args)
        actions (filter map? args)]
    (if-let [costs (can-pay? state side eid card (:title card) raw-costs)]
      (wait-for (pay-sync-next state side (make-eid state eid) costs card actions [])
                (complete-with-result state side eid (->> async-result
                                                          (filter some?)
                                                          (join " and "))))
      (complete-with-result state side eid nil))))

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
