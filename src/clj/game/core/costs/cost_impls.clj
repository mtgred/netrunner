(in-ns 'game.core)

(declare forfeit damage mill is-scored?
         discard-from-hand card-str trash trash-cards
         all-installed-runner-type pick-credit-providing-cards all-active
         lose-tags number-of-virus-counters
         pick-virus-counters-to-spend)

(def cost-records {})

(defn register-cost
  [cost-constructor]
  (alter-var-root #'cost-records assoc (cost-name (cost-constructor 1)) cost-constructor))

(extend-type Click
  CostFns
  (cost-name [this] :click)
  (label [this] (->> (repeat "[Click]")
                     (take (:amount this))
                     (apply str)))
  (rank [this] 1)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (get-in @state [side :click]) (:amount this))))
  (handler [this state side eid card actions]
    (let [a (keep :action actions)]
      (when (not (some #{:steal-cost :bioroid-cost} a))
        (swap! state assoc :click-state (dissoc @state :log)))
      (swap! state update-in [:stats side :lose :click] (fnil + 0) (:amount this))
      (deduct state side [:click (:amount this)])
      (wait-for (trigger-event-sync state side (make-eid state eid)
                                    (if (= side :corp) :corp-spent-click :runner-spent-click)
                                    a (:amount this))
                (swap! state assoc-in [side :register :spent-click] true)
                (complete-with-result state side eid (str "spends " (label this)))))))
(register-cost ->Click)

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

(defn total-available-credits
  [state side eid card]
  (+ (get-in @state [side :credit])
     (->> (eligible-pay-credit-cards state side eid card)
          (map #(+ (get-counters % :recurring)
                   (get-counters % :credit)
                   (-> (card-def %) :interactions :pay-credits ((fn [x] (:custom-amount x 0))))))
          (reduce +))))

(extend-type Credit
  CostFns
  (cost-name [this] :credit)
  (label [this] (str (:amount this) " [Credits]"))
  (rank [this] 2)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (or (<= 0 (- (get-in @state [side :credit]) (:amount this)))
        (<= 0 (- (total-available-credits state side eid card) (:amount this)))))
  (handler [this state side eid card actions]
    (let [provider-func #(eligible-pay-credit-cards state side eid card)]
      (cond
        (and (pos? (:amount this))
             (pos? (count (provider-func))))
        (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid (:amount this)) card nil)
                  (swap! state update-in [:stats side :spent :credit] (fnil + 0) (:amount this))
                  (complete-with-result state side eid (str "pays " (:msg async-result))))
        (pos? (:amount this))
        (do (lose state side :credit (:amount this))
            (complete-with-result state side eid (str "pays " (:amount this) " [Credits]")))
        :else
        (complete-with-result state side eid (str "pays 0 [Credits]"))))))
(register-cost ->Credit)

(extend-type Trash
  CostFns
  (cost-name [this] :trash)
  (label [this] "[trash]")
  (rank [this] 3)
  (value [this] 1)
  (payable? [this state side eid card]
    (installed? (get-card state card)))
  (handler [this state side eid card actions]
    (wait-for (trash state side card {:cause :ability-cost
                                      :unpreventable true})
              (complete-with-result state side eid (str "trashes " (:title card))))))
(register-cost ->Trash)

(defn forfeit-multiple
  [state side eid agendas acc]
  (if (empty? agendas)
    (complete-with-result state side eid acc)
    (let [agenda (first agendas)]
      (wait-for (forfeit state side agenda {:msg false})
                (forfeit-multiple state side eid (rest agendas) (conj acc agenda))))))

(extend-type Forfeit
  CostFns
  (cost-name [this] :forfeit)
  (label [this] (str "forfeit " (quantify (:amount this) "Agenda")))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (get-in @state [side :scored])) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "Agenda") " to forfeit")
       :async true
       :choices {:max (:amount this)
                 :all true
                 :card #(is-scored? state side %)}
       :effect (req (wait-for (forfeit-multiple state side targets [])
                              (complete-with-result
                                state side eid
                                (str "forfeits " (quantify (:amount this) "agenda")
                                     " (" (join ", " (map :title async-result)) ")"))))}
      card nil)))
(register-cost ->Forfeit)

(extend-type ForfeitSelf
  CostFns
  (cost-name [this] :forfeit-self)
  (label [this] "forfeit this Agenda")
  (rank [this] 4)
  (value [this] 1)
  (payable? [this state side eid card]
    (is-scored? state side (get-card state card)))
  (handler [this state side eid card actions]
    (wait-for (forfeit state side card {:msg false})
              (complete-with-result
                state side eid
                (str "forfeits " (:title card))))))
(register-cost ->ForfeitSelf)

(extend-type Tag
  CostFns
  (cost-name [this] :tag)
  (label [this] (str "remove " (quantify (:amount this) "tag")))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (get-in @state [:runner :tag :base] 0) (:amount this))))
  (handler [this state side eid card actions]
    (wait-for (lose-tags state side (:amount this))
              (complete-with-result state side eid (str "removes " (quantify (:amount this) "tag"))))))
(register-cost ->Tag)

(extend-type ReturnToHand
  CostFns
  (cost-name [this] :return-to-hand)
  (label [this] "return this card to your hand")
  (rank [this] 4)
  (value [this] 1)
  (payable? [this state side eid card]
    (active? (get-card state card)))
  (handler [this state side eid card actions]
    (move state side card :hand)
    (complete-with-result
      state side eid
      (str "returns " (:title card)
           " to " (if (= :corp side) "HQ" "their grip")))))
(register-cost ->ReturnToHand)

(extend-type RemoveFromGame
  CostFns
  (cost-name [this] :remove-from-game)
  (label [this] "remove this card from the game")
  (rank [this] 3)
  (value [this] 1)
  (payable? [this state side eid card]
    (active? (get-card state card)))
  (handler [this state side eid card actions]
    (move state side card :rfg)
    (complete-with-result
      state side eid
      (str "removes " (:title card) " from the game"))))
(register-cost ->RemoveFromGame)

(extend-type RfgProgram
  CostFns
  (cost-name [this] :rfg-program)
  (label [this] (str "remove " (quantify (:amount this) "installed program")
                     " from the game"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed-runner-type state :program)) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "program")
                    " to remove from the game")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed? program? (complement facedown?))}
       :async true
       :effect (req (doseq [t targets]
                      (move state side (assoc-in t [:persistent :from-cid] (:cid card)) :rfg))
                    (complete-with-result
                      state side eid
                      (str "removes " (quantify (:amount this) "installed program")
                           " from the game"
                           " (" (join ", " (map #(card-str state %) targets)) ")")))}
      card nil)))
(register-cost ->RfgProgram)

(extend-type TrashInstalledRunnerCard
  CostFns
  (cost-name [this] :installed)
  (label [this] (str "trash " (quantify (:amount this) "installed card")))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed state side)) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "installed card") " to trash")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed? runner?)}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (:amount this) "installed card")
                                     " (" (join ", " (map #(card-str state %) targets)) ")"))))}
      card nil)))
(register-cost ->TrashInstalledRunnerCard)

(extend-type TrashInstalledHardware
  CostFns
  (cost-name [this] :hardware)
  (label [this] (str "trash " (quantify (:amount this) "installed piece") " of hardware"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed-runner-type state :hardware)) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "installed piece") " of hardware to trash")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed? hardware? (complement facedown?))}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (:amount this) "installed piece") " of hardware"
                                     " (" (join ", " (map #(card-str state %) targets)) ")"))))}
      card nil)))
(register-cost ->TrashInstalledHardware)

(extend-type TrashInstalledProgram
  CostFns
  (cost-name [this] :program)
  (label [this] (str "trash " (quantify (:amount this) "installed program")))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed-runner-type state :program)) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "installed program") " to trash")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed? program? (complement facedown?))}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (:amount this) "installed program")
                                     " (" (join ", " (map #(card-str state %) targets)) ")"))))}
      card nil)))
(register-cost ->TrashInstalledProgram)

(extend-type TrashInstalledResource
  CostFns
  (cost-name [this] :resource)
  (label [this] (str "trash " (quantify (:amount this) "installed resource")))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed-runner-type state :resource)) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "installed resource") " to trash")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed? resource? (complement facedown?))}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (:amount this) "installed resource")
                                     " (" (join ", " (map #(card-str state %) targets)) ")"))))}
      card nil)))
(register-cost ->TrashInstalledResource)

(extend-type TrashInstalledConnection
  CostFns
  (cost-name [this] :connection)
  (label [this] (str "trash " (str "trash " (quantify (:amount this) "installed connection resource"))))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (filter #(has-subtype? % "Connection") (all-active-installed state :runner))) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "installed connection resource") " to trash")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed?
                                   resource?
                                   #(has-subtype? % "Connection")
                                   (complement facedown?))}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (:amount this) "installed connection resource")
                                     " (" (join ", " (map #(card-str state %) targets)) ")"))))}
      card nil)))
(register-cost ->TrashInstalledConnection)

(extend-type TrashRezzedIce
  CostFns
  (cost-name [this] :ice)
  (label [this] (str "trash " (str "trash " (quantify (:amount this) "installed rezzed ICE" ""))))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (filter (every-pred installed? rezzed? ice?) (all-installed state :corp))) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "installed rezzed ICE" "") " to trash")
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred installed? rezzed? ice?)}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (:amount this) "installed rezzed ICE" "")
                                     " (" (join ", " (map #(card-str state %) targets)) ")"))))}
      card nil)))
(register-cost ->TrashRezzedIce)

(extend-type TrashFromDeck
  CostFns
  (cost-name [this] :trash-from-deck)
  (label [this] (str "trash " (quantify (:amount this) "card") " from the top of your deck"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (get-in @state [side :deck])) (:amount this))))
  (handler [this state side eid card actions]
    (wait-for (mill state side side (:amount this))
              (complete-with-result
                state side eid
                (str "trashes " (quantify (:amount this) "card") " from the top of "
                     (if (= :corp side) "R&D" "the stack"))))))
(register-cost ->TrashFromDeck)

(extend-type TrashFromHand
  CostFns
  (cost-name [this] :trash-from-hand)
  (label [this] (str "trash " (quantify (:amount this) "card") " from your hand"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (get-in @state [side :hand])) (:amount this))))
  (handler [this state side eid card actions]
    (let [select-fn #(and ((if (= :corp side) corp? runner?) %)
                          (in-hand? %))
          prompt-hand (if (= :corp side) "HQ" "your grip")
          hand (if (= :corp side) "HQ" "their grip")]
      (continue-ability
        state side
        {:prompt (str "Choose " (quantify (:amount this) "card") " in " prompt-hand " to trash")
         :choices {:all true
                   :max (:amount this)
                   :card select-fn}
         :async true
         :effect (req (wait-for (trash-cards state side targets {:unpreventable true :seen false})
                                (complete-with-result
                                  state side eid
                                  (str "trashes " (quantify (:amount this) "card")
                                       " (" (join ", " (map #(card-str state %) targets)) ")"
                                       " from " hand))))}
        nil nil))))
(register-cost ->TrashFromHand)

(extend-type RandomlyTrashFromHand
  CostFns
  (cost-name [this] :randomly-trash-from-hand)
  (label [this] (str "trash " (quantify (:amount this) "card") " randomly from your hand"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (get-in @state [side :hand])) (:amount this))))
  (handler [this state side eid card actions]
    (wait-for (discard-from-hand state side side (:amount this))
              (complete-with-result
                state side eid
                (str "trashes " (quantify (:amount this) "card") " randomly from "
                     (if (= :corp side) "HQ" "the grip"))))))
(register-cost ->RandomlyTrashFromHand)

(extend-type TrashEntireHand
  CostFns
  (cost-name [this] :trash-entire-hand)
  (label [this] "trash all cards in your hand")
  (rank [this] 4)
  (value [this] 1)
  (payable? [this state side eid card] true)
  (handler [this state side eid card actions]
    (let [cards (get-in @state [side :hand])]
      (wait-for (trash-cards state side cards {:unpreventable true})
                (complete-with-result
                  state side eid
                  (str "trashes all (" (count async-result) ") cards in "
                       (if (= :runner side) "their grip" "HQ")
                       (when (and (= :runner side)
                                  (pos? (count async-result)))
                         (str " (" (join ", " (map :title async-result)) ")"))))))))
(register-cost ->TrashEntireHand)

(extend-type TrashHardwareFromHand
  CostFns
  (cost-name [this] :trash-hardware-from-hand)
  (label [this] (str "trash " (quantify (:amount this) "piece") " of hardware in your grip"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (filter hardware? (get-in @state [:runner :hand]))) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "piece") " of hardware to trash from your grip")
       :async true
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred hardware? in-hand?)}
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (count async-result) "piece") " of hardware"
                                     " (" (join ", " (map :title targets)) ")"
                                     " from their grip"))))}
      nil nil)))
(register-cost ->TrashHardwareFromHand)

(extend-type TrashProgramFromHand
  CostFns
  (cost-name [this] :trash-program-from-hand)
  (label [this] (str "trash " (quantify (:amount this) "program") " in your grip"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (filter program? (get-in @state [:runner :hand]))) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "program") " to trash from your grip")
       :async true
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred program? in-hand?)}
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (count async-result) "program")
                                     " (" (join ", " (map :title targets)) ")"
                                     " from their grip"))))}
      nil nil)))
(register-cost ->TrashProgramFromHand)

(extend-type TrashResourceFromHand
  CostFns
  (cost-name [this] :trash-resource-from-hand)
  (label [this] (str "trash " (quantify (:amount this) "resource") " in your grip"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (filter resource? (get-in @state [:runner :hand]))) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (:amount this) "resource") " to trash from your grip")
       :async true
       :choices {:all true
                 :max (:amount this)
                 :card (every-pred resource? in-hand?)}
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                              (complete-with-result
                                state side eid
                                (str "trashes " (quantify (count async-result) "resource")
                                     " (" (join ", " (map :title targets)) ")"
                                     " from their grip"))))}
      nil nil)))
(register-cost ->TrashResourceFromHand)

(extend-type NetDamage
  CostFns
  (cost-name [this] :net)
  (label [this] (str "suffer " (:amount this) " net damage"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= (:amount this) (count (get-in @state [:runner :hand]))))
  (handler [this state side eid card actions]
    (wait-for (damage state side :net (:amount this) {:unpreventable true})
              (complete-with-result
                state side eid
                (str "suffers " (:amount this) " net damage")))))
(register-cost ->NetDamage)

(extend-type MeatDamage
  CostFns
  (cost-name [this] :meat)
  (label [this] (str "suffer " (:amount this) " meat damage"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= (:amount this) (count (get-in @state [:runner :hand]))))
  (handler [this state side eid card actions]
    (wait-for (damage state side :meat (:amount this) {:unpreventable true})
              (complete-with-result
                state side eid
                (str "suffers " (:amount this) " meat damage")))))
(register-cost ->MeatDamage)

(extend-type BrainDamage
  CostFns
  (cost-name [this] :brain)
  (label [this] (str "suffer " (:amount this) " brain damage"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= (:amount this) (count (get-in @state [:runner :hand]))))
  (handler [this state side eid card actions]
    (wait-for (damage state side :brain (:amount this) {:unpreventable true})
              (complete-with-result
                state side eid
                (str "suffers " (:amount this) " brain damage")))))
(register-cost ->BrainDamage)

(extend-type ShuffleInstalledToDeck
  CostFns
  (cost-name [this] :shuffle-installed-to-stack)
  (label [this] (str "shuffle " (quantify (:amount this) "installed card") " into your deck"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed state side)) (:amount this))))
  (handler [this state side eid card actions]
    (continue-ability
      state :runner
      {:prompt (str "Choose " (quantify (:amount this) "installed card")
                    " to shuffle into " (if (= :corp side) "R&D" "the stack"))
       :choices {:max (:amount this)
                 :all true
                 :card (every-pred installed? (if (= :corp side) corp? runner?))}
       :async true
       :effect (req (doseq [c targets]
                      (move state side c :deck))
                    (shuffle! state side :deck)
                    (complete-with-result
                      state side eid
                      (str "shuffles " (quantify (:amount this) "card")
                           " (" (join ", " (map :title targets)) ")"
                           " into " (if (= :corp side) "R&D" "the stack"))))}
      nil nil)))
(register-cost ->ShuffleInstalledToDeck)

(extend-type AddInstalledToBottomOfDeck
  CostFns
  (cost-name [this] :add-installed-to-bottom-of-deck)
  (label [this] (str "add " (quantify (:amount this) "installed card") " to the bottom of your deck"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (count (all-installed state side)) (:amount this))))
  (handler [this state side eid card actions]
    (let [deck (if (= :corp side) "R&D" "the stack")]
      (continue-ability
        state side
        {:prompt (str "Choose " (quantify (:amount this) "installed card")
                      " to move to the bottom of " deck)
         :choices {:max (:amount this)
                   :all true
                   :card (every-pred installed? (if (= :corp side) corp? runner?))}
         :async true
         :effect (req (doseq [c targets]
                        (move state side target :deck))
                      (complete-with-result
                        state side eid
                        (str "adds " (quantify (:amount this) "installed card")
                             " to the bottom of " deck
                             " (" (join ", " (map #(card-str state %) targets)) ")")))}
        card nil))))
(register-cost ->AddInstalledToBottomOfDeck)

(extend-type AnyAgendaCounter
  CostFns
  (cost-name [this] :any-agenda-counter)
  (label [this] "any agenda counter")
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (reduce + (map #(get-counters % :agenda) (get-in @state [:corp :scored]))) (:amount this))))
  (handler [this state side eid card actions]
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
      nil nil)))
(register-cost ->AnyAgendaCounter)

(extend-type AnyVirusCounter
  CostFns
  (cost-name [this] :any-virus-counter)
  (label [this] (str "any " (quantify (:amount this) "virus counter")))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (number-of-virus-counters state) (:amount this))))
  (handler [this state side eid card actions]
    (wait-for (resolve-ability state side (pick-virus-counters-to-spend (:amount this)) card nil)
              (complete-with-result state side eid (str "spends " (:msg async-result))))))
(register-cost ->AnyVirusCounter)

(extend-type AdvancementCounter
  CostFns
  (cost-name [this] :advancement)
  (label [this] (if (< 1 (:amount this))
                  (quantify (:amount this) "hosted advancement counter")
                  "hosted advancement counter"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (get-counters card :advancement) (:amount this))))
  (handler [this state side eid card actions]
    (update! state side (update card :advance-counter - (:amount this)))
    (wait-for (trigger-event-sync state side :counter-added (get-card state card))
              (complete-with-result
                state side eid
                (str "spends "
                     (quantify (:amount this) (str "hosted advancement counter"))
                     " from on " (:title card))))))
(register-cost ->AdvancementCounter)

(extend-type AgendaCounter
  CostFns
  (cost-name [this] :agenda)
  (label [this] (if (< 1 (:amount this))
                  (quantify (:amount this) "hosted agenda counter")
                  "hosted agenda counter"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (get-counters card :agenda) (:amount this))))
  (handler [this state side eid card actions]
    (update! state side (update-in card [:counter :agenda] - (:amount this)))
    (wait-for (trigger-event-sync state side :agenda-counter-spent (get-card state card))
              (complete-with-result
                state side eid
                (str "spends "
                     (quantify (:amount this) (str "hosted agenda counter"))
                     " from on " (:title card))))))
(register-cost ->AgendaCounter)

(extend-type PowerCounter
  CostFns
  (cost-name [this] :power)
  (label [this] (if (< 1 (:amount this))
                  (quantify (:amount this) "hosted power counter")
                  "hosted power counter"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (get-counters card :power) (:amount this))))
  (handler [this state side eid card actions]
    (update! state side (update-in card [:counter :power] - (:amount this)))
    (wait-for (trigger-event-sync state side :counter-added (get-card state card))
              (complete-with-result
                state side eid
                (str "spends "
                     (quantify (:amount this) (str "hosted power counter"))
                     " from on " (:title card))))))
(register-cost ->PowerCounter)

(extend-type VirusCounter
  CostFns
  (cost-name [this] :virus)
  (label [this] (if (< 1 (:amount this))
                  (quantify (:amount this) "hosted virus counter")
                  "hosted virus counter"))
  (rank [this] 4)
  (value [this] (:amount this))
  (payable? [this state side eid card]
    (<= 0 (- (+ (get-counters card :virus)
                (->> (all-active-installed state :runner)
                     (filter #(= "Hivemind" (:title %)))
                     (map #(get-counters % :virus))
                     (reduce +)))
             (:amount this))))
  (handler [this state side eid card actions]
    (update! state side (update-in card [:counter :virus] - (:amount this)))
    (wait-for (trigger-event-sync state side :counter-added (get-card state card))
              (complete-with-result
                state side eid
                (str "spends "
                     (quantify (:amount this) (str "hosted virus counter"))
                     " from on " (:title card))))))
(register-cost ->VirusCounter)
