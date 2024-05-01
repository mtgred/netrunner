(ns game.core.costs
  (:require
   [game.core.bad-publicity :refer [gain-bad-publicity]]
   [game.core.board :refer [all-active all-active-installed all-installed all-installed-runner-type]]
   [game.core.card :refer [active? agenda? corp? facedown? get-card get-counters hardware? has-subtype? ice? in-hand? installed? program? resource? rezzed? runner?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.damage :refer [damage]]
   [game.core.eid :refer [complete-with-result make-eid]]
   [game.core.engine :refer [checkpoint resolve-ability trigger-event-sync]]
   [game.core.effects :refer [is-disabled-reg?]]
   [game.core.flags :refer [is-scored?]]
   [game.core.gaining :refer [deduct lose]]
   [game.core.moving :refer [discard-from-hand forfeit mill move trash trash-cards]]
   [game.core.payment :refer [handler label payable? value stealth-value]]
   [game.core.pick-counters :refer [pick-credit-providing-cards pick-virus-counters-to-spend]]
   [game.core.revealing :refer [reveal]]
   [game.core.rezzing :refer [derez]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.tags :refer [lose-tags gain-tags]]
   [game.core.to-string :refer [card-str]]
   [game.core.update :refer [update!]]
   [game.core.virus :refer [number-of-virus-counters]]
   [game.macros :refer [continue-ability req wait-for]]
   [game.utils :refer [enumerate-str quantify same-card?]]))

;; Click
(defmethod value :click [cost] (:cost/amount cost))
(defmethod label :click [cost]
  (->> (repeat "[Click]")
       (take (value cost))
       (apply str)))
(defmethod payable? :click
  [cost state side _ _]
  (<= 0 (- (get-in @state [side :click]) (value cost))))
(defmethod handler :click
  [cost state side eid _card]
  (let [a (:action eid)]
    (swap! state update-in [:stats side :lose :click] (fnil + 0) (value cost))
    (deduct state side [:click (value cost)])
    (wait-for (trigger-event-sync state side (make-eid state eid)
                                  (if (= side :corp) :corp-spent-click :runner-spent-click)
                                  {:action a
                                   :value (value cost)
                                   :ability-idx (:ability-idx (:source-info eid))})
              ;; sending the idx is mandatory to make wage workers functional
              ;; and so we can look through the events and figure out WHICH abilities were used
              ;; I don't think it will break anything
              (swap! state assoc-in [side :register :spent-click] true)
              (complete-with-result state side eid {:paid/msg (str "spends " (label cost))
                                                    :paid/type :click
                                                    :paid/value (value cost)}))))

;; Lose Click
(defn lose-click-label
  [cost]
  (->> (repeat "[Click]")
       (take (value cost))
       (apply str)))
(defmethod value :lose-click [cost] (:cost/amount cost))
(defmethod label :lose-click [cost]
  (str "Lose " (lose-click-label cost)))
(defmethod payable? :lose-click
  [cost state side _ _]
  (<= 0 (- (get-in @state [side :click]) (value cost))))
(defmethod handler :lose-click
  [cost state side eid _card]
  (swap! state update-in [:stats side :lose :click] (fnil + 0) (value cost))
  (deduct state side [:click (value cost)])
  (wait-for (trigger-event-sync state side (make-eid state eid)
                                (if (= side :corp) :corp-spent-click :runner-spent-click)
                                {:value (value cost)})
            (swap! state assoc-in [side :register :spent-click] true)
            (complete-with-result state side eid {:paid/msg (str "loses " (lose-click-label cost))
                                                  :paid/type :lose-click
                                                  :paid/value (value cost)})))


(defn- all-active-pay-credit-cards
  [state side eid card]
  (filter #(when-let [pc (-> % card-def :interactions :pay-credits)]
             ;; All payment should be "inbetween" checkpoints
             ;; If there's ever some obscure issue with timing for this,
             ;; then we can always manually update the reg here
             ;; --nk, Apr 2024
             (when-not (is-disabled-reg? state %)
                 (if (:req pc)
                   ((:req pc) state side eid % [card])
                   true)))
          (all-active state side)))

(defn- eligible-pay-credit-cards
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

(defn- eligible-pay-stealth-credit-cards
  [state side eid card]
  (filter #(has-subtype? % "Stealth") (eligible-pay-credit-cards state side eid card)))

(defn- total-available-stealth-credits
  [state side eid card]
  (->> (eligible-pay-stealth-credit-cards state side eid card)
       (map #(+ (get-counters % :recurring)
                (get-counters % :credit)
                (-> (card-def %) :interactions :pay-credits ((fn [x] (:custom-amount x 0))))))
       (reduce +)))

;; Credit
(defmethod value :credit [cost] (:cost/amount cost))
(defmethod stealth-value :credit [cost] (let [v (:cost/stealth cost)]
                                          (cond
                                            (= :all-stealth v) (value cost)
                                            v v
                                            :else 0)))
(defmethod label :credit [cost] (str (value cost) " [Credits]"))
(defmethod payable? :credit
  [cost state side eid card]
  (and (<= 0 (- (total-available-stealth-credits state side eid card) (stealth-value cost)))
       (<= 0 (- (value cost) (stealth-value cost)))
       (or (<= 0 (- (get-in @state [side :credit]) (value cost)))
           (<= 0 (- (total-available-credits state side eid card) (value cost))))))
(defmethod handler :credit
  [cost state side eid card]
  (let [provider-func #(eligible-pay-credit-cards state side eid card)]
    (cond
      (and (pos? (value cost))
           (pos? (count (provider-func))))
      (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid (value cost) (stealth-value cost)) card nil)
                (let [pay-async-result async-result]
                  (wait-for (trigger-event-sync
                              state side (make-eid state eid)
                              (if (= side :corp) :corp-spent-credits :runner-spent-credits)
                              (value cost))
                            (swap! state update-in [:stats side :spent :credit] (fnil + 0) (value cost))
                            (complete-with-result state side eid
                                                  {:paid/msg (str "pays " (:msg pay-async-result))
                                                   :paid/type :credit
                                                   :paid/value (:number pay-async-result)
                                                   :paid/targets (:targets pay-async-result)}))))
      (pos? (value cost))
      (do (lose state side :credit (value cost))
          (wait-for (trigger-event-sync
                      state side (make-eid state eid)
                      (if (= side :corp) :corp-spent-credits :runner-spent-credits)
                      (value cost))
                    (swap! state update-in [:stats side :spent :credit] (fnil + 0) (value cost))
                    (complete-with-result state side eid {:paid/msg (str "pays " (value cost) " [Credits]")
                                                          :paid/type :credit
                                                          :paid/value (value cost)})))
      :else
      (complete-with-result state side eid {:paid/msg "pays 0 [Credits]"
                                            :paid/type :credit
                                            :paid/value 0}))))

;; X Credits
(defmethod value :x-credits [_] 0)
;We put stealth credits in the third slot rather than the empty second slot for consistency with credits
(defmethod stealth-value :x-credits [cost] (or (:cost/stealth cost) 0))
(defmethod label :x-credits [_] (str "X [Credits]"))
(defmethod payable? :x-credits
  [cost state side eid card]
  (and (pos? (total-available-credits state side eid card))
       (<= (stealth-value cost) (total-available-stealth-credits state side eid card))))
(defmethod handler :x-credits
  [cost state side eid card]
  (continue-ability
    state side
    {:async true
     :prompt "How many credits do you want to spend?"
     :choices {:number (req (total-available-credits state side eid card))}
     :effect
     (req
       (let [stealth-value (if (= -1 (stealth-value cost)) cost (stealth-value cost))
             cost target
             provider-func #(eligible-pay-credit-cards state side eid card)]
         (cond
           (and (pos? cost)
                (pos? (count (provider-func))))
           (wait-for (resolve-ability state side (pick-credit-providing-cards provider-func eid cost stealth-value) card nil)
                     (swap! state update-in [:stats side :spent :credit] (fnil + 0) cost)
                     (complete-with-result state side eid {:paid/msg (str "pays " (:msg async-result))
                                                           :paid/type :x-credits
                                                           :paid/value (:number async-result)
                                                           :paid/targets (:targets async-result)}))
           (pos? cost)
           (do (lose state side :credit cost)
               (wait-for (trigger-event-sync
                           state side (make-eid state eid)
                           (if (= side :corp) :corp-spent-credits :runner-spent-credits)
                           cost)
                         (swap! state update-in [:stats side :spent :credit] (fnil + 0) cost)
                         (complete-with-result state side eid {:paid/msg (str "pays " cost " [Credits]")
                                                               :paid/type :x-credits
                                                               :paid/value cost})))
           :else
           (complete-with-result state side eid {:paid/msg (str "pays 0 [Credits]")
                                                 :paid/type :x-credits
                                                 :paid/value 0}))))}
    card nil))

;; Expend Helper - this is a dummy cost just for cost strings
(defmethod value :expend [cost] 1)
(defmethod label :expend [cost] "reveal from HQ and trash itself")
(defmethod payable? :expend
  [cost state side eid card]
  (in-hand? (get-card state card)))
(defmethod handler :expend
  [cost state side eid card]
  (wait-for (reveal state :corp (make-eid state eid) [card])
            (wait-for (trash state :corp (make-eid state eid)
                             (assoc (get-card state card) :seen true))
                      (complete-with-result state side eid
                                            {:paid/msg (str "trashes " (:title card) " from HQ")
                                             :paid/type :expend
                                             :paid/value 1
                                             :paid/targets [card]}))))

;; Trash
(defmethod value :trash-can [cost] 1)
(defmethod label :trash-can [cost] "[trash]")
(defmethod payable? :trash-can
  [cost state side eid card]
  (installed? (get-card state card)))
(defmethod handler :trash-can
  [cost state side eid card]
  (wait-for (trash state side card {:cause :ability-cost
                                    :unpreventable true})
            (complete-with-result state side eid {:paid/msg (str "trashes " (:title card))
                                                  :paid/type :trash-can
                                                  :paid/value 1
                                                  :paid/targets [card]})))

;; Forfeit
(defmethod value :forfeit [cost] (:cost/amount cost))
(defmethod label :forfeit [cost] (str "forfeit " (quantify (value cost) "Agenda")))
(defmethod payable? :forfeit
  [cost state side _eid _card]
  (<= 0 (- (count (get-in @state [side :scored])) (value cost))))
(defmethod handler :forfeit
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "Agenda") " to forfeit")
     :async true
     :choices {:max (value cost)
               :all true
               :card #(is-scored? state side %)}
     :effect (req (doseq [agenda targets]
                    ;; We don't have to await this because we're suppressing the
                    ;; checkpoint and forfeit makes all of the trashing unpreventable,
                    ;; meaning that there will be no potential for input. Once
                    ;; everything is queued, then we perform the actual checkpoint.
                    (forfeit state side (make-eid state eid) agenda {:msg false
                                                                     :suppress-checkpoint true}))
                  (wait-for (checkpoint state nil (make-eid state eid) {:durations [:game-trash]})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "forfeits " (quantify (value cost) "agenda")
                                             " (" (enumerate-str (map :title targets)) ")")
                               :paid/type :forfeit
                               :paid/value (value cost)
                               :paid/targets targets})))}
    card nil))

;; ForfeitSelf
(defmethod value :forfeit-self [_cost] 1)
(defmethod label :forfeit-self [_cost] "forfeit this Agenda")
(defmethod payable? :forfeit-self
  [_cost state side _eid card]
  (is-scored? state side (get-card state card)))
(defmethod handler :forfeit-self
  [_cost state side eid card]
  (wait-for (forfeit state side (make-eid state eid) card {:msg false})
            (complete-with-result
              state side eid
              {:paid/msg (str "forfeits " (:title card))
               :paid/type :forfeit-self
               :paid/value 1
               :paid/targets [card]})))


;; Gain tag
(defmethod value :gain-tag [cost] (:cost/amount cost))
(defmethod label :gain-tag [cost] (str "take " (quantify (value cost) "tag")))
(defmethod payable? :gain-tag
  ;; TODO - shouldn't actually be true if we're forced to avoid tags
  ;;  QuianjuPT, Jesminder, dorm-computer can do this -nbkelly, Jan '24
  [_ _ _ _ _]
  true)
(defmethod handler :gain-tag
  [cost state side eid card]
  (wait-for (gain-tags state side (value cost))
            (complete-with-result state side eid {:paid/msg (str "takes " (quantify (value cost) "tag"))
                                                  :paid/type :gain-tag
                                                  :paid/value (value cost)})))

;; Tag
(defmethod value :tag [cost] (:cost/amount cost))
(defmethod label :tag [cost] (str "remove " (quantify (value cost) "tag")))
(defmethod payable? :tag
  [cost state side eid card]
  (<= 0 (- (get-in @state [:runner :tag :base] 0) (value cost))))
(defmethod handler :tag
  [cost state side eid card]
  (wait-for (lose-tags state side (value cost))
            (complete-with-result state side eid {:paid/msg (str "removes " (quantify (value cost) "tag"))
                                                  :paid/type :tag
                                                  :paid/value (value cost)})))

;; Tag-or-bad-pub
(defmethod value :tag-or-bad-pub [cost] (:cost/amount cost))
(defmethod label :tag-or-bad-pub [cost] (str "remove " (quantify (value cost) "tag") " or take " (value cost) " bad publicity"))
(defmethod payable? :tag-or-bad-pub
  [cost state side eid card]
  true)
(defmethod handler :tag-or-bad-pub
  [cost state side eid card]
  (if-not (<= 0 (- (get-in @state [:runner :tag :base] 0) (value cost)))
    (wait-for (gain-bad-publicity state side (make-eid state eid) (value cost) nil)
              (complete-with-result state side eid {:paid/msg (str "gains " (value cost) " bad publicity")
                                                    :paid/type :tag-or-bad-pub
                                                    :paid/value (value cost)}))
    (continue-ability
      state side
      {:prompt "Choose one"
       :choices [(str "Remove " (quantify (value cost) "tag"))
                 (str "Gain " (value cost) " bad publicity")]
       :async true
       :effect (req (if (= target (str "Gain " (value cost) " bad publicity"))
                      (wait-for (gain-bad-publicity state side (make-eid state eid) (value cost) nil)
                                (complete-with-result state side eid {:paid/msg (str "gains " (value cost) " bad publicity")
                                                                      :paid/type :tag-or-bad-pub
                                                                      :paid/value (value cost)}))
                      (wait-for (lose-tags state side (value cost))
                                (complete-with-result state side eid {:paid/msg (str "removes " (quantify (value cost) "tag"))
                                                                      :paid/type :tag-or-bad-pub
                                                                      :paid/value (value cost)}))))}
      card nil)))

;; ReturnToHand
(defmethod value :return-to-hand [cost] 1)
(defmethod label :return-to-hand [cost] "return this card to your hand")
(defmethod payable? :return-to-hand
  [cost state side eid card]
  (active? (get-card state card)))
(defmethod handler :return-to-hand
  [cost state side eid card]
  (move state side card :hand)
  (complete-with-result
    state side eid
    {:paid/msg (str "returns " (:title card)
                   " to " (if (= :corp side) "HQ" "their grip"))
     :paid/type :return-to-hand
     :paid/value 1
     :paid/targets [card]}))

;; RemoveFromGame
(defmethod value :remove-from-game [cost] 1)
(defmethod label :remove-from-game [cost] "remove this card from the game")
(defmethod payable? :remove-from-game
  [cost state side eid card]
  (active? (get-card state card)))
(defmethod handler :remove-from-game
  [cost state side eid card]
  (move state side card :rfg)
  (complete-with-result
    state side eid
    {:paid/msg (str "removes " (:title card) " from the game")
     :paid/type :remove-from-game
     :paid/value 1
     :paid/targets [card]}))

;; RfgProgram
(defmethod value :rfg-program [cost] (:cost/amount cost))
(defmethod label :rfg-program [cost]
  (str "remove " (quantify (value cost) "installed program")
       " from the game"))
(defmethod payable? :rfg-program
  [cost state side eid card]
  (<= 0 (- (count (all-installed-runner-type state :program)) (value cost))))
(defmethod handler :rfg-program
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "program")
                  " to remove from the game")
     :choices {:all true
               :max (value cost)
               :card (every-pred installed? program? (complement facedown?))}
     :async true
     :effect (req (doseq [t targets]
                    (move state side (assoc-in t [:persistent :from-cid] (:cid card)) :rfg))
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "removes " (quantify (value cost) "installed program")
                                   " from the game"
                                   " (" (enumerate-str (map #(card-str state %) targets)) ")")
                     :paid/type :rfg-program
                     :paid/value (value cost)
                     :paid/targets targets}))}
    card nil))

;; TrashOtherInstalledCard - this may NOT target the source card (itself), use :trash-installed instead
(defmethod value :trash-other-installed [cost] (:cost/amount cost))
(defmethod label :trash-other-installed [cost]
  (str "trash " (quantify (value cost) "installed card")))
(defmethod payable? :trash-other-installed
  [cost state side eid card]
  (<= 0 (- (count (filter #(not (same-card? card %)) (all-installed state side))) (value cost))))
(defmethod handler :trash-other-installed
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed card") " to trash")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (not (same-card? % card))
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed card")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :trash-other-installed
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashInstalledCard - this may target the source card (itself)
(defmethod value :trash-installed [cost] (:cost/amount cost))
(defmethod label :trash-installed [cost]
  (str "trash " (quantify (value cost) "installed card")))
(defmethod payable? :trash-installed
  [cost state side eid card]
  (<= 0 (- (count (all-installed state side)) (value cost))))
(defmethod handler :trash-installed
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed card") " to trash")
     :choices {:all true
               :max (value cost)
               :card #(and (installed? %)
                           (if (= side :runner)
                             (runner? %)
                             (corp? %)))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed card")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :trash-installed
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashInstalledHardware
(defmethod value :hardware [cost] (:cost/amount cost))
(defmethod label :hardware [cost]
  (str "trash " (quantify (value cost) "installed piece") " of hardware"))
(defmethod payable? :hardware
  [cost state side eid card]
  (<= 0 (- (count (all-installed-runner-type state :hardware)) (value cost))))
(defmethod handler :hardware
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed piece") " of hardware to trash")
     :choices {:all true
               :max (value cost)
               :card (every-pred installed? hardware? (complement facedown?))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed piece")
                                             " of hardware"
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :hardware
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; DerezOtherHarmonic - this may NOT target the source card (itself)
(defmethod value :derez-other-harmonic [cost] (:cost/amount cost))
(defmethod label :derez-other-harmonic [cost]
  (str "derez " (value cost) " Harmonic ice"))
(defmethod payable? :derez-other-harmonic
  [cost state side eid card]
  (<= 0 (- (count (filter #(and (rezzed? %)
                                (has-subtype? % "Harmonic")
                                (not (same-card? card %)))
                          (all-active-installed state :corp))) (value cost))))
(defmethod handler :derez-other-harmonic
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (value cost) " Harmonic ice to derez")
     :choices {:all true
               :max (value cost)
               :card #(and (rezzed? %)
                           (not (same-card? % card))
                           (has-subtype? % "Harmonic"))}
     :async true
     :effect (req (doseq [harmonic targets]
                    (derez state side harmonic))
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "derezzes " (count targets)
                                   " Harmonic ice (" (enumerate-str (map #(card-str state %) targets)) ")")
                     :paid/type :derez
                     :paid/value (count targets)
                     :paid/targets targets}))}
    card nil))

;; TrashInstalledProgram
(defmethod value :program [cost] (:cost/amount cost))
(defmethod label :program [cost]
  (str "trash " (quantify (value cost) "installed program")))
(defmethod payable? :program
  [cost state side eid card]
  (<= 0 (- (count (all-installed-runner-type state :program)) (value cost))))
(defmethod handler :program
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed program") " to trash")
     :choices {:all true
               :max (value cost)
               :card (every-pred installed? program? (complement facedown?))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed program")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :program
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashInstalledResource
(defmethod value :resource [cost] (:cost/amount cost))
(defmethod label :resource [cost]
  (str "trash " (quantify (value cost) "installed resource")))
(defmethod payable? :resource
  [cost state side eid card]
  (<= 0 (- (count (all-installed-runner-type state :resource)) (value cost))))
(defmethod handler :resource
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed resource") " to trash")
     :choices {:all true
               :max (value cost)
               :card (every-pred installed? resource? (complement facedown?))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed resource")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :resource
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashInstalledConnection
(defmethod value :connection [cost] (:cost/amount cost))
(defmethod label :connection [cost]
  (str "trash " (str "trash " (quantify (value cost) "installed connection resource"))))
(defmethod payable? :connection
  [cost state side eid card]
  (<= 0 (- (count (filter #(has-subtype? % "Connection") (all-active-installed state :runner))) (value cost))))
(defmethod handler :connection
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed connection resource") " to trash")
     :choices {:all true
               :max (value cost)
               :card (every-pred installed?
                                 resource?
                                 #(has-subtype? % "Connection")
                                 (complement facedown?))}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed connection resource")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :connection
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashRezzedIce
(defmethod value :ice [cost] (:cost/amount cost))
(defmethod label :ice [cost]
  (str "trash " (str "trash " (quantify (value cost) "installed rezzed ice" ""))))
(defmethod payable? :ice
  [cost state side eid card]
  (<= 0 (- (count (filter (every-pred installed? rezzed? ice?) (all-installed state :corp))) (value cost))))
(defmethod handler :ice
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "installed rezzed ice" "") " to trash")
     :choices {:all true
               :max (value cost)
               :card (every-pred installed? rezzed? ice?)}
     :async true
     :effect (req (wait-for (trash-cards state side targets {:cause :ability-cost
                                                             :unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "installed rezzed ice" "")
                                             " (" (enumerate-str (map #(card-str state %) targets)) ")")
                               :paid/type :ice
                               :paid/value (count async-result)
                               :paid/targets targets})))}
    card nil))

;; TrashFromDeck
(defmethod value :trash-from-deck [cost] (:cost/amount cost))
(defmethod label :trash-from-deck [cost]
  (str "trash " (quantify (value cost) "card") " from the top of your deck"))
(defmethod payable? :trash-from-deck
  [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :deck])) (value cost))))
(defmethod handler :trash-from-deck
  [cost state side eid card]
  (wait-for (mill state side side (value cost))
            (complete-with-result
              state side eid
              {:paid/msg (str "trashes " (quantify (count async-result) "card")
                             " from the top of "
                             (if (= :corp side) "R&D" "the stack"))
               :paid/type :trash-from-deck
               :paid/value (count async-result)
               :paid/targets async-result})))

;; TrashFromHand
(defmethod value :trash-from-hand [cost] (:cost/amount cost))
(defmethod label :trash-from-hand [cost]
  (str "trash " (quantify (value cost) "card") " from your hand"))
(defmethod payable? :trash-from-hand
  [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :hand])) (value cost))))
(defmethod handler :trash-from-hand
  [cost state side eid card]
  (let [select-fn #(and ((if (= :corp side) corp? runner?) %)
                        (in-hand? %))
        hand (if (= :corp side) "HQ" "the grip")]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (value cost) "card") " to trash")
       :choices {:all true
                 :max (value cost)
                 :card select-fn}
       :async true
       :effect (req (wait-for (trash-cards state side targets {:unpreventable true :seen false})
                              (complete-with-result
                                state side eid
                                {:paid/msg (str "trashes " (quantify (count async-result) "card")
                                               (when (and (= :runner side)
                                                          (pos? (count async-result)))
                                                 (str " (" (enumerate-str (map #(card-str state %) targets)) ")"))
                                               " from " hand)
                                 :paid/type :trash-from-hand
                                 :paid/value (count async-result)
                                 :paid/targets async-result})))}
      nil nil)))

;; RandomlyTrashFromHand
(defmethod value :randomly-trash-from-hand [cost] (:cost/amount cost))
(defmethod label :randomly-trash-from-hand [cost]
  (str "trash " (quantify (value cost) "card") " randomly from your hand"))
(defmethod payable? :randomly-trash-from-hand
  [cost state side eid card]
  (<= 0 (- (count (get-in @state [side :hand])) (value cost))))
(defmethod handler :randomly-trash-from-hand
  [cost state side eid card]
  (wait-for (discard-from-hand state side side (value cost))
            (complete-with-result
              state side eid
              {:paid/msg (str "trashes " (quantify (count async-result) "card")
                             " randomly from "
                             (if (= :corp side) "HQ" "the grip"))
               :paid/type :randomly-trash-from-hand
               :paid/value (count async-result)
               :paid/targets async-result})))

;; TrashEntireHand
(defmethod value :trash-entire-hand [cost] 1)
(defmethod label :trash-entire-hand [cost] "trash all cards in your hand")
(defmethod payable? :trash-entire-hand
  [cost state side eid card] true)
(defmethod handler :trash-entire-hand
  [cost state side eid card]
  (let [cards (get-in @state [side :hand])]
    (wait-for (trash-cards state side cards {:unpreventable true})
              (complete-with-result
                state side eid
                {:paid/msg (str "trashes all (" (count async-result) ") cards in "
                               (if (= :runner side) "their grip" "HQ")
                               (when (and (= :runner side)
                                          (pos? (count async-result)))
                                 (str " (" (enumerate-str (map :title async-result)) ")")))
                 :paid/type :trash-entire-hand
                 :paid/value (count async-result)
                 :paid/targets async-result}))))

;; TrashHardwareFromHand
(defmethod value :trash-hardware-from-hand [cost] (:cost/amount cost))
(defmethod label :trash-hardware-from-hand [cost]
  (str "trash " (quantify (value cost) "piece") " of hardware in the grip"))
(defmethod payable? :trash-hardware-from-hand
  [cost state side eid card]
  (<= 0 (- (count (filter hardware? (get-in @state [:runner :hand]))) (value cost))))
(defmethod handler :trash-hardware-from-hand
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "piece") " of hardware to trash")
     :async true
     :choices {:all true
               :max (value cost)
               :card (every-pred hardware? in-hand?)}
     :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "piece")
                                             " of hardware"
                                             " (" (enumerate-str (map :title targets)) ")"
                                             " from their grip")
                               :paid/type :trash-hardware-from-hand
                               :paid/value (count async-result)
                               :paid/targets async-result})))}
    nil nil))

;; TrashProgramFromHand
(defmethod value :trash-program-from-hand [cost] (:cost/amount cost))
(defmethod label :trash-program-from-hand [cost]
  (str "trash " (quantify (value cost) "program") " in the grip"))
(defmethod payable? :trash-program-from-hand
  [cost state side eid card]
  (<= 0 (- (count (filter program? (get-in @state [:runner :hand]))) (value cost))))
(defmethod handler :trash-program-from-hand
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "program") " to trash")
     :async true
     :choices {:all true
               :max (value cost)
               :card (every-pred program? in-hand?)}
     :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "program")
                                             " (" (enumerate-str (map :title targets)) ")"
                                             " from the grip")
                               :paid/type :trash-program-from-hand
                               :paid/value (count async-result)
                               :paid/targets async-result})))}
    nil nil))

;; TrashResourceFromHand
(defmethod value :trash-resource-from-hand [cost] (:cost/amount cost))
(defmethod label :trash-resource-from-hand [cost]
  (str "trash " (quantify (value cost) "resource") " in the grip"))
(defmethod payable? :trash-resource-from-hand
  [cost state side eid card]
  (<= 0 (- (count (filter resource? (get-in @state [:runner :hand]))) (value cost))))
(defmethod handler :trash-resource-from-hand
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt (str "Choose " (quantify (value cost) "resource") " to trash")
     :async true
     :choices {:all true
               :max (value cost)
               :card (every-pred resource? in-hand?)}
     :effect (req (wait-for (trash-cards state side targets {:unpreventable true})
                            (complete-with-result
                              state side eid
                              {:paid/msg (str "trashes " (quantify (count async-result) "resource")
                                             " (" (enumerate-str (map :title targets)) ")"
                                             " from the grip")
                               :paid/type :trash-resource-from-hand
                               :paid/value (count async-result)
                               :paid/targets async-result})))}
    nil nil))

;; NetDamage
(defmethod value :net [cost] (:cost/amount cost))
(defmethod label :net [cost] (str "suffer " (value cost) " net damage"))
(defmethod payable? :net
  [cost state side eid card]
  (<= (value cost) (count (get-in @state [:runner :hand]))))
(defmethod handler :net
  [cost state side eid card]
  (wait-for (damage state side :net (value cost) {:unpreventable true :card card})
            (complete-with-result
              state side eid
              {:paid/msg (str "suffers " (count async-result) " net damage")
               :paid/type :net
               :paid/value (count async-result)
               :paid/targets async-result})))

;; MeatDamage
(defmethod value :meat [cost] (:cost/amount cost))
(defmethod label :meat [cost] (str "suffer " (value cost) " meat damage"))
(defmethod payable? :meat
  [cost state side eid card]
  (<= (value cost) (count (get-in @state [:runner :hand]))))
(defmethod handler :meat
  [cost state side eid card]
  (wait-for (damage state side :meat (value cost) {:unpreventable true :card card})
            (complete-with-result
              state side eid
              {:paid/msg (str "suffers " (count async-result) " meat damage")
               :paid/type :meat
               :paid/value (count async-result)
               :paid/targets async-result})))

;; BrainDamage
(defmethod value :brain [cost] (:cost/amount cost))
(defmethod label :brain [cost] (str "suffer " (value cost) " core damage"))
(defmethod payable? :brain
  [cost state side eid card]
  (<= (value cost) (count (get-in @state [:runner :hand]))))
(defmethod handler :brain
  [cost state side eid card]
  (wait-for (damage state side :brain (value cost) {:unpreventable true :card card})
            (complete-with-result
              state side eid
              {:paid/msg (str "suffers " (count async-result) " core damage")
               :paid/type :brain
               :paid/value (count async-result)
               :paid/targets async-result})))

;; ShuffleInstalledToDeck
(defmethod value :shuffle-installed-to-stack [cost] (:cost/amount cost))
(defmethod label :shuffle-installed-to-stack [cost]
  (str "shuffle " (quantify (value cost) "installed card") " into your deck"))
(defmethod payable? :shuffle-installed-to-stack
  [cost state side eid card]
  (<= 0 (- (count (all-installed state side)) (value cost))))
(defmethod handler :shuffle-installed-to-stack
  [cost state side eid card]
  (continue-ability
    state :runner
    {:prompt (str "Choose " (quantify (value cost) "installed card")
                  " to shuffle into " (if (= :corp side) "R&D" "the stack"))
     :choices {:max (value cost)
               :all true
               :card (every-pred installed? (if (= :corp side) corp? runner?))}
     :async true
     :effect (req (let [cards (keep #(move state side % :deck) targets)]
                    (shuffle! state side :deck)
                    (complete-with-result
                      state side eid
                      {:paid/msg (str "shuffles " (quantify (count cards) "card")
                                     " (" (enumerate-str (map :title cards)) ")"
                                     " into " (if (= :corp side) "R&D" "the stack"))
                       :paid/type :shuffle-installed-to-stack
                       :paid/value (count cards)
                       :paid/targets cards})))}
    nil nil))

;; AddInstalledToBottomOfDeck
(defmethod value :add-installed-to-bottom-of-deck [cost] (:cost/amount cost))
(defmethod label :add-installed-to-bottom-of-deck [cost]
  (str "add " (quantify (value cost) "installed card") " to the bottom of your deck"))
(defmethod payable? :add-installed-to-bottom-of-deck
  [cost state side eid card]
  (<= 0 (- (count (all-installed state side)) (value cost))))
(defmethod handler :add-installed-to-bottom-of-deck
  [cost state side eid card]
  (let [deck (if (= :corp side) "R&D" "the stack")]
    (continue-ability
      state side
      {:prompt (str "Choose " (quantify (value cost) "installed card")
                    " to move to the bottom of " deck)
       :choices {:max (value cost)
                 :all true
                 :card (every-pred installed? (if (= :corp side) corp? runner?))}
       :async true
       :effect (req (let [cards (keep #(move state side % :deck) targets)]
                      (complete-with-result
                        state side eid
                        {:paid/msg (str "adds " (quantify (count cards) "installed card")
                                       " to the bottom of " deck
                                       " (" (enumerate-str (map #(card-str state %) targets)) ")")
                         :paid/type :add-installed-to-bottom-of-deck
                         :paid/value (count cards)
                         :paid/targets cards})))}
      nil nil)))

;; AddRandomToBottom
(defmethod value :add-random-from-hand-to-bottom-of-deck [cost] (:cost/amount cost))
(defmethod label :add-random-from-hand-to-bottom-of-deck [cost]
  (str "add " (quantify (value cost) "random card") " to the bottom of your deck"))
(defmethod payable? :add-random-from-hand-to-bottom-of-deck
  [cost state side eid card]
  (<= (value cost) (count (get-in @state [side :hand]))))
(defmethod handler :add-random-from-hand-to-bottom-of-deck
  [cost state side eid card]
  (let [deck (if (= :corp side) "R&D" "the stack")
        hand (get-in @state [side :hand])
        chosen (take (value cost) (shuffle hand))]
    (doseq [c chosen]
      (move state side c :deck))
    (complete-with-result
      state side eid
      {:paid/msg (str "adds " (quantify (value cost) "random card")
                     " to the bottom of " deck)
       :paid/type :add-random-from-hand-to-bottom-of-deck
       :paid/value (value cost)
       :paid/targets chosen})))

;; AnyAgendaCounter
(defmethod value :any-agenda-counter [cost] (:cost/amount cost))
(defmethod label :any-agenda-counter [cost] "any agenda counter")
(defmethod payable? :any-agenda-counter
  [cost state side eid card]
  (<= 0 (- (reduce + (map #(get-counters % :agenda) (get-in @state [:corp :scored]))) (value cost))))
(defmethod handler :any-agenda-counter
  [cost state side eid card]
  (continue-ability
    state side
    {:prompt "Choose an agenda with a counter"
     :choices {:card #(and (agenda? %)
                           (is-scored? state side %)
                           (pos? (get-counters % :agenda)))}
     :effect (req (let [title (:title target)
                        target (update! state side (update-in target [:counter :agenda] - (value cost)))]
                    (wait-for (trigger-event-sync state side :agenda-counter-spent target)
                              (complete-with-result
                                state side eid
                                {:paid/msg (str "spends "
                                           (quantify (value cost) (str "hosted agenda counter"))
                                           " from on " title)
                                 :paid/type :any-agenda-counter
                                 :paid/value (value cost)
                                 :paid/targets [target]}))))}
    nil nil))

;; AnyVirusCounter
(defmethod value :any-virus-counter [cost] (:cost/amount cost))
(defmethod label :any-virus-counter [cost]
  (str "any " (quantify (value cost) "virus counter")))
(defmethod payable? :any-virus-counter
  [cost state side eid card]
  (<= 0 (- (number-of-virus-counters state) (value cost))))
(defmethod handler :any-virus-counter
  [cost state side eid card]
  (wait-for (resolve-ability state side (pick-virus-counters-to-spend (value cost)) card nil)
            (complete-with-result
              state side eid
              {:paid/msg (str "spends " (:msg async-result))
               :paid/type :any-virus-counter
               :paid/value (:number async-result)
               :paid/targets (:targets async-result)})))

;; AdvancementCounter
(defmethod value :advancement [cost] (:cost/amount cost))
(defmethod label :advancement [cost]
  (if (< 1 (value cost))
    (quantify (value cost) "hosted advancement counter")
    "hosted advancement counter"))
(defmethod payable? :advancement
  [cost state side eid card]
  (<= 0 (- (get-counters card :advancement) (value cost))))
(defmethod handler :advancement
  [cost state side eid card]
  (let [title (:title card)
        card (update! state side (update card :advance-counter - (value cost)))]
    (wait-for (trigger-event-sync state side :counter-added card)
              (complete-with-result
                state side eid
                {:paid/msg (str "spends "
                               (quantify (value cost) (str "hosted advancement counter"))
                               " from on " title)
                 :paid/type :advancement
                 :paid/value (value cost)}))))

;; AgendaCounter
(defmethod value :agenda [cost] (:cost/amount cost))
(defmethod label :agenda [cost]
  (if (< 1 (value cost))
    (quantify (value cost) "hosted agenda counter")
    "hosted agenda counter"))
(defmethod payable? :agenda
  [cost state side eid card]
  (<= 0 (- (get-counters card :agenda) (value cost))))
(defmethod handler :agenda
  [cost state side eid card]
  (let [title (:title card)
        card (update! state side (update-in card [:counter :agenda] - (value cost)))]
    (wait-for (trigger-event-sync state side :agenda-counter-spent card)
              (complete-with-result
                state side eid
                {:paid/msg (str "spends "
                               (quantify (value cost) "hosted agenda counter")
                               " from on " title)
                 :paid/type :agenda
                 :paid/value (value cost)}))))

;; PowerCounter
(defmethod value :power [cost] (:cost/amount cost))
(defmethod label :power [cost]
  (if (< 1 (value cost))
    (quantify (value cost) "hosted power counter")
    "hosted power counter"))
(defmethod payable? :power
  [cost state side eid card]
  (<= 0 (- (get-counters card :power) (value cost))))
(defmethod handler :power
  [cost state side eid card]
  (let [title (:title card)
        card (update! state side (update-in card [:counter :power] - (value cost)))]
    (wait-for (trigger-event-sync state side :counter-added card)
              (complete-with-result
                state side eid
                {:paid/msg (str "spends "
                               (quantify (value cost) "hosted power counter")
                               " from on " title)
                 :paid/type :power
                 :paid/value (value cost)}))))

;; XPowerCounter
(defmethod value :x-power [_] 0)
(defmethod label :x-power [_] "X hosted power counters")
(defmethod payable? :x-power
  [cost state side eid card]
  (pos? (get-counters card :power)))
(defmethod handler :x-power
  [cost state side eid card]
  (continue-ability
    state side
    {:async true
     :prompt "How many hosted power counters do you want to spend?"
     :choices {:number (req (get-counters card :power))}
     :effect
     (req (let [cost target
                title (:title card)
                card (update! state side (update-in card [:counter :power] - cost))]
            (wait-for (trigger-event-sync state side :counter-added card)
                      (complete-with-result
                        state side eid
                        {:paid/msg (str "spends "
                                       (quantify cost "hosted power counter")
                                       " from on " title)
                         :paid/type :x-power
                         :paid/value cost}))))}
    card nil))

;; VirusCounter
(defmethod value :virus [cost] (:cost/amount cost))
(defmethod label :virus [cost]
  (if (< 1 (value cost))
    (quantify (value cost) "hosted virus counter")
    "hosted virus counter"))
(defmethod payable? :virus
  [cost state side eid card]
  (<= 0 (- (+ (get-counters card :virus)
              (->> (all-active-installed state :runner)
                   (filter #(= "Hivemind" (:title %)))
                   (map #(get-counters % :virus))
                   (reduce +)))
           (value cost))))
(defmethod handler :virus
  [cost state side eid card]
  (if (pos? (->> (all-active-installed state :runner)
                 (filter #(= "Hivemind" (:title %)))
                 (keep #(get-counters % :virus))
                 (reduce +)))
    (wait-for (resolve-ability state side (pick-virus-counters-to-spend card (value cost)) card nil)
              (complete-with-result
                state side eid
                {:paid/msg (str "spends " (:msg async-result))
                 :paid/type :virus
                 :paid/value (:number async-result)
                 :paid/targets (:targets async-result)}))
    (let [title (:title card)
          card (update! state side (update-in card [:counter :virus] - (value cost)))]
        (wait-for (trigger-event-sync state side :counter-added card)
                  (complete-with-result
                    state side eid
                    {:paid/msg (str "spends "
                                   (quantify (value cost) (str "hosted virus counter"))
                                   " from on " title)
                     :paid/type :virus
                     :paid/value (value cost)})))))
