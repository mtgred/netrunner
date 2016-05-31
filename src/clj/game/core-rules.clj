(in-ns 'game.core)

(declare card-init card-str deactivate effect-completed enforce-msg gain-agenda-point get-agenda-points
         handle-end-run is-type? resolve-steal-events show-prompt untrashable-while-rezzed?
         in-corp-scored? update-all-ice win win-decked prevent-draw)

;;;; Functions for applying core Netrunner game rules.

;;; Playing cards.
(defn play-instant
  "Plays an Event or Operation."
  ([state side card] (play-instant state side (make-eid state) card nil))
  ([state side eid? card?] (if (:eid eid?)
                             (play-instant state side eid? card? nil)
                             (play-instant state side (make-eid state) eid? card?)))
  ([state side eid {:keys [title] :as card} {:keys [targets extra-cost no-additional-cost]}]
   (when-not (seq (get-in @state [side :locked (-> card :zone first)]))
     (let [cdef (card-def card)
           additional-cost (if (and (has-subtype? card "Double")
                                    (not (get-in @state [side :register :double-ignore-additional])))
                             (concat (:additional-cost cdef) [:click 1])
                             (:additional-cost cdef))
           eid (if-not eid (make-eid state) eid)]
       ;; ensure the instant can be played
       (if (and (if-let [req (:req cdef)]
                  (req state side eid card targets) true) ; req is satisfied
                (not (and (has-subtype? card "Current")
                          (get-in @state [side :register :cannot-play-current])))
                (not (and (has-subtype? card "Run")
                          (get-in @state [side :register :cannot-run])))
                (not (and (has-subtype? card "Priority")
                          (get-in @state [side :register :spent-click])))) ; if priority, have not spent a click
         (if-let [cost-str (pay state side card :credit (:cost card) extra-cost
                                  (when-not no-additional-cost additional-cost))] ; play cost can be paid
           (let [c (move state side (assoc card :seen true) :play-area)]
             (system-msg state side (str (build-spend-msg cost-str "play") title))
             (trigger-event state side (if (= side :corp) :play-operation :play-event) c)
             (if (has-subtype? c "Current")
               (do (doseq [s [:corp :runner]]
                     (when-let [current (first (get-in @state [s :current]))] ; trash old current
                       (say state side {:user "__system__" :text (str (:title current) " is trashed.")})
                       (trash state side current)))
                   (let [moved-card (move state side (first (get-in @state [side :play-area])) :current)]
                     (card-init state side moved-card)))
               (do (resolve-ability state side (assoc cdef :eid eid) card nil)
                   (when-let [c (some #(when (= (:cid %) (:cid card)) %) (get-in @state [side :play-area]))]
                     (move state side c :discard))
                   (when (has-subtype? card "Terminal")
                     (lose state side :click (-> @state side :click))))))
           ;; could not pay the card's price; mark the effect as being over.
           (effect-completed state side eid card))
         ;; card's req was not satisfied; mark the effect as being over.
         (effect-completed state side eid card))))))

(defn max-draw
  "Put an upper limit on the number of cards that can be drawn in this turn."
  [state side n]
  (swap! state assoc-in [side :register :max-draw] n))

(defn remaining-draws
  "Calculate remaining number of cards that can be drawn this turn if a maximum exists"
  [state side]
  (when-let [max-draw (get-in @state [side :register :max-draw])]
    (let [drawn-this-turn (get-in @state [side :register :drawn-this-turn] 0)]
      (max (- max-draw drawn-this-turn) 0))))

(defn draw
  "Draw n cards from :deck to :hand."
  ([state side] (draw state side 1 nil))
  ([state side n] (draw state side n nil))
  ([state side n {:keys [suppress-event] :as args}]
   (let [active-player (get-in @state [:active-player])
         draws-wanted n
         n (if (and (= side active-player) (get-in @state [active-player :register :max-draw]))
             (min n (remaining-draws state side))
             n)
         deck-count (count (get-in @state [side :deck]))]
     (when (and (= side :corp) (> n deck-count))
       (win-decked state))
     (when-not (and (= side active-player) (get-in @state [side :register :cannot-draw]))
       (let [drawn (zone :hand (take n (get-in @state [side :deck])))]
         (swap! state update-in [side :hand] #(concat % drawn))
         (swap! state update-in [side :deck] (partial drop n))
         (swap! state update-in [side :register :drawn-this-turn] (fnil #(+ % n) 0))
         (when (and (not suppress-event) (pos? deck-count))
           (trigger-event state side (if (= side :corp) :corp-draw :runner-draw) n))
         (when (= 0 (remaining-draws state side))
           (prevent-draw state side))))
     (when (< n draws-wanted)
       (let [prevented (- draws-wanted n)]
         (system-msg state (other-side side) (str "prevents " prevented " card"
                                                  (when (> prevented 1) "s")
                                                  " from being drawn")))))))

;;; Damage
(defn flatline [state]
  (when-not (:winner state)
    (system-msg state :runner "is flatlined")
    (win state :corp "Flatline")))

(defn damage-count
  "Calculates the amount of damage to do, taking into account prevention and boosting effects."
  [state side dtype n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:damage :damage-bonus dtype])) 0))
      (- (or (when-not unpreventable (get-in @state [:damage :damage-prevent dtype])) 0))
      (max 0)))

(defn damage-bonus
  "Registers a bonus of n damage to the next damage application of the given type."
  [state side dtype n]
  (swap! state update-in [:damage :damage-bonus dtype] (fnil #(+ % n) 0)))

(defn damage-prevent
  "Registers a prevention of n damage to the next damage application of the given type."
  [state side dtype n]
  (swap! state update-in [:damage :damage-prevent dtype] (fnil #(+ % n) 0)))

(defn damage-defer
  "Registers n damage of the given type to be deferred until later. (Chronos Protocol.)"
  [state side dtype n]
  (swap! state assoc-in [:damage :defer-damage dtype] n))

(defn get-defer-damage [state side dtype {:keys [unpreventable] :as args}]
  (when-not unpreventable (get-in @state [:damage :defer-damage dtype])))

(defn enable-runner-damage-choice
  [state side]
  (swap! state assoc-in [:damage :damage-choose-runner] true))

(defn enable-corp-damage-choice
  [state side]
  (swap! state assoc-in [:damage :damage-choose-corp] true))

(defn runner-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-runner]))

(defn corp-can-choose-damage?
  [state]
  (get-in @state [:damage :damage-choose-corp]))

(defn damage-choice-priority
  "Determines which side gets to act if either or both have the ability to choose cards for damage.
  Currently just for Chronos Protocol vs Titanium Ribs"
  [state]
  (let [active-player (get-in @state [:active-player])]
    (when (and (corp-can-choose-damage? state) (runner-can-choose-damage? state))
      (if (= active-player :corp)
        (swap! state update-in [:damage] dissoc :damage-choose-runner)
        (swap! state update-in [:damage] dissoc :damage-choose-corp)))))

(defn resolve-damage
  "Resolves the attempt to do n damage, now that both sides have acted to boost or
  prevent damage."
  [state side eid type n {:keys [unpreventable unboostable card] :as args}]
  (swap! state update-in [:damage :defer-damage] dissoc type)
  (damage-choice-priority state)
  (trigger-event state side :pre-resolve-damage type card n)
  (when-not (or (get-in @state [:damage :damage-replace])
                (get-in @state [:damage :damage-choose-runner])
                (get-in @state [:damage :damage-choose-corp]))
    (let [n (if (get-defer-damage state side type args) 0 n)]
      (let [hand (get-in @state [:runner :hand])]
        (when (< (count hand) n)
          (flatline state))
        (when (= type :brain)
          (swap! state update-in [:runner :brain-damage] #(+ % n))
          (swap! state update-in [:runner :hand-size-modification] #(- % n)))
        (doseq [c (take n (shuffle hand))]
          (trash state side c {:unpreventable true :cause type} type))
        (trigger-event state side :damage type card))))
  (effect-completed state side eid card))

(defn damage
  "Attempts to deal n damage of the given type to the runner. Starts the
  prevention/boosting process and eventually resolves the damage."
  ([state side type n] (damage state side (make-eid state) type n nil))
  ([state side type n args] (damage state side (make-eid state) type n args))
  ([state side eid type n {:keys [unpreventable unboostable card] :as args}]
   (swap! state update-in [:damage :damage-bonus] dissoc type)
   (swap! state update-in [:damage :damage-prevent] dissoc type)
   ;; alert listeners that damage is about to be calculated.
   (trigger-event state side :pre-damage type card n)
   (let [n (damage-count state side type n args)]
     (let [prevent (get-in @state [:prevent :damage type])]
       (if (and (not unpreventable) prevent (pos? (count prevent)))
         ;; runner can prevent the damage.
         (do (system-msg state :runner "has the option to prevent damage")
             (show-prompt
               state :runner nil (str "Prevent any of the " n " " (name type) " damage?") ["Done"]
               (fn [choice]
                 (let [prevent (get-in @state [:damage :damage-prevent type])]
                   (when prevent
                     (trigger-event state side :prevented-damage type prevent))
                   (system-msg state :runner
                               (if prevent
                                 (str "prevents " (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                          " " (name type) " damage")
                                 "will not prevent damage"))
                   (resolve-damage state side eid type (max 0 (- n (or prevent 0))) args)))
               {:priority 10}))
         (resolve-damage state side eid type n args))))))


;;; Tagging
(defn tag-count
  "Calculates the number of tags to give, taking into account prevention and boosting effects."
  [state side n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:tag :tag-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:tag :tag-prevent])) 0))
      (max 0)))

(defn tag-prevent [state side n]
  (swap! state update-in [:tag :tag-prevent] (fnil #(+ % n) 0)))

(defn tag-remove-bonus
  "Applies a cost increase of n to removing tags with the click action. (SYNC.)"
  [state side n]
  (swap! state update-in [:runner :tag-remove-bonus] (fnil #(+ % n) 0)))

(defn resolve-tag [state side n args]
  (when (pos? n)
    (gain state :runner :tag n)
    (toast state :runner (str "Took " n " tag" (when (> n 1) "s") "!") "info")
    (trigger-event state side :runner-gain-tag n)))

(defn tag-runner
  "Attempts to give the runner n tags, allowing for boosting/prevention effects."
  ([state side n] (tag-runner state side n nil))
  ([state side n {:keys [unpreventable unboostable card] :as args}]
   (swap! state update-in [:tag] dissoc :tag-bonus :tag-prevent)
   (trigger-event state side :pre-tag card)
   (let [n (tag-count state side n args)]
     (let [prevent (get-in @state [:prevent :tag :all])]
       (if (and (pos? n) (not unpreventable) (pos? (count prevent)))
         (do (system-msg state :runner "has the option to avoid tags")
             (show-prompt
               state :runner nil (str "Avoid any of the " n " tags?") ["Done"]
               (fn [choice]
                 (let [prevent (get-in @state [:tag :tag-prevent])]
                   (system-msg state :runner
                               (if prevent
                                 (str "avoids " (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                      (if (< 1 prevent) " tags" " tag"))
                                 "will not avoid tags"))
                   (resolve-tag state side (max 0 (- n (or prevent 0))) args)))
               {:priority 10}))
         (resolve-tag state side n args))))))


;;; Trashing
(defn trash-resource-bonus
  "Applies a cost increase of n to trashing a resource with the click action. (SYNC.)"
  [state side n]
  (swap! state update-in [:corp :trash-cost-bonus] (fnil #(+ % n) 0)))

(defn trash-prevent [state side type n]
  (swap! state update-in [:trash :trash-prevent type] (fnil #(+ % n) 0)))

(defn resolve-trash
  [state side {:keys [zone type] :as card} {:keys [unpreventable cause keep-server-alive] :as args} & targets]
  (let [cdef (card-def card)
        moved-card (move state (to-keyword (:side card)) card :discard {:keep-server-alive keep-server-alive})]
    (when-let [trash-effect (:trash-effect cdef)]
      (resolve-ability state side trash-effect moved-card (cons cause targets)))
    (swap! state update-in [:per-turn] dissoc (:cid moved-card))))

(defn trash
  "Attempts to trash the given card, allowing for boosting/prevention effects."
  ([state side {:keys [zone type] :as card}] (trash state side card nil))
  ([state side {:keys [zone type] :as card} {:keys [unpreventable cause suppress-event] :as args} & targets]
   (when (not (some #{:discard} zone))
     (if (untrashable-while-rezzed? card)
       (enforce-msg state card "cannot be trashed while installed")
       ;; Card is not enforced untrashable
       (let [ktype (keyword (clojure.string/lower-case type))]
         (when (and (not unpreventable) (not= cause :ability-cost))
           (swap! state update-in [:trash :trash-prevent] dissoc ktype))
         (when (and (not suppress-event) (not= (last zone) :current)) ; Trashing a current does not trigger a trash event.
           (apply trigger-event state side (keyword (str (name side) "-trash")) card cause targets))
         (let [prevent (get-in @state [:prevent :trash ktype])]
           ;; Check for prevention effects
           (if (and (not unpreventable) (not= cause :ability-cost) (pos? (count prevent)))
             (do (system-msg state :runner "has the option to prevent trash effects")
                 (show-prompt state :runner nil
                              (str "Prevent the trashing of " (:title card) "?") ["Done"]
                              (fn [choice]
                                (if-let [prevent (get-in @state [:trash :trash-prevent ktype])]
                                  (do (system-msg state :runner (str "prevents the trashing of " (:title card)))
                                      (swap! state update-in [:trash :trash-prevent] dissoc ktype))
                                  (do (system-msg state :runner (str "will not prevent the trashing of " (:title card)))
                                      (apply resolve-trash state side card args targets))))
                              {:priority 10}))
             ;; No prevention effects; resolve the trash.
             (apply resolve-trash state side card args targets))))))))

(defn trash-cards [state side cards]
  (doseq [c cards] (trash state side c)))

(defn trash-no-cost
  "Trashes a card at no cost while it is being accessed. (Imp.)"
  [state side]
  (when-let [card (:card (first (get-in @state [side :prompt])))]
    (when (is-type? card "Agenda") ; trashing before the :access events actually fire; fire them manually
      (resolve-steal-events state side card))
    (trash state side card)
    (swap! state update-in [side :prompt] rest)
    (when-let [run (:run @state)]
      (when (and (:ended run) (empty? (get-in @state [:runner :prompt])) )
        (handle-end-run state :runner)))))

;;; Agendas
(defn get-agenda-points
  "Apply agenda-point modifications to calculate the number of points this card is worth
  to the given player."
  [state side card]
  (let [base-points (:agendapoints card)
        runner-fn (:agendapoints-runner (card-def card))
        corp-fn (:agendapoints-corp (card-def card))]
    (if (and (= side :runner) (not (nil? runner-fn)))
      (runner-fn state side (make-eid state) card nil)
      (if (and (= side :corp) (not  (nil? corp-fn)))
        (corp-fn state side (make-eid state) card nil)
        base-points))))

(defn advancement-cost-bonus
  "Applies an advancement requirement increase of n the next agenda whose advancement requirement
  is being calculated. (SanSan City Grid.)"
  [state side n]
  (swap! state update-in [:bonus :advancement-cost] (fnil #(+ % n) 0)))

(defn advancement-cost [state side {:keys [advancementcost] :as card}]
  (if (nil? advancementcost)
    nil
    (-> (if-let [costfun (:advancement-cost-bonus (card-def card))]
          (+ advancementcost (costfun state side (make-eid state) card nil))
          advancementcost)
        (+ (or (get-in @state [:bonus :advancement-cost]) 0))
        (max 0))))

(defn update-advancement-cost
  "Recalculates the advancement requirement for the given agenda."
  [state side agenda]
  (swap! state update-in [:bonus] dissoc :advancement-cost)
  (trigger-event state side :pre-advancement-cost agenda)
  (update! state side (assoc agenda :current-cost (advancement-cost state side agenda))))

(defn update-all-advancement-costs [state side]
  (doseq [ag (->> (mapcat :content (flatten (seq (get-in @state [:corp :servers]))))
                  (filter #(is-type? % "Agenda")))]
    (update-advancement-cost state side ag)))

(defn as-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points."
  [state side card n]
  (move state side (assoc card :agendapoints n) :scored)
  (gain-agenda-point state side n))

(defn forfeit
  "Forfeits the given agenda to the :rfg zone."
  [state side card]
  (let [c (if (in-corp-scored? state side card)
            (deactivate state side card) card)]
    (system-msg state side (str "forfeits " (:title c)))
    (gain-agenda-point state side (- (get-agenda-points state side c)))
    (move state :corp c :rfg)))

(defn gain-agenda-point
  "Gain n agenda points and check for winner."
  [state side n]
  (gain state side :agenda-point n)
  (when (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
    (win state side "Agenda")))


;;; Miscellaneous
(defn purge
  "Purges viruses."
  [state side]
  (trigger-event state side :pre-purge)
  (let [rig-cards (apply concat (vals (get-in @state [:runner :rig])))
        hosted-cards (filter :installed (mapcat :hosted rig-cards))
        hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
    (doseq [card (concat rig-cards hosted-cards hosted-on-ice)]
      (when (or (has-subtype? card "Virus")
                (contains? (:counter card) :virus))
        (add-counter state :runner card :virus (- (get-in card [:counter :virus] 0)))))
    (update-all-ice state side))
  (trigger-event state side :purge))

(defn mill
  "Force the discard of n cards from :deck to :discard."
  ([state side] (mill state side 1))
  ([state side n]
   (let [milled (zone :discard (take n (get-in @state [side :deck])))]
     (swap! state update-in [side :discard] #(concat % milled)))
   (swap! state update-in [side :deck] (partial drop n))))

(defn expose
  "Exposes the given card."
  [state side target]
  (system-msg state side
              (str "exposes " (card-str state target {:visible true})))
  (when-let [ability (:expose (card-def target))]
    (resolve-ability state side ability target nil))
  (trigger-event state side :expose target))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update-in [side] dissoc :openhand))

(defn win
  "Records a win reason for statistics."
  [state side reason]
  (system-msg state side "wins the game")
  (swap! state assoc :winner side :reason reason :end-time (java.util.Date.)))

(defn win-decked
  "Records a win via decking the corp."
  [state]
  (system-msg state :corp "is decked")
  (win state :runner "Decked"))

(defn init-trace-bonus
  "Applies a bonus base strength of n to the next trace attempt."
  [state side n]
  (swap! state update-in [:bonus :trace] (fnil #(+ % n) 0)))
