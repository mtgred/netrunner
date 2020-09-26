(in-ns 'game.core)

(declare remove-old-current trash-cards)

;;;; Functions for applying core Netrunner game rules.

(defn fake-move
  [state side eid c args]
  (move state side c :rfg)
  (effect-completed state side eid))

(defn current-handler
  [state side eid card]
  (if (has-subtype? card "Current")
    (wait-for (remove-old-current state side :corp)
              (wait-for (remove-old-current state side :runner)
                        (let [c (some #(when (same-card? % card) %) (get-in @state [side :play-area]))
                              c (move state side c :current)]
                          (effect-completed state side (make-result eid c)))))
    (effect-completed state side (make-result eid card))))

;;; Playing cards.
(defn- complete-play-instant
  "Completes the play of the event / operation that the player can play for"
  [state side eid {:keys [title] :as card} cost-str ignore-cost]
  (let [play-msg (if ignore-cost
                   "play "
                   (build-spend-msg cost-str "play"))]
    (system-msg state side (str play-msg title (when ignore-cost " at no cost")))
    (play-sfx state side "play-instant")
    ;; Need to await trashing the existing current
    (wait-for
      (current-handler state side card)
      ;; Select the "on the table" version of the card
      (let [card async-result
            cdef (card-def card)]
        (when card
          (card-init state side (if (:rfg-instead-of-trashing cdef)
                                  (assoc card :rfg-instead-of-trashing true)
                                  card)
                     {:resolve-effect false :init-data true}))
        (let [card (get-card state card)]
          (wait-for (trigger-event-sync state side (if (= side :corp) :play-operation :play-event) card)
                    ;; Resolve ability, removing :req as that has already been checked
                    (wait-for (resolve-ability state side (dissoc cdef :req :cost :additional-cost) card nil)
                              (let [c (some #(when (same-card? card %) %) (get-in @state [side :play-area]))
                                    trash-after-resolving (:trash-after-resolving cdef true)
                                    zone (if (:rfg-instead-of-trashing c) :rfg :discard)]
                                (if (and c trash-after-resolving)
                                  (let [trash-or-move (if (= zone :rfg) fake-move trash)]
                                    (wait-for (trash-or-move state side c {:unpreventable true})
                                              (unregister-events state side card)
                                              (unregister-constant-effects state side card)
                                              (when (= zone :rfg)
                                                (system-msg state side
                                                            (str "removes " (:title c) " from the game instead of trashing it")))
                                              (when (has-subtype? card "Terminal")
                                                (lose state side :click (-> @state side :click))
                                                (swap! state assoc-in [:corp :register :terminal] true))
                                              (effect-completed state side eid)))
                                  (do (when (has-subtype? card "Terminal")
                                        (lose state side :click (-> @state side :click))
                                        (swap! state assoc-in [:corp :register :terminal] true))
                                      (effect-completed state side eid)))))))))))

(defn play-instant
  "Plays an Event or Operation."
  ([state side eid card {:keys [targets ignore-cost base-cost no-additional-cost]}]
   (let [eid (eid-set-defaults eid :source nil :source-type :play)
         cdef (card-def card)
         cost (play-cost state side card)
         additional-costs (play-additional-cost-bonus state side card)
         costs (merge-costs
                 [(when-not ignore-cost
                    [base-cost [:credit cost]])
                  (when (and (has-subtype? card "Triple")
                             (not no-additional-cost))
                    [:click 2])
                  (when (and (has-subtype? card "Double")
                             (not no-additional-cost)
                             (not (get-in @state [side :register :double-ignore-additional])))
                    [:click 1])
                  (when-not (and no-additional-cost ignore-cost)
                    [additional-costs])])
         eid (if-not eid (make-eid state) eid)]
     ;; ensure the instant can be played
     (if (and ;; req is satisfied
              (should-trigger? state side eid card targets cdef)
              ;; The zone isn't locked
              (empty? (get-in @state [side :locked (-> card :zone first)]))
              ;; This is a current, and currents can be played
              (not (and (has-subtype? card "Current")
                        (get-in @state [side :register :cannot-play-current])))
              ;; This is a run event or makes a run, and running is allowed
              (not (and (or (:makes-run cdef)
                            (has-subtype? card "Run"))
                        (not (can-run? state :runner))))
              ;; if priority, have not spent a click
              (not (and (has-subtype? card "Priority")
                        (get-in @state [side :register :spent-click]))))
       ;; Wait on pay to finish before triggering instant-effect
       (let [original-zone (:zone card)
             moved-card (move state side (assoc card :seen true) :play-area)]
         ;; Only mark the register once costs have been paid and card has been moved
         (if (has-subtype? card "Run")
           (swap! state assoc-in [:runner :register :click-type] :run))
         (wait-for (pay state side (make-eid state eid) moved-card costs {:action :play-instant})
                   (if-let [cost-str async-result]
                     (complete-play-instant state side eid moved-card cost-str ignore-cost)
                     ;; could not pay the card's price; put it back and mark the effect as being over.
                     (do
                       (move state side moved-card original-zone)
                       (effect-completed state side eid)))))
       ;; card's req or other effects was not satisfied; mark the effect as being over.
       (effect-completed state side eid)))))

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

(defn- damage-prevent-update-prompt
  "Look at the current runner prompt and (if a damage prevention prompt), update message."
  [state side dtype n]
  (if-let [oldprompt (first (get-in @state [side :prompt]))]
    (if-let [match (re-matches #"^Prevent any of the (\d+) (\w+) damage\?.*" (:msg oldprompt))]
      (let [dnumber (str->int (second match))
            promptdtype (case (nth match 2)
                          "net" :net
                          "brain" :brain
                          "meat" :meat)
            prevented (get-in @state [:damage :damage-prevent promptdtype] 0)
            newprompt (assoc oldprompt :msg (str "Prevent any of the " dnumber " " (name promptdtype) " damage? (" prevented "/" dnumber " prevented)"))
            update-fn #(cons newprompt (rest %))]
        (if (>= prevented dnumber)
          (do (swap! state update-in [side :prompt] next)
              ((:effect oldprompt) nil))
          (swap! state update-in [side :prompt] update-fn))))))

(defn damage-prevent
  "Registers a prevention of n damage to the next damage application of the given type. Afterwards update current prevention prompt, if found."
  [state side dtype n]
  (swap! state update-in [:damage :damage-prevent dtype] (fnil #(+ % n) 0))
  (damage-prevent-update-prompt state side dtype n))

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

(defn handle-replaced-damage
  [state side eid]
  (swap! state update-in [:damage :defer-damage] dissoc type)
  (swap! state update-in [:damage] dissoc :damage-replace)
  (effect-completed state side eid))

(defn chosen-damage
  [state side & targets]
  (swap! state update-in [:damage :chosen-damage] #(apply conj % (flatten targets))))

(defn get-chosen-damage
  [state]
  (get-in @state [:damage :chosen-damage]))

(defn resolve-damage
  "Resolves the attempt to do n damage, now that both sides have acted to boost or
  prevent damage."
  [state side eid type n {:keys [unpreventable unboostable card] :as args}]
  (swap! state update-in [:damage :defer-damage] dissoc type)
  (swap! state dissoc-in [:damage :chosen-damage])
  (damage-choice-priority state)
  (wait-for (trigger-event-simult state side :pre-resolve-damage nil type side n)
            (if (get-in @state [:damage :damage-replace])
              (handle-replaced-damage state side eid)
              (if (pos? n)
                (let [hand (get-in @state [:runner :hand])
                      chosen-cards (seq (get-chosen-damage state))
                      chosen-cids (into #{} (map :cid chosen-cards))
                      leftovers (remove #(contains? chosen-cids (:cid %)) hand)
                      cards-trashed (filter identity (flatten (conj chosen-cards (seq (take (- n (count chosen-cards)) (shuffle leftovers))))))]
                  (when (= type :brain)
                    (swap! state update-in [:runner :brain-damage] #(+ % n))
                    (swap! state update-in [:runner :hand-size :mod] #(- % n)))
                  (when-let [trashed-msg (join ", " (map :title cards-trashed))]
                    (system-msg state :runner (str "trashes " trashed-msg " due to " (name type) " damage")))
                  (if (< (count hand) n)
                    (do (flatline state)
                        (swap! state update-in [:stats :corp :damage :all] (fnil + 0) n)
                        (swap! state update-in [:stats :corp :damage type] (fnil + 0) n)
                        (trash-cards state side eid cards-trashed {:unpreventable true}))
                    (wait-for (trash-cards state side cards-trashed {:unpreventable true :cause type})
                              (swap! state update-in [:stats :corp :damage :all] (fnil + 0) n)
                              (swap! state update-in [:stats :corp :damage type] (fnil + 0) n)
                              (trigger-event state side :damage type card n cards-trashed)
                              (complete-with-result state side eid (seq [type card n cards-trashed])))))
                (effect-completed state side eid)))))

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
   (let [n (damage-count state side type n args)
         prevent (get-prevent-list state :runner type)]
     (if (and (not unpreventable) (cards-can-prevent? state :runner prevent type))
       ;; runner can prevent the damage.
       (do (system-msg state :runner "has the option to avoid damage")
           (show-wait-prompt state :corp "Runner to prevent damage" {:priority 10})
           (swap! state assoc-in [:prevent :current] type)
           (show-prompt
             state :runner nil (str "Prevent any of the " n " " (name type) " damage?") ["Done"]
             (fn [_] (let [prevent (get-in @state [:damage :damage-prevent type])]
                       (when prevent (trigger-event state side :prevented-damage type prevent))
                       (system-msg state :runner
                                   (if prevent (str "prevents " (if (>= prevent Integer/MAX_VALUE) "all" prevent)
                                                    " " (name type) " damage") "will not prevent damage"))
                       (clear-wait-prompt state :corp)
                       (resolve-damage state side eid type (max 0 (- n (or prevent 0))) args)))
             {:priority 10}))
       (resolve-damage state side eid type n args)))))


;;; Tagging
(defn tag-count
  "Calculates the number of tags to give, taking into account prevention and boosting effects."
  [state side n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:tag :tag-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:tag :tag-prevent])) 0))
      (max 0)))

(defn tag-prevent
  ([state side eid n]
   (swap! state update-in [:tag :tag-prevent] (fnil #(+ % n) 0))
   (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) (list :tag n))))

(defn tag-remove-bonus
  "Applies a cost increase of n to removing tags with the click action. (SYNC.)"
  [state side n]
  (swap! state update-in [:runner :tag-remove-bonus] (fnil #(+ % n) 0)))

(defn resolve-tag
  "Resolve runner gain tags. Always gives `:base` tags."
  [state side eid n _]
  (trigger-event state side :pre-resolve-tag n)
  (if (pos? n)
    (do (gain state :runner :tag {:base n})
        (toast state :runner (str "Took " (quantify n "tag") "!") "info")
        (trigger-event-sync state side eid :runner-gain-tag n))
    (effect-completed state side eid)))

(defn gain-tags
  "Attempts to give the runner n tags, allowing for boosting/prevention effects."
  ([state side eid n] (gain-tags state side eid n nil))
  ([state side eid n {:keys [unpreventable unboostable card] :as args}]
   (swap! state update-in [:tag] dissoc :tag-bonus :tag-prevent)
   (wait-for (trigger-event-simult state side :pre-tag nil card)
             (let [n (tag-count state side n args)
                   prevent (get-prevent-list state :runner :tag)]
               (if (and (pos? n)
                        (not unpreventable)
                        (cards-can-prevent? state :runner prevent :tag))
                 (do (system-msg state :runner "has the option to avoid tags")
                     (show-wait-prompt state :corp "Runner to prevent tags" {:priority 10})
                     (swap! state assoc-in [:prevent :current] :tag)
                     (show-prompt
                       state :runner nil
                       (str "Avoid " (when (< 1 n) "any of the ") (quantify n "tag") "?") ["Done"]
                       (fn [_]
                         (let [prevent (get-in @state [:tag :tag-prevent])
                               prevent-msg (if prevent
                                             (str "avoids "
                                                  (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                                  (if (< 1 prevent) " tags" " tag"))
                                             "will not avoid tags")]
                           (system-msg state :runner prevent-msg)
                           (clear-wait-prompt state :corp)
                           (resolve-tag state side eid (max 0 (- n (or prevent 0))) args)))
                       {:priority 10}))
                 (resolve-tag state side eid n args))))))

(defn lose-tags
  "Always removes `:base` tags"
  ([state side eid n]
   (if (= n :all)
     (lose-tags state side eid (get-in @state [:runner :tag :base]))
     (do (swap! state update-in [:stats :runner :lose :tag] (fnil + 0) n)
         (deduct state :runner [:tag {:base n}])
         (trigger-event-sync state side eid :runner-lose-tag n side)))))


;;;; Bad Publicity
(defn bad-publicity-count
  "Calculates the number of bad publicity to give, taking into account prevention and boosting effects."
  [state side n {:keys [unpreventable unboostable] :as args}]
  (-> n
      (+ (or (when-not unboostable (get-in @state [:bad-publicity :bad-publicity-bonus])) 0))
      (- (or (when-not unpreventable (get-in @state [:bad-publicity :bad-publicity-prevent])) 0))
      (max 0)))

(defn bad-publicity-prevent [state side n]
  (swap! state update-in [:bad-publicity :bad-publicity-prevent] (fnil #(+ % n) 0))
  (trigger-event state side (if (= side :corp) :corp-prevent :runner-prevent) `(:bad-publicity ~n)))

(defn resolve-bad-publicity [state side eid n args]
  (trigger-event state side :pre-resolve-bad-publicity n)
  (if (pos? n)
    (do (gain state :corp :bad-publicity n)
        (toast state :corp (str "Took " n " bad publicity!") "info")
        (trigger-event-sync state side (make-result eid n) :corp-gain-bad-publicity n))
    (effect-completed state side eid)))

(defn gain-bad-publicity
  "Attempts to give the corp n bad publicity, allowing for boosting/prevention effects."
  ([state side n] (gain-bad-publicity state side (make-eid state) n nil))
  ([state side eid n] (gain-bad-publicity state side eid n nil))
  ([state side eid n {:keys [unpreventable card] :as args}]
   (swap! state update-in [:bad-publicity] dissoc :bad-publicity-bonus :bad-publicity-prevent)
   (wait-for (trigger-event-sync state side :pre-bad-publicity card)
             (let [n (bad-publicity-count state side n args)
                   prevent (get-prevent-list state :corp :bad-publicity)]
               (if (and (pos? n)
                        (not unpreventable)
                        (cards-can-prevent? state :corp prevent :bad-publicity))
                 (do (system-msg state :corp "has the option to avoid bad publicity")
                     (show-wait-prompt state :runner "Corp to prevent bad publicity" {:priority 10})
                     (swap! state assoc-in [:prevent :current] :bad-publicity)
                     (show-prompt
                       state :corp nil
                       (str "Avoid " (when (< 1 n) "any of the ") n " bad publicity?") ["Done"]
                       (fn [_]
                         (let [prevent (get-in @state [:bad-publicity :bad-publicity-prevent])]
                           (system-msg state :corp
                                       (if prevent
                                         (str "avoids "
                                              (if (= prevent Integer/MAX_VALUE) "all" prevent)
                                              " bad publicity")
                                         "will not avoid bad publicity"))
                           (clear-wait-prompt state :runner)
                           (resolve-bad-publicity state side eid (max 0 (- n (or prevent 0))) args)))
                       {:priority 10}))
                 (resolve-bad-publicity state side eid n args))))))

(defn lose-bad-publicity
  ([state side n] (lose-bad-publicity state side (make-eid state) n))
  ([state side eid n]
   (if (= n :all)
     (lose-bad-publicity state side eid (get-in @state [:corp :bad-publicity :base]))
     (do (lose state :corp :bad-publicity n)
         (trigger-event-sync state side eid :corp-lose-bad-publicity n side)))))


;;; Miscellaneous
(defn purge
  "Purges viruses."
  [state side]
  (trigger-event state side :pre-purge)
  (let [installed-cards (concat (all-installed state :runner)
                                (all-installed state :corp))
        hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
    (doseq [card (concat installed-cards hosted-on-ice)]
      (when (or (has-subtype? card "Virus")
                (contains? (:counter card) :virus))
        (add-counter state :runner card :virus (- (get-counters card :virus)))))
    (update-all-ice state side))
  (trigger-event state side :purge))

(defn mill
  "Force the discard of n cards from the deck by trashing them"
  [state from-side eid to-side n]
  (let [cards (take n (get-in @state [to-side :deck]))]
    (trash-cards state from-side eid cards {:unpreventable true})))

(defn discard-from-hand
  "Force the discard of n cards from the hand by trashing them"
  [state from-side eid to-side n]
  (let [cards (take n (shuffle (get-in @state [to-side :hand])))]
    (trash-cards state from-side eid cards {:unpreventable true})))

(defn change-hand-size
  "Changes a side's hand-size modification by specified amount (positive or negative)"
  [state side n]
  (gain state side :hand-size {:mod n}))

