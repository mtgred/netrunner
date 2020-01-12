(in-ns 'game.core)

(declare can-run? can-trash? card-init card-str cards-can-prevent? check-winner close-access-prompt
         enforce-msg get-prevent-list get-agenda-points in-corp-scored? play-sfx
         prevent-draw remove-old-current should-trigger? system-say system-msg steal-trigger-events
         trash-cards untrashable-while-rezzed? update-all-agenda-points update-all-ice
         untrashable-while-resources? win win-decked)

;;;; Functions for applying core Netrunner game rules.

(defn fake-move
  [state side eid c args]
  (move state side c :rfg)
  (effect-completed state side eid))

;;; Playing cards.
(defn- complete-play-instant
  "Completes the play of the event / operation that the player can play for"
  [state side eid {:keys [title] :as card} cost-str ignore-cost]
  (let [play-msg (if ignore-cost
                   "play "
                   (build-spend-msg cost-str "play"))]
    (system-msg state side (str play-msg title (when ignore-cost " at no cost")))
    (play-sfx state side "play-instant")
    (when (has-subtype? card "Current")
      (doseq [s [:corp :runner]]
        (remove-old-current state side s))
      (let [c (some #(when (same-card? % card) %) (get-in @state [side :play-area]))]
        (move state side c :current)))
    ;; Select the "on the table" version of the card
    (let [cdef (card-def card)
          card (some #(when (same-card? % card) %) (concat (get-in @state [side :play-area]) (get-in @state [side :current])))]
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
                                    (effect-completed state side eid))))))))))

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
       ;; Wait on pay-sync to finish before triggering instant-effect
       (let [original-zone (:zone card)
             moved-card (move state side (assoc card :seen true) :play-area)]
         ;; Only mark the register once costs have been paid and card has been moved
         (if (has-subtype? card "Run")
           (swap! state assoc-in [:runner :register :click-type] :run))
         (wait-for (pay-sync state side (make-eid state eid) moved-card costs {:action :play-instant})
                   (if-let [cost-str async-result]
                     (complete-play-instant state side eid moved-card cost-str ignore-cost)
                     ;; could not pay the card's price; put it back and mark the effect as being over.
                     (do
                       (move state side moved-card original-zone)
                       (effect-completed state side eid)))))
       ;; card's req or other effects was not satisfied; mark the effect as being over.
       (effect-completed state side eid)))))

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

(defn draw-bonus
  "Registers a bonus of n draws to the next draw (Daily Business Show)"
  [state side n]
  (swap! state update-in [:bonus :draw] (fnil #(+ % n) 0)))

(defn draw
  "Draw n cards from :deck to :hand."
  ([state side] (draw state side (make-eid state) 1 nil))
  ([state side n] (draw state side (make-eid state) n nil))
  ([state side n args] (draw state side (make-eid state) n args))
  ([state side eid n {:keys [suppress-event] :as args}]
   (swap! state update-in [side :register] dissoc :most-recent-drawn) ;clear the most recent draw in case draw prevented
   (wait-for (trigger-event-simult state side (if (= side :corp) :pre-corp-draw :pre-runner-draw) nil n)
             (let [active-player (get-in @state [:active-player])
                   n (+ n (get-in @state [:bonus :draw] 0))
                   draws-wanted n
                   draws-after-prevent (if (and (= side active-player) (get-in @state [active-player :register :max-draw]))
                                         (min n (remaining-draws state side))
                                         n)
                   deck-count (count (get-in @state [side :deck]))]
               (when (and (= side :corp) (> draws-after-prevent deck-count))
                 (win-decked state))
               (if (or (and (= side active-player)
                            (get-in @state [side :register :cannot-draw]))
                       (not (pos? draws-after-prevent))
                       (not (pos? deck-count)))
                 (effect-completed state side eid)
                 (let [to-draw (take draws-after-prevent (get-in @state [side :deck]))
                       drawn (doall (for [card to-draw] (move state side card :hand)))]
                   (swap! state assoc-in [side :register :most-recent-drawn] drawn)
                   (swap! state update-in [side :register :drawn-this-turn] (fnil #(+ % draws-after-prevent) 0))
                   (swap! state update-in [:stats side :gain :card] (fnil + 0) n)
                   (swap! state update-in [:bonus] dissoc :draw)
                   (if (and (not suppress-event) (pos? deck-count))
                     (wait-for
                       (trigger-event-sync state side (if (= side :corp) :corp-draw :runner-draw) draws-after-prevent)
                       (trigger-event-sync state side eid (if (= side :corp) :post-corp-draw :post-runner-draw) draws-after-prevent))
                     (effect-completed state side eid))
                   (when (safe-zero? (remaining-draws state side))
                     (prevent-draw state side))))
               (when (< draws-after-prevent draws-wanted)
                 (let [prevented (- draws-wanted draws-after-prevent)]
                   (system-msg state (other-side side) (str "prevents "
                                                            (quantify prevented "card")
                                                            " from being drawn"))))))))

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
  (wait-for (trigger-event-sync state side :pre-resolve-damage type card n)
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
                              (effect-completed state side eid))))
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
  ([state side n] (tag-prevent state side (make-eid state) n))
  ([state side eid n]
   (swap! state update-in [:tag :tag-prevent] (fnil #(+ % n) 0))
   (trigger-event-sync state side eid (if (= side :corp) :corp-prevent :runner-prevent) `(:tag ~n))))

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
  ([state side n] (gain-tags state side (make-eid state) n nil))
  ([state side eid n] (gain-tags state side eid n nil))
  ([state side eid n {:keys [unpreventable unboostable card] :as args}]
   (swap! state update-in [:tag] dissoc :tag-bonus :tag-prevent)
   (trigger-event state side :pre-tag card)
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
       (resolve-tag state side eid n args)))))

(defn lose-tags
  "Always removes `:base` tags"
  ([state side n] (lose-tags state side (make-eid state) n))
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
        (trigger-event-sync state side eid :corp-gain-bad-publicity n))
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

;;; Trashing
(defn trash-resource-bonus
  "Applies a cost increase of n to trashing a resource with the click action. (SYNC.)"
  [state side n]
  (swap! state update-in [:corp :trash-cost-bonus] (fnil #(+ % n) 0)))

(defn trash-prevent [state side type n]
  (swap! state update-in [:trash :trash-prevent type] (fnil #(+ % n) 0)))

(defn- prevent-trash
  ([state side card oid] (prevent-trash state side (make-eid state) card oid nil))
  ([state side card oid args] (prevent-trash state side (make-eid state) card oid args))
  ([state side eid {:keys [zone type] :as card} oid
    {:keys [unpreventable cause keep-server-alive suppress-event] :as args}]
   (if (and card (not-any? #{:discard} zone))
     (cond

       (untrashable-while-rezzed? card)
       (do (enforce-msg state card "cannot be trashed while installed")
           (effect-completed state side eid))

       (and (= side :runner)
            (not (can-trash? state side card)))
       (do (enforce-msg state card "cannot be trashed")
           (effect-completed state side eid))

       (and (= side :corp)
            (untrashable-while-resources? card)
            (> (count (filter resource? (all-active-installed state :runner))) 1))
       (do (enforce-msg state card "cannot be trashed while there are other resources installed")
           (effect-completed state side eid))

       ;; Card is not enforced untrashable
       :else
       (let [ktype (keyword (string/lower-case type))]
         (when (and (not unpreventable)
                    (not= cause :ability-cost))
           (swap! state update-in [:trash :trash-prevent] dissoc ktype))
         (let [type (->> ktype name (str "trash-") keyword)
               prevent (get-prevent-list state :runner type)]
           ;; Check for prevention effects
           (if (and (not unpreventable)
                    (not= cause :ability-cost)
                    (cards-can-prevent? state :runner prevent type card args))
             (do (system-msg state :runner "has the option to prevent trash effects")
                 (show-wait-prompt state :corp "Runner to prevent trash effects" {:priority 10})
                 (show-prompt state :runner nil
                              (str "Prevent the trashing of " (:title card) "?") ["Done"]
                              (fn [_]
                                (clear-wait-prompt state :corp)
                                (if-let [_ (get-in @state [:trash :trash-prevent ktype])]
                                  (do (system-msg state :runner (str "prevents the trashing of " (:title card)))
                                      (swap! state update-in [:trash :trash-prevent] dissoc ktype)
                                      (effect-completed state side eid))
                                  (do (system-msg state :runner (str "will not prevent the trashing of " (:title card)))
                                      (swap! state update-in [:trash :trash-list oid] concat [card])
                                      (effect-completed state side eid))))
                              {:priority 10}))
             ;; No prevention effects: add the card to the trash-list
             (do (swap! state update-in [:trash :trash-list oid] concat [card])
                 (effect-completed state side eid))))))
     (effect-completed state side eid))))

(defn trash-cards
  "Attempts to trash each given card, and then once all given cards have been either
  added or not added to the trash list, all of those cards are trashed"
  ([state side cards] (trash-cards state side (make-eid state) cards nil))
  ([state side eid cards] (trash-cards state side eid cards nil))
  ([state side eid cards {:keys [cause keep-server-alive host-trashed game-trash] :as args}]
   (let [num-cards (< 1 (count cards))]
     (letfn [(get-card? [s c]
               ;; Holdover from old code. Should be `get-card` in all cases, but that
               ;; requires fixing a bunch of cards and there's not been time yet.
               (if num-cards (get-card s c) c))
             (preventrec [cs]
               (if (seq cs)
                 (wait-for (prevent-trash state side (get-card? state (first cs)) eid args)
                           (preventrec (rest cs)))
                 (let [trashlist (get-in @state [:trash :trash-list eid])
                       ;; Criteria for abilities that trigger when the card is trashed
                       get-trash-effect (fn [card]
                                          (when (and card
                                                     (not (:disabled card))
                                                     (or (and (runner? card)
                                                              (installed? card)
                                                              (not (facedown? card)))
                                                         (and (rezzed? card)
                                                              (not host-trashed))
                                                         (and (:when-inactive (:trash-effect (card-def card)))
                                                              (not host-trashed))
                                                         (in-play-area? card)))
                                            (:trash-effect (card-def card))))
                       ;; No card should end up in the opponent's discard pile, so instead
                       ;; of using `side`, we use the card's `:side`.
                       move-card #(move state (to-keyword (:side %)) % :discard {:keep-server-alive keep-server-alive})
                       ;; Perform the move of the cards from their current location to
                       ;; the discard. At the same time, gather their `:trash-effect`s
                       ;; to be used in the simult event later.
                       moved-cards (->> trashlist
                                        (map #(get-card? state %))
                                        (filter identity)
                                        ;; juxt is used to perform both the move and
                                        ;; `get-trash-effect` on each card in the list.
                                        ;; This gives us a list of tuples:
                                        ;; the moved card and the trash effect
                                        ;; This is used when we build the `card-abilities`
                                        ;; list of effects, applying the pair to
                                        ;; `ability-as-handler`, which is the format
                                        ;; `trigger-event-simult` handles the additional
                                        ;; abilities.
                                        (map (juxt move-card get-trash-effect))
                                        (map #(apply ability-as-handler %))
                                        (into []))]
                   (swap! state update-in [:trash :trash-list] dissoc eid)
                   (when (seq (remove #{side} (map #(to-keyword (:side %)) trashlist)))
                     (swap! state assoc-in [side :register :trashed-card] true))
                   (let [;; The trash event will be determined by who is performing the
                         ;; trash. `:game-trash` in this case refers to when a checkpoint
                         ;; sees a card has been trashed and it has hosted cards, so it
                         ;; trashes each hosted card. (Rule 10.3.1g)
                         ;; This doesn't count as either player trashing the card, but
                         ;; the cards are counted as trashed by the engine and so
                         ;; abilities that don't care who performed the trash (Simulchip
                         ;; for example) still need it either logged or watchable.
                         trash-event (cond
                                       game-trash :game-trash
                                       (= side :corp) :corp-trash
                                       (= side :runner) :runner-trash)
                         targets (concat trashlist (list {:cause cause}))]
                     (apply trigger-event-simult state side eid trash-event {:card-abilities moved-cards} targets)))))]
       (preventrec cards)))))

(defn trash
  ([state side card] (trash-cards state side (make-eid state) [card] nil))
  ([state side card args] (trash-cards state side (make-eid state) [card] args))
  ([state side eid card args] (trash-cards state side eid [card] args)))

;;; Agendas
(defn get-agenda-points
  "Apply agenda-point modifications to calculate the number of points this card is worth
  to the given player."
  [state side card]
  (let [base-points (:agendapoints card 0)
        as-agenda-points (:as-agenda-points card 0)
        points-fn (if (= side :corp)
                    (:agendapoints-corp (card-def card))
                    (:agendapoints-runner (card-def card)))]
    (if (fn? points-fn)
      (points-fn state side nil card nil)
      (+ base-points
         as-agenda-points
         (sum-effects state side card :agenda-value nil)))))

(defn advancement-cost-bonus
  "Applies an advancement requirement increase of n the next agenda whose advancement requirement
  is being calculated. (SanSan City Grid.)"
  [state side n]
  (swap! state update-in [:bonus :advancement-cost] (fnil #(+ % n) 0)))

(defn advancement-cost [state side {:keys [advancementcost] :as card}]
  (if (some? advancementcost)
    (-> (if-let [costfun (:advancement-cost-bonus (card-def card))]
          (+ advancementcost (costfun state side (make-eid state) card nil))
          advancementcost)
        (+ (get-in @state [:bonus :advancement-cost] 0))
        (max 0))))

(defn update-advancement-cost
  "Recalculates the advancement requirement for the given agenda."
  [state side agenda]
  (swap! state update-in [:bonus] dissoc :advancement-cost)
  (trigger-event state side :pre-advancement-cost agenda)
  (update! state side (assoc agenda :current-cost (advancement-cost state side agenda))))

(defn update-all-advancement-costs [state side]
  (->> (get-in @state [:corp :servers])
       seq
       flatten
       (mapcat :content)
       (filter agenda?)
       (mapv #(update-advancement-cost state side %))))

(defn as-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points."
  ([state side card n] (as-agenda state side (make-eid state) card n nil))
  ([state side eid card n] (as-agenda state side eid card n nil))
  ([state side eid card n {:keys [register-events force]}]
   (let [card (deactivate state side card)
         card (move state side (assoc card :agendapoints n) :scored {:force force})]
     (if register-events
       (wait-for (card-init state side card {:resolve-effect false})
                 (wait-for (resolve-ability state side (make-eid state eid) (:swapped (card-def card)) card nil)
                           (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
                                     (update-all-agenda-points state side)
                                     (check-winner state side)
                                     (effect-completed state side eid))))
       (wait-for (trigger-event-sync state side :as-agenda (assoc card :as-agenda-side side :as-agenda-points n))
                 (update-all-agenda-points state side)
                 (check-winner state side)
                 (effect-completed state side eid))))))

(defn forfeit
  "Forfeits the given agenda to the :rfg zone."
  ([state side card] (forfeit state side (make-eid state) card))
  ([state side eid card] (forfeit state side eid card {:msg true}))
  ([state side eid card args]
   ;; Remove all hosted cards first
   (doseq [h (:hosted card)]
     (trash state side
            (update-in h [:zone] #(map to-keyword %))
            {:unpreventable true :suppress-event true}))
   (let [card (get-card state card)]
     (when (:msg args)
       (system-msg state side (str "forfeits " (:title card))))
     (move state (to-keyword (:side card)) card :rfg)
     (update-all-agenda-points state side)
     (check-winner state side)
     (trigger-event-sync state side eid (keyword (str (name side) "-forfeit-agenda")) card))))

(defn check-winner
  [state side]
  (doseq [side [:corp :runner]]
    (when (and (>= (get-in @state [side :agenda-point]) (get-in @state [side :agenda-point-req]))
               (not (any-effects state side :cannot-win-on-points)))
      (win state side "Agenda"))))

(defn update-agenda-points-card
  [state side card]
  (update! state side (assoc card :current-points (get-agenda-points state side card))))

(defn sum-agenda-points
  [state side]
  (let [user-adjusted-points (sum-effects state side side :user-agenda-points nil)
        scored-points (->> (get-in @state [side :scored])
                           (map :current-points)
                           (reduce + 0))
        total-points (+ user-adjusted-points scored-points)]
    (swap! state assoc-in [side :agenda-point] total-points)))

(defn update-agenda-points
  [state side]
  (doseq [card (get-in @state [side :scored])]
    (update-agenda-points-card state side card))
  (sum-agenda-points state side))

(defn update-all-agenda-points
  [state side]
  (doseq [side [:corp :runner]]
    (update-agenda-points state side)))


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
  ([state side] (mill state side (make-eid state) side 1))
  ([state side n] (mill state side (make-eid state) side n))
  ([state from-side to-side n] (mill state from-side (make-eid state) to-side n))
  ([state from-side eid to-side n]
   (let [cards (take n (get-in @state [to-side :deck]))]
     (trash-cards state from-side eid cards {:unpreventable true}))))

(defn discard-from-hand
  "Force the discard of n cards from the hand by trashing them"
  ([state side] (discard-from-hand state side (make-eid state) side 1))
  ([state side n] (discard-from-hand state side (make-eid state) side n))
  ([state from-side to-side n] (discard-from-hand state from-side (make-eid state) to-side n))
  ([state from-side eid to-side n]
   (let [cards (take n (shuffle (get-in @state [to-side :hand])))]
     (trash-cards state from-side eid cards {:unpreventable true}))))

(defn change-hand-size
  "Changes a side's hand-size modification by specified amount (positive or negative)"
  [state side n]
  (gain state side :hand-size {:mod n}))

;; Exposing
(defn expose-prevent
  [state side n]
  (swap! state update-in [:expose :expose-prevent] #(+ (or % 0) n)))

(defn- resolve-expose
  [state side eid target args]
  (system-msg state side (str "exposes " (card-str state target {:visible true})))
  (if-let [ability (:expose (card-def target))]
    (wait-for (resolve-ability state side ability target nil)
              (trigger-event-sync state side (make-result eid true) :expose target))
    (trigger-event-sync state side (make-result eid true) :expose target)))

(defn expose
  "Exposes the given card."
  ([state side target] (expose state side (make-eid state) target))
  ([state side eid target] (expose state side eid target nil))
  ([state side eid target {:keys [unpreventable] :as args}]
    (swap! state update-in [:expose] dissoc :expose-prevent)
    (if (rezzed? target)
      (effect-completed state side eid) ; cannot expose faceup cards
      (wait-for (trigger-event-sync state side :pre-expose target)
                (let [prevent (get-prevent-list state :corp :expose)]
                  (if (and (not unpreventable)
                           (cards-can-prevent? state :corp prevent :expose))
                    (do (system-msg state :corp "has the option to prevent a card from being exposed")
                        (show-wait-prompt state :runner "Corp to prevent the expose" {:priority 10})
                        (show-prompt state :corp nil
                                     (str "Prevent " (:title target) " from being exposed?") ["Done"]
                                     (fn [_]
                                       (clear-wait-prompt state :runner)
                                       (if-let [_ (get-in @state [:expose :expose-prevent])]
                                         (effect-completed state side (make-result eid false)) ;; ??
                                         (do (system-msg state :corp "will not prevent a card from being exposed")
                                             (resolve-expose state side eid target args))))
                                     {:priority 10}))
                    (if-not (get-in @state [:expose :expose-prevent])
                      (resolve-expose state side eid target args)
                      (effect-completed state side (make-result eid false)))))))))

(defn reveal-hand
  "Reveals a side's hand to opponent and spectators."
  [state side]
  (swap! state assoc-in [side :openhand] true))

(defn conceal-hand
  "Conceals a side's revealed hand from opponent and spectators."
  [state side]
  (swap! state update-in [side] dissoc :openhand))

(defn reveal
  "Trigger the event for revealing one or more cards."
  [state side & targets]
  (apply trigger-event-sync state side (make-eid state) (if (= :corp side) :corp-reveal :runner-reveal) (flatten targets)))

(defn clear-win
  "Clears the current win condition.  Requires both sides to have issued the command"
  [state side]
  (swap! state assoc-in [side :clear-win] true)
  (when (and (-> @state :runner :clear-win) (-> @state :corp :clear-win))
    (system-msg state side "cleared the win condition")
    (swap! state dissoc-in [:runner :clear-win])
    (swap! state dissoc-in [:corp :clear-win])
    (swap! state dissoc :winner :loser :winning-user :losing-user :reason :winning-deck-id :losing-deck-id :end-time)))

(defn win
  "Records a win reason for statistics."
  [state side reason]
  (when-not (:winner @state)
    (let [started (get-in @state [:stats :time :started])
          now (t/now)]
      (system-msg state side "wins the game")
      (play-sfx state side "game-end")
      (swap! state assoc-in [:stats :time :ended] now)
      (swap! state assoc-in [:stats :time :elapsed] (t/in-minutes (t/interval started now)))
      (swap! state assoc
             :winner side
             :loser (other-side side)
             :winning-user (get-in @state [side :user :username])
             :losing-user (get-in @state [(other-side side) :user :username])
             :reason reason :end-time (java.util.Date.)
             :winning-deck-id (get-in @state [side :deck-id])
             :losing-deck-id (get-in @state [(other-side side) :deck-id])))))

(defn win-decked
  "Records a win via decking the corp."
  [state]
  (system-msg state :corp "is decked")
  (win state :runner "Decked"))
