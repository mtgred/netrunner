(in-ns 'game.core)

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

(declare get-current-ice set-current-ice)

(defn update-current-ice-to-trash
  "If the current ice is going to be trashed, update it with any changes"
  [state trashlist]
  (let [current-ice (get-current-ice state)
        current-ice-to-trash (first (filter #(same-card? current-ice %) trashlist))]
    (when current-ice-to-trash
      (set-current-ice state (get-card state current-ice-to-trash)))))

(defn trash-cards
  "Attempts to trash each given card, and then once all given cards have been either
  added or not added to the trash list, all of those cards are trashed"
  ; ([state side cards] (trash-cards state side (make-eid state) cards nil))
  ([state side eid cards] (trash-cards state side eid cards nil))
  ([state side eid cards {:keys [cause keep-server-alive host-trashed game-trash] :as args}]
   (let [num-cards (< 1 (count cards))]
     (letfn [(get-card? [s c]
               (when-let [card (get-card s c)]
                 (assoc card :seen (:seen c))))
             (preventrec [cs]
               (if (seq cs)
                 (wait-for (prevent-trash state side (get-card? state (first cs)) eid args)
                           (preventrec (rest cs)))
                 (let [trashlist (get-in @state [:trash :trash-list eid])
                       _ (update-current-ice-to-trash state trashlist)
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
                       ;; If the trashed card is installed, update all of the indicies
                       ;; of the other installed cards in the same location
                       update-indicies (fn [card]
                                         (when (installed? card)
                                           (update-installed-card-indices state side (:zone card))))
                       ;; Perform the move of the cards from their current location to
                       ;; the discard. At the same time, gather their `:trash-effect`s
                       ;; to be used in the simult event later.
                       moved-cards (reduce
                                     (fn [acc card]
                                       (if-let [card (get-card? state card)]
                                         (let [moved-card (move-card card)
                                               trash-effect (get-trash-effect card)]
                                           (update-indicies card)
                                           (conj acc [moved-card trash-effect]))
                                         acc))
                                     []
                                     trashlist)
                       card-abilities (mapv #(apply ability-as-handler %) moved-cards)]
                   (swap! state update-in [:trash :trash-list] dissoc eid)
                   (when (seq (remove #{side} (map #(to-keyword (:side %)) trashlist)))
                     (swap! state assoc-in [side :register :trashed-card] true))
                   ;; Pseudo-shuffle archives. Keeps seen cards in play order and shuffles unseen cards.
                   (swap! state assoc-in [:corp :discard]
                     (vec (sort-by #(if (:seen %) -1 (rand-int 30)) (get-in @state [:corp :discard]))))
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
                         targets (concat trashlist (list {:cause cause}))
                         eid (make-result eid (mapv first moved-cards))]
                     (apply trigger-event-simult state side eid trash-event {:card-abilities card-abilities} targets)))))]
       (preventrec cards)))))

(defn trash
  ; ([state side card] (trash-cards state side (make-eid state) [card] nil))
  ; ([state side card args] (trash-cards state side (make-eid state) [card] args))
  ; ([state side eid card] (trash-cards state side eid [card] nil))
  ([state side eid card args] (trash-cards state side eid [card] args)))
