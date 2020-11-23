(ns game.core.moving
  (:require
    [game.core.agendas :refer [update-all-agenda-points]]
    [game.core.board :refer [all-active-installed]]
    [game.core.card :refer [card-index corp? facedown? fake-identity? get-card get-zone
                            ice? in-play-area? installed? resource? rezzed? runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [register-constant-effects unregister-constant-effects]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid make-result]]
    [game.core.engine :refer [checkpoint dissoc-req make-pending-event queue-event register-events resolve-ability should-trigger? trigger-event trigger-event-sync unregister-events]]
    [game.core.finding :refer [find-cid get-scoring-owner]]
    [game.core.flags :refer [can-trash? card-flag? cards-can-prevent? get-prevent-list untrashable-while-resources? untrashable-while-rezzed?]]
    [game.core.hosting :refer [remove-from-host]]
    [game.core.ice :refer [get-current-ice set-current-ice]]
    [game.core.initializing :refer [card-init deactivate reset-card]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [enforce-msg system-msg system-say]]
    [game.core.servers :refer [is-remote?]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [check-winner]]
    [game.macros :refer [wait-for]]
    [game.utils :refer [dissoc-in make-cid remove-once same-card? same-side? to-keyword]]
    [jinteki.utils :refer [other-side]]
    [clojure.string :as string]))

;; Helpers for move
(defn- remove-old-card
  "Removes the old pre-move card from the game state, for use in move"
  [state side {:keys [zone host] :as card}]
  (doseq [s [:runner :corp]]
    (if host
      (remove-from-host state side card)
      (swap! state update-in (cons s (vec zone)) (fn [coll] (remove-once #(same-card? card %) coll))))))

(defn uninstall
  "Triggers :uninstall effects"
  ([state side {:keys [disabled] :as card}]
  (when-let [uninstall-effect (:uninstall (card-def card))]
    (when (not disabled)
      (uninstall-effect state side (make-eid state) card nil)))
  card))

(declare trash)

(defn- get-moved-card
  "Get the moved cards with correct abilities and keys hooked up / removed etc."
  [state side {:keys [zone host installed] :as card} to]
  (let [zone (if host (map to-keyword (:zone host)) zone)
        src-zone (first zone)
        target-zone (if (vector? to) (first to) to)
        same-zone? (= src-zone target-zone)
        dest (if (sequential? to) (vec to) [to])
        to-facedown (= dest [:rig :facedown])
        to-installed (#{:servers :rig} (first dest))
        from-installed (#{:servers :rig} src-zone)
        trash-hosted (fn [h]
                       (trash state side
                              (make-eid state)
                              (update-in h [:zone] #(map to-keyword %))
                              {:unpreventable true
                               :host-trashed true
                               :game-trash true})
                       ())
        update-hosted (fn [h]
                        (let [newz (flatten (list dest))
                              newh (-> h
                                       (assoc-in [:zone] '(:onhost))
                                       (assoc-in [:host :zone] newz))]
                          (update! state side newh)
                          (unregister-events state side h)
                          (register-events state side newh)
                          (unregister-constant-effects state side h)
                          (register-constant-effects state side newh)
                          newh))
        hosted (seq (flatten (map (if same-zone? update-hosted trash-hosted) (:hosted card))))
        ;; Set :seen correctly
        c (if (= :corp side)
            (cond
              ;; Moving rezzed card to discard, explicitly mark as seen
              (and (= :discard (first dest))
                   (rezzed? card))
              (assoc card :seen true)
              ;; Moving card to HQ or R&D, explicitly mark as not seen
              (#{:hand :deck} (first dest))
              (dissoc card :seen)
              ;; Else return card
              :else
              card)
            card)
        c (if (and (not (and (= (get-scoring-owner state card) :runner)
                             (#{:scored} src-zone)
                             (#{:hand :deck :discard :rfg} target-zone)))
                   (or installed
                       host
                       (#{:servers :scored :current :play-area} src-zone))
                   (or (#{:hand :deck :discard :rfg} target-zone)
                       to-facedown)
                   (not (facedown? c)))
            (deactivate state side c to-facedown)
            c)
        c (if (and from-installed
                   (not (facedown? c)))
            (uninstall state side c)
            c)
        c (if to-installed
            (assoc c :installed :this-turn)
            (dissoc c :installed))
        c (if to-facedown
            (assoc c :facedown true)
            (dissoc c :facedown))
        c (if (= :scored (first dest))
            (assoc c :scored-side side)
            c)
        cid (if (and (not (contains? #{:deck :hand :discard} src-zone))
                     (contains? #{:deck :hand :discard} target-zone))
              (make-cid)
              (:cid c))
        moved-card (assoc c :zone dest
                            :host nil
                            :hosted hosted
                            :cid cid
                            :previous-zone (:zone c))
        ;; Set up abilities for stolen agendas
        moved-card (if (and (= :scored (first dest))
                            (card-flag? moved-card :has-abilities-when-stolen true))
                     (merge moved-card {:abilities (:abilities (card-def moved-card))})
                     moved-card)]
    moved-card))

(defn update-installed-card-indices
  [state side server]
  (when (seq (get-in @state (cons side server)))
    (swap! state update-in (cons side server)
           #(into [] (map-indexed (fn [idx card] (assoc card :index idx)) %)))))

(defn move
  "Moves the given card to the given new zone."
  ([state side card to] (move state side card to nil))
  ([state side {:keys [zone host cid] :as card} to {:keys [front index keep-server-alive force suppress-event]}]
   (let [zone (if host (map to-keyword (:zone host)) zone)
         src-zone (first zone)
         target-zone (if (vector? to) (first to) to)]
     (if (fake-identity? card)
       ;; Make Fake-Identity cards "disappear"
       (do (deactivate state side card false)
           (remove-old-card state side card))
       (when (and card
                  (or host
                      (some #(same-card? card %) (get-in @state (cons :runner (vec zone))))
                      (some #(same-card? card %) (get-in @state (cons :corp (vec zone)))))
                  (or force
                      (empty? (get-in @state [(to-keyword (:side card)) :locked (-> card :zone first)]))))
         (when-not suppress-event
           (trigger-event state side :pre-card-moved card src-zone target-zone))
         (let [dest (if (sequential? to) (vec to) [to])
               moved-card (get-moved-card state side card to)]
           (if (= cid (:cid moved-card))
             ;; Moving the card hasn't changed the cid
             (let [new-effects (reduce
                                 (fn [all-effects current-effect]
                                   (if (= cid (:cid (:card current-effect)))
                                     (conj all-effects (assoc current-effect :card moved-card))
                                     (conj all-effects current-effect)))
                                 []
                                 (:effects @state))]
               (swap! state assoc :effects (into [] new-effects)))
             ;; Moving the card has changed the cid
             (swap! state assoc :effects
                    (->> (:effects @state)
                         (remove #(same-card? card (:card %)))
                         (into []))))
           (remove-old-card state side card)
           (let [pos-to-move-to (cond index index
                                      front 0
                                      :else (count (get-in @state (cons side dest))))]
             (swap! state update-in (cons side dest) #(into [] (concat (take pos-to-move-to %) [moved-card] (drop pos-to-move-to %)))))
           (when (seq zone)
             (update-installed-card-indices state side zone))
           (update-installed-card-indices state side dest)
           (let [z (vec (cons :corp (butlast zone)))]
             (when (and (not keep-server-alive)
                        (is-remote? z)
                        (empty? (get-in @state (conj z :content)))
                        (empty? (get-in @state (conj z :ices))))
               (swap! state dissoc-in z)))
           (when-let [move-zone-fn (:move-zone (card-def moved-card))]
             (move-zone-fn state side (make-eid state) moved-card card))
           (when-not suppress-event
             (trigger-event state side :card-moved card (assoc moved-card :move-to-side side)))
           ; This is for removing `:location :X` events that are non-default locations,
           ; such as Subliminal Messaging only registering in :discard. We first unregister
           ; any non-default events from the previous zone and the register the non-default
           ; events for the current zone.
           ; NOTE: I (NoahTheDuke) experimented with using this as the basis for all event
           ; registration and handling, but there are too many edge-cases in the engine
           ; right now. Maybe at some later date it'll work, but currently (Oct '19),
           ; there are more important things to focus on.
           (let [zone #{(first (:previous-zone moved-card))}
                 old-events (filter #(zone (:location %)) (:events (card-def moved-card)))]
             (when (seq old-events)
               (unregister-events state side moved-card {:events (into [] old-events)})))
           (let [zone #{(first (:zone moved-card))}
                 events (filter #(zone (:location %)) (:events (card-def moved-card)))]
             (when (seq events)
               (register-events state side moved-card events)))
           ;; Default a card when moved to inactive zones (except :persistent key)
           (when (some #{:discard :hand :deck :rfg} dest)
             (reset-card state side moved-card))
           (get-card state moved-card)))))))

(defn move-zone
  "Moves all cards from one zone to another, as in Chronos Project."
  [state side server to]
  (when-not (seq (get-in @state [side :locked server]))
    (doseq [card (get-in @state [side server])]
      (move state side card to))))

;;; Trashing
(defn trash-resource-bonus
  "Applies a cost increase of n to trashing a resource with the click action. (SYNC.)"
  [state _ n]
  (swap! state update-in [:corp :trash-cost-bonus] (fnil #(+ % n) 0)))

(defn trash-prevent
  [state _ type n]
  (swap! state update-in [:trash :trash-prevent type] (fnil #(+ % n) 0)))

(defn- prevent-trash-impl
  [state side eid {:keys [zone type] :as card} oid {:keys [unpreventable cause game-trash] :as args}]
  (if (and card (not-any? #{:discard} zone))
    (cond
      (and (not game-trash)
           (untrashable-while-rezzed? card))
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
                (show-wait-prompt state :corp "Runner to prevent trash effects")
                (show-prompt state :runner nil
                             (str "Prevent the trashing of " (:title card) "?") ["Done"]
                             (fn [_]
                               (clear-wait-prompt state :corp)
                               (if-let [_ (get-in @state [:trash :trash-prevent ktype])]
                                 (do (system-msg state :runner (str "prevents the trashing of " (:title card)))
                                     (swap! state update-in [:trash :trash-prevent] dissoc ktype)
                                     (effect-completed state side eid))
                                 (do (system-msg state :runner (str "will not prevent the trashing of " (:title card)))
                                     (complete-with-result state side eid card))))))
            ;; No prevention effects: add the card to the trash-list
            (complete-with-result state side eid card)))))
    (effect-completed state side eid)))

(defn update-current-ice-to-trash
  "If the current ice is going to be trashed, update it with any changes"
  [state trashlist]
  (let [current-ice (get-current-ice state)
        current-ice-to-trash (first (filter #(same-card? current-ice %) trashlist))]
    (when current-ice-to-trash
      (set-current-ice state (get-card state current-ice-to-trash)))))

(defn- get-card?
  [state c]
  (when-let [card (get-card state c)]
    (assoc card :seen (:seen c))))

(defn- prevent-trash
  ([state side eid cs args] (prevent-trash state side eid cs args []))
  ([state side eid cs args acc]
   (if (seq cs)
     (wait-for (prevent-trash-impl state side (make-eid state eid) (get-card? state (first cs)) eid args)
               (if-let [card async-result]
                 (prevent-trash state side eid (rest cs) args (conj acc card))
                 (prevent-trash state side eid (rest cs) args acc)))
     (complete-with-result state side eid acc))))

(defn trash-cards
  "Attempts to trash each given card, and then once all given cards have been either
  added or not added to the trash list, all of those cards are trashed"
  ([state side eid cards] (trash-cards state side eid cards nil))
  ([state side eid cards {:keys [cause keep-server-alive host-trashed game-trash accessed] :as args}]
   (wait-for (prevent-trash state side (make-eid state eid) cards args)
             (let [trashlist async-result
                   _ (update-current-ice-to-trash state trashlist)
                   ;; Criteria for abilities that trigger when the card is trashed
                   get-trash-effect (fn [card]
                                      (let [trash-effect (:on-trash (card-def card))]
                                        (when (and card
                                                   (not (:disabled card))
                                                   (or (and (runner? card)
                                                            (installed? card)
                                                            (not (facedown? card)))
                                                       (and (rezzed? card)
                                                            (not host-trashed))
                                                       (and (:when-inactive trash-effect)
                                                            (not host-trashed))
                                                       (in-play-area? card))
                                                   (should-trigger? state side eid card
                                                                    [{:card card
                                                                      :cause cause
                                                                      :accessed accessed}]
                                                                    trash-effect))
                                          (let [once-per (:once-per-instance trash-effect)]
                                            (-> trash-effect
                                                (assoc :once-per-instance (if (some? once-per) once-per true))
                                                dissoc-req)))))
                   ;; No card should end up in the opponent's discard pile, so instead
                   ;; of using `side`, we use the card's `:side`.
                   move-card (fn [card]
                               (move state (to-keyword (:side card)) card :discard {:keep-server-alive keep-server-alive}))
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
                                 trashlist)]
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
                 (doseq [[card trash-effect] moved-cards
                         :when trash-effect]
                   (make-pending-event state trash-event card trash-effect))
                 (doseq [trashed-card trashlist]
                   (queue-event state trash-event {:card trashed-card
                                                   :cause cause
                                                   :accessed accessed}))
                 (checkpoint state nil eid nil))))))

(defn trash
  [state side eid card args] (trash-cards state side eid [card] args))

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

(defn remove-old-current
  "Trashes or RFG the existing current when a new current is played, or an agenda is stolen / scored"
  [state side eid current-side]
  (if-let [current (first (get-in @state [current-side :current]))]
    (do (trigger-event state side :trash-current current)
        (unregister-constant-effects state side current)
        (let [current (get-card state current)]
          (if (get-in current [:special :rfg-when-trashed])
            (do (system-say state side (str (:title current) " is removed from the game."))
                (move state (other-side side) current :rfg)
                (effect-completed state side eid))
            (do (system-say state side (str (:title current) " is trashed."))
                (trash state (to-keyword (:side current)) eid current nil)))))
    (effect-completed state side eid)))

(defn swap-installed
  "Swaps two installed corp cards"
  [state side a b]
  (let [pred? (every-pred corp? installed?)]
    (when (and (pred? a)
               (pred? b))
      (let [a-index (card-index state a)
            b-index (card-index state b)
            a-new (assoc a :zone (:zone b))
            b-new (assoc b :zone (:zone a))]
        (swap! state update-in (cons :corp (:zone a)) assoc a-index b-new)
        (swap! state update-in (cons :corp (:zone b)) assoc b-index a-new)
        (update-installed-card-indices state :corp (:zone a))
        (update-installed-card-indices state :corp (:zone b))
        (doseq [new-card [a-new b-new]]
          (unregister-events state side new-card)
          (when (rezzed? new-card)
            (register-events state side new-card))
          (doseq [h (:hosted new-card)]
            (let [newh (-> h
                           (assoc-in [:zone] '(:onhost))
                           (assoc-in [:host :zone] (:zone new-card)))]
              (update! state side newh)
              (unregister-events state side h)
              (register-events state side newh))))
        (trigger-event state side :swap a-new b-new)))))

(defn swap-ice
  "Swaps two pieces of ICE."
  [state side a b]
  (let [pred? (every-pred corp? installed? ice?)]
    (when (and (pred? a)
               (pred? b))
      (swap-installed state side a b)
      (set-current-ice state))))

(defn swap-cards
  "Swaps two cards when one or both aren't installed"
  [state side a b]
  (when (same-side? (:side a) (:side b))
    (let [moved-a (move state side a (get-zone b)
                        {:keep-server-alive true
                         :index (card-index state b)
                         :suppress-event true})
          moved-b (move state side b (get-zone a)
                        {:keep-server-alive true
                         :index (card-index state a)
                         :suppress-event true})]
      (trigger-event state side :swap moved-a moved-b)
      (when (and (:run @state)
                 (or (ice? a)
                     (ice? b)))
        (set-current-ice state))
      [(get-card state moved-a) (get-card state moved-b)])))

(defn swap-agendas
  "Swaps the two specified agendas, first one scored (on corp side), second one stolen (on runner side)"
  [state side scored stolen]
  ;; Update location information
  (let [scored (assoc scored :scored-side :runner)
        stolen (assoc stolen :scored-side :corp)]
    ;; Move agendas
    (swap! state update-in [:corp :scored]
           (fn [coll] (conj (remove-once #(same-card? % scored) coll) stolen)))
    (swap! state update-in [:runner :scored]
           (fn [coll] (conj (remove-once #(same-card? % stolen) coll)
                            (if-not (card-flag? scored :has-abilities-when-stolen true)
                              (dissoc scored :abilities :events) scored))))
    ;; Set up abilities and events for new scored agenda
    (let [new-scored (find-cid (:cid stolen) (get-in @state [:corp :scored]))
          abilities (:abilities (card-def new-scored))
          new-scored (merge new-scored {:abilities abilities})]
      (update! state :corp new-scored)
      (unregister-events state side new-scored)
      (register-events state side new-scored)
      (unregister-constant-effects state side new-scored)
      (register-constant-effects state side new-scored)
      (resolve-ability state side (:swapped (card-def new-scored)) new-scored new-scored))
    ;; Overload :scored for interrupts like Project Vacheron I guess
    (let [new-stolen (find-cid (:cid scored) (get-in @state [:runner :scored]))]
      (resolve-ability state side (:stolen (card-def new-stolen)) new-stolen [new-stolen]))
    ;; Set up abilities and events for new stolen agenda
    (when-not (card-flag? scored :has-events-when-stolen true)
      (let [new-stolen (find-cid (:cid scored) (get-in @state [:runner :scored]))]
        (deactivate state :corp new-stolen)))
    ;; Update agenda points
    (update-all-agenda-points state side)
    (check-winner state side)))

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
            (make-eid state)
            (update-in h [:zone] #(map to-keyword %))
            {:unpreventable true :suppress-event true}))
   (let [card (get-card state card)]
     (when (:msg args)
       (system-msg state side (str "forfeits " (:title card))))
     (move state (to-keyword (:side card)) card :rfg)
     (update-all-agenda-points state side)
     (check-winner state side)
     (trigger-event-sync state side eid (if (= :corp side) :corp-forfeit-agenda :runner-forfeit-agenda) card))))
