(ns game.core.moving
  (:require
    [clojure.string :as string]
    [game.core.agendas :refer [update-all-agenda-points]]
    [game.core.board :refer [all-active-installed]]
    [game.core.card :refer [active? card-index condition-counter? convert-to-agenda corp? facedown? fake-identity? get-card get-title get-zone has-subtype? ice? in-hand? in-play-area? installed? program? resource? rezzed? runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.effects :refer [register-static-abilities unregister-static-abilities]]
    [game.core.eid :refer [complete-with-result effect-completed make-eid make-result]]
    [game.core.engine :as engine :refer [checkpoint dissoc-req register-pending-event queue-event register-default-events register-events should-trigger? trigger-event trigger-event-sync unregister-events]]
    [game.core.finding :refer [get-scoring-owner]]
    [game.core.flags :refer [can-trash? card-flag? cards-can-prevent? get-prevent-list untrashable-while-resources? untrashable-while-rezzed? zone-locked?]]
    [game.core.hosting :refer [remove-from-host]]
    [game.core.ice :refer [get-current-ice set-current-ice update-breaker-strength]]
    [game.core.initializing :refer [card-init deactivate reset-card]]
    [game.core.memory :refer [init-mu-cost]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [enforce-msg system-msg]]
    [game.core.servers :refer [is-remote? target-server type->rig-zone]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [check-win-by-agenda]]
    [game.macros :refer [wait-for when-let*]]
    [game.utils :refer [dissoc-in make-cid remove-once same-card? same-side? to-keyword]]
    [medley.core :refer [insert-nth]]))

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
                       (engine/move* state side
                                    (make-eid state)
                                    :trash
                                    (update h :zone #(map to-keyword %))
                                    {:unpreventable true
                                     :host-trashed true
                                     :game-trash true})
                       nil)
        update-hosted (fn [h]
                        (let [newz (flatten (list dest))
                              newh (-> h
                                       (assoc :zone [:onhost])
                                       (assoc-in [:host :zone] newz))]
                          (update! state side newh)
                          (when (active? newh)
                            (unregister-events state side h)
                            (register-default-events state side newh)
                            (unregister-static-abilities state side h)
                            (register-static-abilities state side newh)
                            (when (program? newh)
                              (init-mu-cost state newh)))
                          [newh]))
        hosted (seq (mapcat (if same-zone? update-hosted trash-hosted) (:hosted card)))
        ;; Set :seen correctly
        c (if (= :corp side)
            (cond
              ;; Moving rezzed card or condition counter to discard, explicitly mark as seen
              (and (= :discard (first dest))
                   (or (rezzed? card)
                       (condition-counter? card)))
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

(defn- update-effects
  "If a card moves within a zone (ice changes positions, card moves between servers),
  then we should update it in the relevant :effects maps. We determine this by comparing
  the :cid. Otherwise, remove the events with `:while-active` duration."
  [state {:keys [cid] :as card} moved-card]
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
                (remove #(and (same-card? card (:card %))
                              (= :while-active (:duration %))))
                (into [])))))

(defn update-installed-card-indices
  [state side server]
  (when (seq (get-in @state (cons side server)))
    (swap! state update-in (cons side server)
           #(into [] (map-indexed (fn [idx card] (assoc card :index idx)) %)))))

(defn- update-run-position
  "If there is an active run, update the Runner's position if any ice was moved to or from an inward position."
  [state old-card moved-card]
  (when-let* [run (:run @state)
              position (:position run)
              _ (pos? position)]
    (letfn [(protecting-run-server? [card]
              (and (ice? card)
                   (= (target-server run) (second (:zone card)))
                   (= :ices (last (:zone card)))))
            (inward-position? [card]
              (< (:index card) position))]
      (cond
        (and (protecting-run-server? old-card)
             (inward-position? old-card))
        (swap! state update-in [:run :position] dec)
        (and (protecting-run-server? moved-card)
             (inward-position? moved-card))
        (swap! state update-in [:run :position] inc)))))

(defn move
  "Moves the given card to the given new zone."
  ([state side card to] (move state side card to nil))
  ([state side {:keys [zone host] :as card} to {:keys [front index keep-server-alive force suppress-event swap]}]
   (let [zone (if host (map to-keyword (:zone host)) zone)]
     (if (fake-identity? card)
       ;; Make Fake-Identity cards "disappear"
       (do (deactivate state side card false)
           (remove-old-card state side card))
       (when (and card
                  (or host
                      (some #(same-card? card %) (get-in @state (cons :runner (vec zone))))
                      (some #(same-card? card %) (get-in @state (cons :corp (vec zone)))))
                  (or force
                      (not (zone-locked? state (to-keyword (:side card)) (first (get-zone card))))))
         (let [dest (if (sequential? to) (vec to) [to])
               moved-card (get-moved-card state side card to)]
           (update-effects state card moved-card)
           (remove-old-card state side card)
           (let [pos-to-move-to (cond index index
                                      front 0
                                      :else (count (get-in @state (cons side dest))))]
             (swap! state update-in (cons side dest) #(vec (insert-nth pos-to-move-to moved-card %))))
           (when (seq zone)
             (update-installed-card-indices state side zone))
           (update-installed-card-indices state side dest)
           (when-not swap
             (update-run-position state card (get-card state moved-card)))
           (let [z (vec (cons :corp (butlast zone)))]
             (when (and (not keep-server-alive)
                        (is-remote? z)
                        (empty? (get-in @state (conj z :content)))
                        (empty? (get-in @state (conj z :ices))))
               (swap! state dissoc-in z)))
           (when-let [move-zone-fn (:move-zone (card-def moved-card))]
             (move-zone-fn state side (make-eid state) moved-card card))
           (when-not suppress-event
             (trigger-event state side :card-moved {:card card
                                                    :moved-card (get-card state moved-card)}))
           ;; move-zone-fn and the event can both modify the card, so re-bind here
           (let [moved-card (get-card state moved-card)]
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
             (get-card state moved-card))))))))

(defmethod engine/move* :move [state side _eid _action card args]
  (move state side card (:to args) args))

(defn move-zone
  "Moves all cards from one zone to another, as in Chronos Project."
  [state side server to]
  (when-not (zone-locked? state side server)
    (doseq [card (get-in @state [side server])]
      (move state side card to))))

;;; Trashing
(defn trash-prevent
  [state _ type n]
  (swap! state update-in [:trash :trash-prevent type] (fnil #(+ % n) 0)))

(defn- prevent-trash-impl
  [state side eid {:keys [zone type] :as card} {:keys [unpreventable cause game-trash] :as args}]
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
     (wait-for (prevent-trash-impl state side (make-eid state eid) (get-card? state (first cs)) args)
               (if-let [card async-result]
                 (prevent-trash state side eid (rest cs) args (conj acc card))
                 (prevent-trash state side eid (rest cs) args acc)))
     (complete-with-result state side eid acc))))

(defn get-trash-effect
  "Criteria for abilities that trigger when the card is trashed."
  [state side eid card {:keys [accessed cause cause-card host-trashed]}]
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
                                  :cause-card cause-card
                                  :accessed accessed}]
                                trash-effect))
      (-> trash-effect
          (assoc :once-per-instance true
                 :condition :inactive)
          (dissoc-req)))))

(defn set-duration-on-trash-events
  "CR 1.8 9.1.8g: If an active card moves to a zone where it is inactive, an ability of
  that card with a trigger condition that is met by this zone change remains active in
  the card’s new location until any corresponding instances of the ability resolve.

  This means that we must update all of the relevant `:X-trash` conditional abilities to
  last until the checkpoint, as calling `move` on a card will unregister all of its
  `:while-active` conditional abilities."
  [state card trash-event]
  (swap! state assoc :events
         (reduce
           (fn [acc cur]
             (let [event (if (and (same-card? card (:card cur))
                                  (= trash-event (:event cur)))
                           (assoc cur :duration trash-event)
                           cur)]
               (conj acc event)))
           []
           (:events @state))))

(defn get-trash-event
  "The trash event will be determined by who is performing the trash.

  `:game-trash` in this case refers to when a checkpoint sees a card has been trashed
  and it has hosted cards, so it trashes each hosted card. (CR 1.8 10.3.1g) This doesn't
  count as either player trashing the card, but the cards are counted as trashed by the
  engine and so abilities that don't care who performed the trash (Simulchip for
  example) still need it either logged or watchable."
  [side game-trash]
  (cond
    game-trash :game-trash
    (= side :corp) :corp-trash
    (= side :runner) :runner-trash))

(defn trash-cards
  "Attempts to trash each given card, and then once all given cards have been either
  added or not added to the trash list, all of those cards are trashed"
  ([state side eid cards] (trash-cards state side eid cards nil))
  ([state side eid cards {:keys [accessed cause cause-card keep-server-alive game-trash suppress-checkpoint] :as args}]
   (if (empty? (filter identity cards))
     (effect-completed state side eid)
     (wait-for (prevent-trash state side (make-eid state eid) cards args)
               (let [trashlist async-result
                     _ (update-current-ice-to-trash state trashlist)
                     trash-event (get-trash-event side game-trash)
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
                                       (let [_ (set-duration-on-trash-events state card trash-event)
                                             moved-card (move-card card)
                                             trash-effect (get-trash-effect state side eid card args)]
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
                        (vec (sort-by #(if (:seen %) -1 1) (get-in @state [:corp :discard]))))
                 (let [eid (make-result eid (mapv first moved-cards))]
                   (doseq [[card trash-effect] moved-cards
                           :when trash-effect]
                     (register-pending-event state trash-event card trash-effect))
                   (doseq [trashed-card trashlist]
                     (queue-event state trash-event {:card trashed-card
                                                     :cause cause
                                                     :cause-card cause-card
                                                     :accessed accessed}))
                   (if suppress-checkpoint
                     (effect-completed state nil eid)
                     (checkpoint state nil eid {:duration trash-event}))))))))

(defmethod engine/move* :trash-cards [state side eid _action cards args]
  (trash-cards state side eid cards args))

(defn trash
  ([state side eid card] (trash-cards state side eid [card] nil))
  ([state side eid card args] (trash-cards state side eid [card] args)))

(defmethod engine/move* :trash [state side eid _action card args]
  (trash-cards state side eid [card] args))

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
          (unregister-static-abilities state side new-card)
          (if (rezzed? new-card)
            (do (register-default-events state side new-card)
                (register-static-abilities state side new-card))
            (when-let [dre (:derezzed-events (card-def new-card))]
              (register-events state side new-card (map #(assoc % :condition :derezzed) dre))))
          (doseq [h (:hosted new-card)]
            (let [newh (-> h
                           (assoc-in [:zone] '(:onhost))
                           (assoc-in [:host :zone] (:zone new-card)))]
              (update! state side newh)
              (unregister-events state side h)
              (register-default-events state side newh)
              (unregister-static-abilities state side h)
              (register-static-abilities state side newh)
              (when (program? newh)
                (init-mu-cost state newh)))))
        (trigger-event state side :swap {:swap-type :installed
                                         :card1 a-new
                                         :card2 b-new})))))

(defn swap-ice
  "Swaps 2 pieces of ice."
  [state side a b]
  (let [pred? (every-pred corp? installed? ice?)]
    (when (and (pred? a)
               (pred? b))
      (swap-installed state side a b)
      (set-current-ice state))))

(defn remove-from-currently-drawing
  [state side card]
  (swap! state update-in [side :register :currently-drawing]
         (fn [mrd] (conj (pop mrd) (remove-once #(= (:cid %) (:cid card)) (peek mrd))))))

(defn add-to-currently-drawing
  [state side card]
  (swap! state update-in [side :register :currently-drawing] #(conj (pop %) (conj (peek %) card))))

(defn swap-cards
  "Swaps two cards when one or both aren't installed"
  [state side a b]
  (when (same-side? (:side a) (:side b))
    (let [a (get-card state a)
          b (get-card state b)
          a-side (to-keyword (:side a))
          b-side (to-keyword (:side b))
          moved-a (move state a-side a (get-zone b)
                        {:keep-server-alive true
                         :index (card-index state b)
                         :suppress-event true
                         :swap true})
          moved-b (move state b-side b (get-zone a)
                        {:keep-server-alive true
                         :index (card-index state a)
                         :suppress-event true
                         :swap true})
          ;; under the new nsg rules, swapping a card into play triggers an install event
          ;; TODO - quote exactly where
          install-event (or (and (installed? a) (not (installed? b)))
                            (and (installed? b) (not (installed? a))))]
      (trigger-event state side :swap {:swap-type :not-installed
                                       :card1 moved-a
                                       :card2 moved-b})
      (doseq [moved [moved-a moved-b]]
        (when (installed? moved)
          (when-let [dre (:derezzed-events (card-def moved))]
            (register-events state side moved (map #(assoc % :condition :derezzed) dre)))))
      (when (and (:run @state)
                 (or (ice? a)
                     (ice? b)))
        (set-current-ice state))
      (when (-> @state side :register :currently-drawing (peek))
        (when (in-hand? a) (remove-from-currently-drawing state a-side a))
        (when (in-hand? b) (remove-from-currently-drawing state b-side b))
        (when (in-hand? moved-a) (add-to-currently-drawing state a-side moved-a))
        (when (in-hand? moved-b) (add-to-currently-drawing state b-side moved-b)))
      [(get-card state moved-a) (get-card state moved-b)])))

(defn swap-cards-async
  "Swaps two cards when one or both aren't installed"
  [state side eid a b]
  (let [async-result (swap-cards state side a b)
        moved-a (first async-result)
        moved-b (second async-result)
        install-event (= 1 (count (filter installed? [moved-a moved-b])))]
    ;; todo - we might need behaviour for runner swap installs down the line, depending on future cards
    ;; that's a problem for another day
    (if (and install-event (= :corp side))
      (let [installed-card (if (installed? moved-a) moved-a moved-b)
            cdef (card-def installed-card)]
        (queue-event state :corp-install {:card (get-card state installed-card)
                                          :install-state (:install-state cdef)})
        (wait-for (checkpoint state nil (make-eid state eid))
                  (complete-with-result state side eid async-result)))
      (complete-with-result state side eid async-result))))

(defn swap-agendas
  "Swaps the two specified agendas, first one scored (on corp side), second one stolen (on runner side).
  Returns the first agenda now in runner score area and second agenda now in corp score area."
  [state side scored stolen]
  (let [new-stolen (move state :runner scored :scored)
        new-scored (move state :corp stolen :scored)]
    (unregister-events state side stolen)
    (unregister-static-abilities state side stolen)
    (register-default-events state side new-scored)
    (register-static-abilities state side new-scored)
    (when-not (card-flag? scored :has-events-when-stolen true)
      (deactivate state :corp new-stolen))
    (trigger-event state side :swap {:swap-type :agendas
                                     :card1 new-stolen :card2 new-scored})
    (update-all-agenda-points state side)
    (check-win-by-agenda state side)
    [(get-card state new-stolen) (get-card state new-scored)]))

(defn as-agenda
  "Adds the given card to the given side's :scored area as an agenda worth n points."
  [state side card n]
  (let [card (deactivate state side card)
        card (convert-to-agenda card n)]
    (move state side card :scored {:force true})
    (update-all-agenda-points state side)
    (check-win-by-agenda state side)))

(defn forfeit
  "Forfeits the given agenda to the :rfg zone."
  ([state side eid card] (forfeit state side eid card {:msg true}))
  ([state side eid card {:keys [msg suppress-checkpoint]}]
   (wait-for (trash-cards state side (make-eid state eid) (:hosted card) {:game-trash true
                                                                          :suppress-checkpoint true
                                                                          :unpreventable true})
             (let [card (get-card state card)]
               (when msg
                 (system-msg state side (str "forfeits " (get-title card))))
               (move state (to-keyword (:side card)) card :rfg)
               (update-all-agenda-points state side)
               (check-win-by-agenda state side)
               (queue-event state (if (= :corp side) :corp-forfeit-agenda :runner-forfeit-agenda) {:card card})
               (if suppress-checkpoint
                 (complete-with-result state side eid card)
                 (checkpoint state nil (make-result eid card) {:duration :game-trash}))))))

(defn flip-facedown
  "Flips a runner card facedown, either manually (if it's hosted) or by calling move to facedown"
  [state side {:keys [host] :as card}]
  (if host
    (let [card (deactivate state side card true)
          card (assoc-in card [:facedown] true)]
      (update! state side card))
    (move state side card [:rig :facedown])))

(defn flip-faceup
  "Flips a runner card facedown, either manually (if it's hosted) or by calling move to correct area.
  Wires events without calling effect/init-data"
  [state side {:keys [host] :as card}]
  (let [card (if host
               (dissoc card :facedown)
               (move state side card (type->rig-zone (:type card))))]
   (card-init state side card {:resolve-effect false :init-data false})
   (when (has-subtype? card "Icebreaker")
     (update-breaker-strength state side card))))
