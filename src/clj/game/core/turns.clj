(in-ns 'game.core)

(declare all-active card-flag-fn? clear-turn-register! create-deck hand-size keep-hand mulligan
         make-card turn-message add-sub)

(defn- card-implemented
  "Checks if the card is implemented. Looks for a valid return from `card-def`.
  If implemented also looks for `:implementation` key which may contain special notes.
  Returns either:
    nil - not implemented
    :full - implemented fully
    msg - string with implementation notes"
  [card]
  (when-let [cdef (card-def card)]
    ;; Card is defined - hence implemented
    (if-let [impl (:implementation cdef)]
      (if (:recurring cdef) (str impl ". Recurring credits usage not restricted") impl)
      (if (:recurring cdef) "Recurring credits usage not restricted" :full))))

;;; Functions for the creation of games and the progression of turns.
(defn init-identity
  "Initialise the identity"
  [state side identity]
  (card-init state side identity)
  (when-let [baselink (:baselink identity)]
    (gain state side :link baselink)))

(defn- init-hands [state]
  (draw state :corp 5 {:suppress-event true})
  (draw state :runner 5 {:suppress-event true})
  (when (and (-> @state :corp :identity :title)
             (-> @state :runner :identity :title))
    (show-wait-prompt state :runner "Corp to keep hand or mulligan"))
  (doseq [side [:corp :runner]]
    (when (-> @state side :identity :title)
      (show-prompt state side nil "Keep hand?"
                   ["Keep" "Mulligan"]
                   #(if (= % "Keep")
                      (keep-hand state side nil)
                      (mulligan state side nil))
                   {:prompt-type :mulligan}))))

(defn- init-game-state
  "Initialises the game state"
  [{:keys [players gameid spectatorhands room] :as game}]
  (let [corp (some #(when (corp? %) %) players)
        runner (some #(when (runner? %) %) players)
        corp-deck (create-deck (:deck corp) (:user corp))
        runner-deck (create-deck (:deck runner) (:user runner))
        corp-deck-id (get-in corp [:deck :_id])
        runner-deck-id (get-in runner [:deck :_id])
        corp-options (get-in corp [:options])
        runner-options (get-in runner [:options])
        corp-identity (make-card (or (get-in corp [:deck :identity])
                                 {:side "Corp" :type "Identity" :title "Custom Biotics: Engineered for Success"}))
        runner-identity (make-card (or (get-in runner [:deck :identity])
                                   {:side "Runner" :type "Identity" :title "The Professor: Keeper of Knowledge"}))
        corp-quote (quotes/make-quote corp-identity runner-identity)
        runner-quote (quotes/make-quote runner-identity corp-identity)]
    (atom
      (new-state
        gameid
        room
        (t/now)
        spectatorhands
        (new-corp (:user corp) corp-identity corp-options (zone :deck corp-deck) corp-deck-id corp-quote)
        (new-runner (:user runner) runner-identity runner-options (zone :deck runner-deck) runner-deck-id runner-quote)))))

(defn init-game
  "Initializes a new game with the given players vector."
  [game]
  (let [state (init-game-state game)
        corp-identity (get-in @state [:corp :identity])
        runner-identity (get-in @state [:runner :identity])]
    (init-identity state :corp corp-identity)
    (init-identity state :runner runner-identity)
    (let [side :corp]
      (wait-for (trigger-event-sync state side :pre-start-game nil)
                (let [side :runner]
                  (wait-for (trigger-event-sync state side :pre-start-game nil)
                            (init-hands state)))))
    state))

(defn- subroutines-init
  "Initialised the subroutines associated with the card, these work as abilities"
  [card cdef]
  (->> (:subroutines cdef)
       (reduce (fn [ice sub] (add-sub ice sub (:cid ice) {:printed true})) card)
       :subroutines))

(defn make-card
  "Makes or remakes (with current cid) a proper card from a server card"
  ([card] (make-card card (make-cid)))
  ([card cid]
   (-> card
       (assoc :cid cid
              :implementation (card-implemented card)
              :subroutines (subroutines-init (assoc card :cid cid) (card-def card)))
       (dissoc :setname :text :_id :influence :number :influencelimit :factioncost)
       (map->Card))))

(defn build-card
  [card]
  (let [server-card (or (server-card (:title card)) card)
        c (assoc (make-card server-card) :art (:art card))]
    (if-let [init (:init (card-def c))]
      (merge c init)
      c)))

(defn create-deck
  "Creates a shuffled draw deck (R&D/Stack) from the given list of cards.
  Loads card data from the server-card map if available."
  ([deck] (create-deck deck nil))
  ([deck user]
   (shuffle (mapcat #(map build-card (repeat (:qty %) (assoc (:card %) :art (:art %))))
                    (shuffle (vec (:cards deck)))))))

(defn make-rid
  "Returns a progressively-increasing integer to identify a new remote server."
  [state]
  (get-in (swap! state update-in [:rid] inc) [:rid]))

(defn mulligan
  "Mulligan starting hand."
  [state side args]
  (shuffle-into-deck state side :hand)
  (draw state side 5 {:suppress-event true})
  (let [card (get-in @state [side :identity])]
    (when-let [cdef (card-def card)]
      (when-let [mul (:mulligan cdef)]
        (mul state side (make-eid state) card nil))))
  (swap! state assoc-in [side :keep] :mulligan)
  (system-msg state side "takes a mulligan")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :corp) (-> @state :runner :identity :title))
    (clear-wait-prompt state :runner)
    (show-wait-prompt state :corp "Runner to keep hand or mulligan"))
  (when (and (= side :runner)  (-> @state :corp :identity :title))
    (clear-wait-prompt state :corp)))

(defn keep-hand
  "Choose not to mulligan."
  [state side args]
  (swap! state assoc-in [side :keep] :keep)
  (system-msg state side "keeps their hand")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :corp) (-> @state :runner :identity :title))
    (clear-wait-prompt state :runner)
    (show-wait-prompt state :corp "Runner to keep hand or mulligan"))
  (when (and (= side :runner)  (-> @state :corp :identity :title))
    (clear-wait-prompt state :corp)))

(defn end-phase-12
  "End phase 1.2 and trigger appropriate events for the player."
  ([state side args] (end-phase-12 state side (make-eid state) args))
  ([state side eid args]
   (turn-message state side true)
   (wait-for (trigger-event-simult state side (if (= side :corp) :corp-turn-begins :runner-turn-begins) nil nil)
             (when (= side :corp)
               (wait-for (draw state side 1 nil)
                         (trigger-event-simult state side eid :corp-mandatory-draw nil nil)))
             (swap! state dissoc (if (= side :corp) :corp-phase-12 :runner-phase-12))
             (when (= side :corp)
               (update-all-advancement-costs state side)))))

(defn start-turn
  "Start turn."
  [state side args]

  ; Functions to set up state for undo-turn functionality
  (doseq [s [:runner :corp]] (swap! state dissoc-in [s :undo-turn]))
  (swap! state assoc :turn-state (dissoc @state :log))

  (when (= side :corp)
    (swap! state update-in [:turn] inc))

  (doseq [c (filter :new (all-installed state side))]
    (update! state side (dissoc c :new)))

  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)

  (let [phase (if (= side :corp) :corp-phase-12 :runner-phase-12)
        start-cards (filter #(card-flag-fn? state side % phase true)
                            (all-active state side))
        extra-clicks (get-in @state [side :extra-click-temp] 0)]
    (gain state side :click (get-in @state [side :click-per-turn]))
    (cond
      (neg? extra-clicks) (lose state side :click (abs extra-clicks))
      (pos? extra-clicks) (gain state side :click extra-clicks))
    (swap! state dissoc-in [side :extra-click-temp])
    (swap! state assoc phase true)
    (trigger-event state side phase nil)
    (if (not-empty start-cards)
      (toast state side
             (str "You may use " (string/join ", " (map :title start-cards))
                  (if (= side :corp)
                    " between the start of your turn and your mandatory draw."
                    " before taking your first click."))
             "info")
      (end-phase-12 state side args))))

(defn handle-end-of-turn-discard
  ([state side _card _targets] (handle-end-of-turn-discard state side (make-eid state) _card _targets))
  ([state side eid _card _targets]
   (let [cur-hand-size (count (get-in @state [side :hand]))
         max-hand-size (max (hand-size state side) 0)]
     (if (> cur-hand-size max-hand-size)
       (continue-ability
         state side
         {:prompt (str "Discard down to " (quantify max-hand-size "card"))
          :choices {:req in-hand?
                    :max (- cur-hand-size max-hand-size)
                    :all true}
          :effect (req (system-msg state side
                                   (str "discards "
                                        (if (= :runner side)
                                          (join ", " (map :title targets))
                                          (quantify (count targets) "card"))
                                        " from " (if (= :runner side) "their Grip" "HQ")
                                        " at end of turn"))
                       (doseq [t targets]
                         (trash state side t {:unpreventable true}))
                       (effect-completed state side eid))}
         nil nil)
       (effect-completed state side eid)))))

(defn end-turn
  ([state side args] (end-turn state side (make-eid state) args))
  ([state side eid args]
   (wait-for
     (handle-end-of-turn-discard state side nil nil)
     (turn-message state side false)
     (when (and (= side :runner)
                (neg? (hand-size state side)))
       (flatline state))
     (wait-for (trigger-event-sync state side (if (= side :runner) :runner-turn-ends :corp-turn-ends) nil)
               (trigger-event state side (if (= side :runner) :post-runner-turn-ends :post-corp-turn-ends))
               (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
               (doseq [card (all-active-installed state :runner)]
                 ;; Clear :installed :this-turn as turn has ended
                 (when (= :this-turn (installed? card))
                   (update! state side (assoc card :installed true)))
                 ;; Clear the added-virus-counter flag for each virus in play.
                 ;; We do this even on the corp's turn to prevent shenanigans with something like Gorman Drip and Surge
                 (when (has-subtype? card "Virus")
                   (set-prop state :runner card :added-virus-counter false))
                 ;; Remove all-turn strength from icebreakers.
                 ;; We do this even on the corp's turn in case the breaker is boosted due to Offer You Can't Refuse
                 (when (has-subtype? card "Icebreaker")
                   (update! state side (update-in (get-card state card) [:pump] dissoc :all-turn))
                   (update-breaker-strength state :runner card)))
               (doseq [card (all-installed state :corp)]
                 ;; Clear :this-turn flags as turn has ended
                 (when (= :this-turn (installed? card))
                   (update! state side (assoc card :installed true)))
                 (when (= :this-turn (:rezzed card))
                   (update! state side (assoc card :rezzed true))))
               ;; Update strength of all ice every turn
               (update-all-ice state side)
               (swap! state assoc :end-turn true)
               (swap! state update-in [side :register] dissoc :cannot-draw)
               (swap! state update-in [side :register] dissoc :drawn-this-turn)
               (clear-turn-register! state)
               (swap! state assoc :turn-events nil)
               (when-let [extra-turns (get-in @state [side :extra-turns])]
                 (when (pos? extra-turns)
                   (start-turn state side nil)
                   (swap! state update-in [side :extra-turns] dec)
                   (system-msg state side (string/join ["will have " (quantify extra-turns "extra turn") " remaining."]))))
               (effect-completed state side eid)))))
