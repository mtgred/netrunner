(in-ns 'game.core)

(declare all-active card-flag-fn? clear-turn-register! clear-wait-prompt create-deck hand-size keep-hand mulligan
         show-wait-prompt turn-message)

(def game-states (atom {}))

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
                      (mulligan state side nil))))))

(defn init-game
  "Initializes a new game with the given players vector."
  [{:keys [players gameid spectatorhands room] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp) (:user corp))
        runner-deck (create-deck (:deck runner) (:user runner))
        corp-deck-id (get-in corp [:deck :_id])
        runner-deck-id (get-in runner [:deck :_id])
        corp-options (get-in corp [:options])
        runner-options (get-in runner [:options])
        corp-identity (assoc (or (get-in corp [:deck :identity]) {:side "Corp" :type "Identity"}) :cid (make-cid))
        corp-identity (assoc corp-identity :implementation (card-implemented corp-identity))
        runner-identity (assoc (or (get-in runner [:deck :identity]) {:side "Runner" :type "Identity"}) :cid (make-cid))
        runner-identity (assoc runner-identity :implementation (card-implemented runner-identity))
        state (atom
                {:gameid gameid :log [] :active-player :runner :end-turn true
                 :room room
                 :rid 0 :turn 0 :eid 0
                 :sfx [] :sfx-current-id 0
                 :options {:spectatorhands spectatorhands}
                 :corp {:user (:user corp) :identity corp-identity
                        :options corp-options
                        :deck (zone :deck corp-deck)
                        :deck-id corp-deck-id
                        :hand []
                        :discard [] :scored [] :rfg [] :play-area []
                        :servers {:hq {} :rd{} :archives {}}
                        :click 0 :credit 5 :bad-publicity 0 :has-bad-pub 0
                        :toast []
                        :hand-size-base 5 :hand-size-modification 0
                        :agenda-point 0
                        :click-per-turn 3 :agenda-point-req 7 :keep false}
                 :runner {:user (:user runner) :identity runner-identity
                          :options runner-options
                          :deck (zone :deck runner-deck)
                          :deck-id runner-deck-id
                          :hand []
                          :discard [] :scored [] :rfg [] :play-area []
                          :rig {:program [] :resource [] :hardware []}
                          :toast []
                          :click 0 :credit 5 :run-credit 0 :memory 4 :link 0 :tag 0
                          :hand-size-base 5 :hand-size-modification 0
                          :agenda-point 0
                          :hq-access 1 :rd-access 1 :tagged 0
                          :brain-damage 0 :click-per-turn 4 :agenda-point-req 7 :keep false}})]
    (init-identity state :corp corp-identity)
    (init-identity state :runner runner-identity)
    ;(swap! game-states assoc gameid state)
    (let [side :corp]
      (when-completed (trigger-event-sync state side :pre-start-game)
                      (let [side :runner]
                        (when-completed (trigger-event-sync state side :pre-start-game)
                                        (init-hands state)))))
    state))

(defn server-card
  ([title] (@all-cards title))
  ([title user]
   (@all-cards title)))

(defn make-card
  "Makes or remakes (with current cid) a proper card from an @all-cards card"
  ([card] (make-card card (make-cid)))
  ([card cid]
  (-> card
      (assoc :cid cid :implementation (card-implemented card))
      (dissoc :setname :text :_id :influence :number :influencelimit :factioncost))))

(defn reset-card
  "Resets a card back to its original state overlaid with any play-state data"
  ([state side card]
   (update! state side (merge card (make-card (get @all-cards (:title card)) (:cid card))))))

(defn create-deck
  "Creates a shuffled draw deck (R&D/Stack) from the given list of cards.
  Loads card data from server-side @all-cards map if available."
  ([deck] (create-deck deck nil))
  ([deck user]
   (shuffle (mapcat #(map (fn [card]
                            (let [server-card (or (server-card (:title card) user) card)
                                  c (assoc (make-card server-card) :art (:art card))]
                              (if-let [init (:init (card-def c))] (merge c init) c)))
                          (repeat (:qty %) (assoc (:card %) :art (:art %))))
                    (shuffle (vec (:cards deck)))))))

(defn make-rid
  "Returns a progressively-increasing integer to identify a new remote server."
  [state]
  (get-in (swap! state update-in [:rid] inc) [:rid]))

(defn make-eid
  [state]
  {:eid (:eid (swap! state update-in [:eid] inc))})

(defn make-result
  [eid result]
  (assoc eid :result result))

(defn mulligan
  "Mulligan starting hand."
  [state side args]
  (shuffle-into-deck state side :hand)
  (draw state side 5 {:suppress-event true})
  (let [card (get-in @state [side :identity])]
    (when-let [cdef (card-def card)]
      (when-let [mul (:mulligan cdef)]
        (mul state side (make-eid state) card nil))))
  (swap! state assoc-in [side :keep] true)
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
  (swap! state assoc-in [side :keep] true)
  (system-msg state side "keeps their hand")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :corp) (-> @state :runner :identity :title))
    (clear-wait-prompt state :runner)
    (show-wait-prompt state :corp "Runner to keep hand or mulligan"))
  (when (and (= side :runner)  (-> @state :corp :identity :title))
    (clear-wait-prompt state :corp)))

(defn end-phase-12
  "End phase 1.2 and trigger appropriate events for the player."
  [state side args]
  (turn-message state side true)
  (let [extra-clicks (or (get-in @state [side :extra-click-temp]) 0)]
    (gain state side :click (get-in @state [side :click-per-turn]))
    (when-completed (trigger-event-sync state side (if (= side :corp) :corp-turn-begins :runner-turn-begins))
                    (do (when (= side :corp)
                          (draw state side)
                          (trigger-event state side :corp-mandatory-draw))

                        (cond

                          (< extra-clicks 0)
                          (lose state side :click (abs extra-clicks))

                          (> extra-clicks 0)
                          (gain state side :click extra-clicks))

                        (swap! state dissoc-in [side :extra-click-temp])
                        (swap! state dissoc (if (= side :corp) :corp-phase-12 :runner-phase-12))
                        (when (= side :corp)
                          (update-all-advancement-costs state side))))))

(defn start-turn
  "Start turn."
  [state side args]
  (when (= side :corp)
    (swap! state update-in [:turn] inc))

  (doseq [c (filter #(:new %) (all-installed state side))]
    (update! state side (dissoc c :new)))

  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)

  (let [phase (if (= side :corp) :corp-phase-12 :runner-phase-12)
        start-cards (filter #(card-flag-fn? state side % phase true)
                            (all-active state side))]
    (swap! state assoc phase true)
    (trigger-event state side phase nil)
    (if (not-empty start-cards)
      (toast state side
                 (str "You may use " (clojure.string/join ", " (map :title start-cards))
                      (if (= side :corp)
                        " between the start of your turn and your mandatory draw."
                        " before taking your first click."))
                 "info")
      (end-phase-12 state side args))))

(defn end-turn
  ([state side args] (end-turn state side (make-eid state) args))
  ([state side eid args]
   (let [max-hand-size (max (hand-size state side) 0)]
     (when (<= (count (get-in @state [side :hand])) max-hand-size)
       (turn-message state side false)
       (when (and (= side :runner)
                  (neg? (hand-size state side)))
         (flatline state))
       (when-completed
         (trigger-event-sync state side (if (= side :runner) :runner-turn-ends :corp-turn-ends))
         (do (when (= side :runner)
               (trigger-event state side :post-runner-turn-ends))
             (doseq [a (get-in @state [side :register :end-turn])]
               (resolve-ability state side (:ability a) (:card a) (:targets a)))
             (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
             (let [rig-cards (apply concat (vals (get-in @state [:runner :rig])))
                   hosted-cards (filter :installed (mapcat :hosted rig-cards))
                   hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
               (doseq [card (concat rig-cards hosted-cards hosted-on-ice)]
                 ;; Clear the added-virus-counter flag for each virus in play.
                 ;; We do this even on the corp's turn to prevent shenanigans with something like Gorman Drip and Surge
                 (when (has-subtype? card "Virus")
                   (set-prop state :runner card :added-virus-counter false))))
             (swap! state assoc :end-turn true)
             (swap! state update-in [side :register] dissoc :cannot-draw)
             (swap! state update-in [side :register] dissoc :drawn-this-turn)
             (doseq [c (filter #(= :this-turn (:rezzed %)) (all-installed state :corp))]
               (update! state side (assoc c :rezzed true)))
             (clear-turn-register! state)
             (swap! state dissoc :turn-events)
             (when-let [extra-turns (get-in @state [side :extra-turns])]
               (when (> extra-turns 0)
                 (start-turn state side nil)
                 (swap! state update-in [side :extra-turns] dec)
                 (let [turns (if (= 1 extra-turns) "turn" "turns")]
                   (system-msg state side (clojure.string/join ["will have " extra-turns " extra " turns " remaining."])))))
             (effect-completed state side eid)))))))
