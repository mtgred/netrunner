(in-ns 'game.core)

(declare card-flag-fn? clear-turn-register! clear-wait-prompt create-deck hand-size keep-hand mulligan show-wait-prompt
         turn-message)

;;; Functions for the creation of games and the progression of turns.
(defn init-game
  "Initializes a new game with the given players vector."
  [{:keys [players gameid] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-identity (assoc (or (get-in corp [:deck :identity]) {:side "Corp" :type "Identity"}) :cid (make-cid))
        runner-identity (assoc (or (get-in runner [:deck :identity]) {:side "Runner" :type "Identity"}) :cid (make-cid))
        state (atom
                {:gameid gameid :log [] :active-player :runner :end-turn true
                 :rid 0 :turn 0
                 :corp {:user (:user corp) :identity corp-identity
                        :deck (zone :deck (drop 5 corp-deck))
                        :hand (zone :hand (take 5 corp-deck))
                        :discard [] :scored [] :rfg [] :play-area []
                        :servers {:hq {} :rd{} :archives {}}
                        :click 0 :credit 5 :bad-publicity 0
                        :hand-size-base 5 :hand-size-modification 0
                        :agenda-point 0
                        :click-per-turn 3 :agenda-point-req 7 :keep false}
                 :runner {:user (:user runner) :identity runner-identity
                          :deck (zone :deck (drop 5 runner-deck))
                          :hand (zone :hand (take 5 runner-deck))
                          :discard [] :scored [] :rfg [] :play-area []
                          :rig {:program [] :resource [] :hardware []}
                          :click 0 :credit 5 :run-credit 0 :memory 4 :link 0 :tag 0
                          :hand-size-base 5 :hand-size-modification 0
                          :agenda-point 0
                          :hq-access 1 :rd-access 1 :tagged 0
                          :brain-damage 0 :click-per-turn 4 :agenda-point-req 7 :keep false}})]
    (card-init state :corp corp-identity)
    (card-init state :runner runner-identity)
    (swap! game-states assoc gameid state)
    (when (and (-> @state :corp :identity :title) (-> @state :runner :identity :title))
      (show-wait-prompt state :runner "Corp to keep hand or mulligan"))
    (doseq [s [:corp :runner]]
      (when (-> @state s :identity :title)
        (show-prompt state s nil "Keep hand?" ["Keep" "Mulligan"]
                     #(if (= % "Keep") (keep-hand state s nil) (mulligan state s nil)))))
    @game-states))

(defn create-deck
  "Creates a shuffled draw deck (R&D/Stack) from the given list of cards."
  [deck]
  (shuffle (mapcat #(map (fn [card]
                           (let [c (assoc card :cid (make-cid))
                                 c (dissoc c :setname :text :_id :influence :number :influencelimit
                                           :factioncost)]
                             (if-let [init (:init (card-def c))] (merge c init) c)))
                         (repeat (:qty %) (:card %)))
                   (:cards deck))))


(defn make-rid
  "Returns a progressively-increasing integer to identify a new remote server."
  [state]
  (get-in (swap! state update-in [:rid] inc) [:rid]))

;; Appears to be unused???
(def reset-value
  {:corp {:credit 5 :bad-publicity 0
          :hand-size-base 5 :hand-size-modification 0}
   :runner {:credit 5 :run-credit 0 :link 0 :memory 4
            :hand-size-base 5 :hand-size-modification 0}})

(defn mulligan
  "Mulligan starting hand."
  [state side args]
  (shuffle-into-deck state side :hand)
  (draw state side 5)
  (let [card (get-in @state [side :identity])]
    (when-let [cdef (card-def card)]
      (when-let [mul (:mulligan cdef)]
        (mul state side card nil))))
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
  (gain state side :click (get-in @state [side :click-per-turn]))
  (trigger-event state side (if (= side :corp) :corp-turn-begins :runner-turn-begins))
  (when (= side :corp)
    (draw state side))
  (swap! state dissoc (if (= side :corp) :corp-phase-12 :runner-phase-12))
  (when (= side :corp)
    (update-all-advancement-costs state side)))

(defn start-turn
  "Start turn."
  [state side args]
  (when (= side :corp)
    (swap! state update-in [:turn] inc))

  (swap! state assoc :active-player side :per-turn nil :end-turn false)
  (swap! state assoc-in [side :register] nil)

  (let [phase (if (= side :corp) :corp-phase-12 :runner-phase-12)
        start-cards (filter #(card-flag-fn? state side % phase true)
                            (concat (cons (get-in @state [side :identity])
                                          (all-installed state side))
                                    (when (= side :corp) (get-in @state [side :scored]))))]
    (swap! state assoc phase true)
    (if (not-empty start-cards)
      (toast state side
                 (str "You may use " (clojure.string/join ", " (map :title start-cards))
                      (if (= side :corp)
                        " between the start of your turn and your mandatory draw."
                        " before taking your first click."))
                 "info")
      (end-phase-12 state side args))))

(defn end-turn [state side args]
  (let [max-hand-size (max (hand-size state side) 0)]
    (when (<= (count (get-in @state [side :hand])) max-hand-size)
      (turn-message state side false)
      (if (= side :runner)
        (do (when (neg? (hand-size state side))
              (flatline state))
            (trigger-event state side :runner-turn-ends))
        (trigger-event state side :corp-turn-ends))
      (doseq [a (get-in @state [side :register :end-turn])]
        (resolve-ability state side (:ability a) (:card a) (:targets a)))
      (let [rig-cards (apply concat (vals (get-in @state [:runner :rig])))
            hosted-cards (filter :installed (mapcat :hosted rig-cards))
            hosted-on-ice (->> (get-in @state [:corp :servers]) seq flatten (mapcat :ices) (mapcat :hosted))]
        (doseq [card (concat rig-cards hosted-cards hosted-on-ice)]
          ;; Clear the added-virus-counter flag for each virus in play.
          ;; We do this even on the corp's turn to prevent shenanigans with something like Gorman Drip and Surge
          (when (has-subtype? card "Virus")
            (set-prop state :runner card :added-virus-counter false))))
      (swap! state assoc :end-turn true)
      (clear-turn-register! state)
      (swap! state dissoc :turn-events))))
