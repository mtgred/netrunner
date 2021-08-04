(ns game.core.set-up
  (:require
    [clj-time.core :as t]
    [game.core.card :refer [corp? runner?]]
    [game.core.card-defs :refer [card-def]]
    [game.core.checkpoint :refer [fake-checkpoint]]
    [game.core.drawing :refer [draw]]
    [game.core.eid :refer [make-eid register-effect-completed]]
    [game.core.engine :refer [trigger-event trigger-event-sync]]
    [game.core.initializing :refer [card-init make-card]]
    [game.core.pipeline :refer [continue-gp! queue-step! queue-steps!]]
    [game.core.player :refer [new-corp new-runner]]
    [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
    [game.core.say :refer [system-msg]]
    [game.core.shuffling :refer [shuffle-into-deck]]
    [game.core.state :refer [new-state]]
    [game.core.steps.step :refer [complete! ->SimpleStep]]
    [game.core.turns :refer [begin-turn]]
    [game.quotes :as quotes]
    [game.utils :refer [server-card]]))

(defn build-card
  [card]
  (let [s-card (or (server-card (:title card)) card)]
    (assoc (make-card s-card) :art (:art card))))

(defn create-deck
  "Creates a shuffled draw deck (R&D/Stack) from the given list of cards.
  Loads card data from the server-card map if available."
  [deck]
  (shuffle (mapcat #(map build-card (repeat (:qty %) (assoc (:card %) :art (:art %))))
                   (shuffle (vec (:cards deck))))))

;;; Functions for the creation of games and the progression of turns.
(defn mulligan
  "Mulligan starting hand."
  [state side _]
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
  [state side _]
  (swap! state assoc-in [side :keep] :keep)
  (system-msg state side "keeps their hand")
  (trigger-event state side :pre-first-turn)
  (when (and (= side :corp) (-> @state :runner :identity :title))
    (clear-wait-prompt state :runner)
    (show-wait-prompt state :corp "Runner to keep hand or mulligan"))
  (when (and (= side :runner)  (-> @state :corp :identity :title))
    (clear-wait-prompt state :corp)))

(defn- init-hands [state step]
  (doseq [side [:corp :runner]]
    (draw state side 5 {:suppress-event true}))
  (doseq [side [:corp :runner]]
    (when (-> @state side :identity :title)
      (show-prompt state side nil "Keep hand?"
                   ["Keep" "Mulligan"]
                   #(do (if (= (:value %) "Keep")
                          (keep-hand state side nil)
                          (mulligan state side nil))
                        (complete! step))
                   {:prompt-type :mulligan})))
  (when (and (-> @state :corp :identity :title)
             (-> @state :runner :identity :title))
    (show-wait-prompt state :runner "Corp to keep hand or mulligan")))

(defn- init-game-state
  "Initialises the game state"
  [{:keys [players gameid timer spectatorhands api-access save-replay room]}]
  (let [corp (some #(when (corp? %) %) players)
        runner (some #(when (runner? %) %) players)
        corp-deck (create-deck (:deck corp))
        runner-deck (create-deck (:deck runner))
        corp-deck-id (get-in corp [:deck :_id])
        runner-deck-id (get-in runner [:deck :_id])
        corp-options (get-in corp [:options])
        runner-options (get-in runner [:options])
        corp-identity (build-card (or (get-in corp [:deck :identity])
                                      {:side "Corp" :type "Identity" :title "Custom Biotics: Engineered for Success"}))
        runner-identity (build-card (or (get-in runner [:deck :identity])
                                        {:side "Runner" :type "Identity" :title "The Professor: Keeper of Knowledge"}))
        corp-quote (quotes/make-quote corp-identity runner-identity)
        runner-quote (quotes/make-quote runner-identity corp-identity)]
    (atom
      (new-state
        gameid
        room
        (t/now)
        {:timer timer
         :spectatorhands spectatorhands
         :api-access api-access
         :save-replay save-replay}
        (new-corp (:user corp) corp-identity corp-options (map #(assoc % :zone [:deck]) corp-deck) corp-deck-id corp-quote)
        (new-runner (:user runner) runner-identity runner-options (map #(assoc % :zone [:deck]) runner-deck) runner-deck-id runner-quote)))))

(defn- create-basic-action-cards
  [state]
  (swap! state assoc-in [:corp :basic-action-card] (make-card {:side "Corp" :type "Basic Action" :title "Corp Basic Action Card"}))
  (swap! state assoc-in [:runner :basic-action-card] (make-card {:side "Runner" :type "Basic Action" :title "Runner Basic Action Card"})))

(defn set-up-phase
  "Initializes a new game with the given players vector."
  [state]
  (->SimpleStep
    (fn [set-up-step]
      (queue-step! state (->SimpleStep (fn [_] (fake-checkpoint state))))
      (doseq [side [:corp :runner]]
        (queue-step!
          state
          (->SimpleStep
            (fn init-game-fn [step]
              (let [eid (make-eid state)]
                (register-effect-completed
                  state eid (fn [_] (complete! step)))
                (trigger-event-sync state side eid :pre-start-game nil)
                false)))))
      (queue-steps!
        state
        [(->SimpleStep (fn [step] (init-hands state step) false))
         (->SimpleStep (fn [_] (fake-checkpoint state)))
         (->SimpleStep (fn [_] (complete! set-up-step)))]))))

(defn init-game
  [game]
  (let [state (init-game-state game)
        corp-identity (get-in @state [:corp :identity])
        runner-identity (get-in @state [:runner :identity])]
    (when-let [messages (seq (:messages game))]
      (swap! state assoc :log (conj (vec messages) {:user "__system__" :text "[hr]"})))
    (card-init state :corp corp-identity)
    (card-init state :runner runner-identity)
    (create-basic-action-cards state)
    (queue-step! state (set-up-phase state))
    (queue-step! state (->SimpleStep (fn [_] (begin-turn state))))
    (continue-gp! state)
    state))
