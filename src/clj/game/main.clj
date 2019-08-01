(ns game.main
  (:require [cheshire.core :refer [parse-string generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.core :refer [card-is-public?] :as core]
            [game.core.toasts :refer [toast]]
            [game.core.card :refer [private-card]]
            [differ.core :as differ]))

(add-encoder java.lang.Object encode-str)

(def spectator-commands
  {"typing" core/typing
   "typingstop" core/typingstop})

(def commands
  {"ability" core/play-ability
   "access" core/successful-run
   "advance" core/advance
   "change" core/change
   "choice" core/resolve-prompt
   "close-deck" core/close-deck
   "concede" core/concede
   "continue" core/continue
   "corp-ability" core/play-corp-ability
   "corp-phase-43" core/corp-phase-43
   "credit" core/click-credit
   "derez" #(core/derez %1 %2 (:card %3))
   "draw" core/click-draw
   "dynamic-ability" core/play-dynamic-ability
   "end-phase-12" core/end-phase-12
   "end-turn" core/end-turn
   "jack-out" core/jack-out
   "keep" core/keep-hand
   "move" core/move-card
   "mulligan" core/mulligan
   "no-action" core/no-action
   "play" core/play
   "purge" core/do-purge
   "remove-tag" core/remove-tag
   "rez" #(core/rez %1 %2 (:card %3) nil)
   "run" core/click-run
   "runner-ability" core/play-runner-ability
   "score" #(core/score %1 %2 (game.core/get-card %1 (:card %3)))
   "select" core/select
   "shuffle" core/shuffle-deck
   "start-turn" core/start-turn
   "subroutine" core/play-subroutine
   "system-msg" #(core/system-msg %1 %2 (:msg %3))
   "toast" toast
   "trash-resource" core/trash-resource
   "unbroken-subroutines" core/play-unbroken-subroutines
   "view-deck" core/view-deck})

(defn strip [state]
  (-> state
    (dissoc :events :turn-events :per-turn :prevent :damage :effect-completed :click-state :turn-state)
    (update-in [:corp :register] dissoc :most-recent-drawn)
    (update-in [:runner :register] dissoc :most-recent-drawn)))

(defn not-spectator?
  "Returns true if the specified user in the specified state is not a spectator"
  [state user]
  (let [corp-id (get-in @state [:corp :user :_id])
        runner-id (get-in @state [:runner :user :_id])]
    (and state ((set [corp-id runner-id]) (:_id user)))))

(defn- private-card-vector [state side cards]
  (vec (map (fn [card]
              (cond
                (not (card-is-public? state side card)) (private-card card)
                (:hosted card) (update-in card [:hosted] #(private-card-vector state side %))
                :else card))
            cards)))

(defn- make-private-runner [state]
  (-> (:runner @state)
      (update-in [:hand] #(private-card-vector state :runner %))
      (update-in [:discard] #(private-card-vector state :runner %))
      (update-in [:deck] #(private-card-vector state :runner %))
      (update-in [:rig :facedown] #(private-card-vector state :runner %))
      (update-in [:rig :resource] #(private-card-vector state :runner %))))

(defn- make-private-corp [state]
  (let [zones (concat [[:hand]] [[:discard]] [[:deck]]
                      (for [server (keys (:servers (:corp @state)))] [:servers server :ices])
                      (for [server (keys (:servers (:corp @state)))] [:servers server :content]))]
    (loop [s (:corp @state)
           z zones]
      (if (empty? z)
        s
        (recur (update-in s (first z) #(private-card-vector state :corp %)) (next z))))))

(defn- make-private-deck [state side deck]
  (if (:view-deck (side @state))
    deck
    (private-card-vector state side deck)))

(defn- private-states
  "Generates privatized states for the Corp, Runner and any spectators from the base state.
  If `:spectatorhands` is on, all information is passed on to spectators as well."
  [state]
  ;; corp, runner, spectator
  (let [corp-private (make-private-corp state)
        runner-private (make-private-runner state)
        corp-deck (update-in (:corp @state) [:deck] #(make-private-deck state :corp %))
        runner-deck (update-in (:runner @state) [:deck] #(make-private-deck state :runner %))]
    [(assoc @state :runner runner-private
                   :corp corp-deck)
     (assoc @state :corp corp-private
                   :runner runner-deck)
     (if (get-in @state [:options :spectatorhands])
       (assoc @state :corp corp-deck :runner runner-deck)
       (assoc @state :corp corp-private :runner runner-private))]))

(defn public-states [state]
  (let [[new-corp new-runner new-spect] (private-states state)]
    {:runner-state (strip new-runner)
     :corp-state   (strip new-corp)
     :spect-state  (strip new-spect)}))

(defn public-diffs [old-state new-state]
  (let [[old-corp old-runner old-spect] (when old-state (private-states (atom old-state)))
        [new-corp new-runner new-spect] (private-states new-state)

        runner-diff (differ/diff (strip old-runner) (strip new-runner))
        corp-diff (differ/diff (strip old-corp) (strip new-corp))
        spect-diff (differ/diff (strip old-spect) (strip new-spect))]
    {:runner-diff runner-diff
     :corp-diff   corp-diff
     :spect-diff  spect-diff}))

(defn set-action-id
  "Creates a unique action id for each server response - used in client lock"
  [state side]
  (swap! state update-in [side :aid] (fnil inc 0)))

(defn handle-action
  "Ensures the user is allowed to do command they are trying to do"
  [user command state side args]
  (if (not-spectator? state user)
    (when-let [c (get commands command)]
      (c state side args)
      (set-action-id state side))
    (when-let [cmd (spectator-commands command)]
      (cmd state side args))))

(defn handle-concede
  "Concedes victory from the given player."
  [state side]
  (when (and state side)
    (core/concede state side)))

(defn handle-say
  "Adds a message from a user to the chat log."
  [state side user message]
  (when (and state side)
    (core/say state side {:user (select-keys user [:username :emailhash]) :text message})))

(defn handle-notification
  [state text]
  (when state
    (swap! state update-in [:log] #(conj % {:user "__system__" :text text}))))

(defn handle-announcement
  [state text]
  (when state
    (doseq [side [:runner :corp]]
      (toast state side text "warning" {:time-out 0 :close-button true}))))

(defn handle-typing
  [state side user typing]
  (when (and state side)
    (if typing
      (core/typing state side {:user user})
      (core/typingstop state side {:user user}))))

(defn handle-rejoin
  [state {:keys [_id username] :as user}]
  (when-let [side (cond
                    (= _id (get-in @state [:corp :user :_id])) :corp
                    (= _id (get-in @state [:runner :user :_id])) :runner
                    :else nil)]
    (swap! state assoc-in [side :user] user)
    (handle-notification state (str username " rejoined the game."))))
