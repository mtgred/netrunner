(ns game.main
  (:require [cheshire.core :refer [parse-string generate-string]]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.core :refer [card-is-public?] :as core]
            [game.core.eid :as eid]
            [game.core.toasts :refer [toast]]
            [game.core.card :refer [private-card get-card]]
            [game.utils :refer [dissoc-in]]))

(add-encoder java.lang.Object encode-str)

(def spectator-commands
  {"typing" core/typing
   "typingstop" core/typingstop})

(def commands
  {"ability" core/play-ability
   "advance" core/click-advance
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
   "start-next-phase" core/start-next-phase
   "end-turn" core/end-turn
   "generate-install-list" core/generate-install-list
   "generate-runnable-zones" core/generate-runnable-zones
   "indicate-action" core/indicate-action
   "jack-out" core/jack-out
   "keep" core/keep-hand
   "move" core/move-card
   "mulligan" core/mulligan
   "play" core/play
   "purge" core/do-purge
   "remove-tag" core/remove-tag
   "rez" #(core/rez %1 %2 (:card %3) (dissoc %3 :card))
   "run" core/click-run
   "runner-ability" core/play-runner-ability
   "score" #(core/score %1 %2 (get-card %1 (:card %3)))
   "select" core/select
   "shuffle" core/shuffle-deck
   "start-turn" core/start-turn
   "subroutine" core/play-subroutine
   "system-msg" #(core/system-msg %1 %2 (:msg %3))
   "toast" toast
   "toggle-auto-no-action" core/toggle-auto-no-action
   "trash" #(core/trash %1 %2 (eid/make-eid %1) (get-card %1 (:card %3)) nil)
   "trash-resource" core/trash-resource
   "unbroken-subroutines" core/play-unbroken-subroutines
   "view-deck" core/view-deck})

(defn not-spectator?
  "Returns true if the specified user in the specified state is not a spectator"
  [state user]
  (let [corp-id (get-in @state [:corp :user :_id])
        runner-id (get-in @state [:runner :user :_id])]
    (and state ((set [corp-id runner-id]) (:_id user)))))

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
      (core/update-all-card-labels state)
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
