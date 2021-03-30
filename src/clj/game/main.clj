(ns game.main
  (:require [cheshire.generate :refer [add-encoder encode-str]]
            [clojure.string :as str]
            [game.core :as core]))

(add-encoder java.lang.Object encode-str)

(def spectator-commands
  {"typing" core/typing
   "typingstop" core/typingstop})

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
    (when (core/process-action command state side args)
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
    (let [author (or (select-keys user [:username :emailhash])
                     (get-in @state [side :user]))
          text (if (= (str/trim message) "null") " null" message)]
      (or (core/parse-and-perform-command state side author text)
          (core/say state side {:user user
                                :text message})))))

(defn handle-notification
  [state text]
  (when state
    (swap! state update :log conj {:user "__system__" :text text})))

(defn handle-announcement
  [state text]
  (when state
    (doseq [side [:runner :corp]]
      (core/toast state side text "warning" {:time-out 0 :close-button true}))))

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
