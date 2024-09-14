(ns game.main
  (:require [stringer.core :as s]
            [cheshire.generate :refer [add-encoder encode-str]]
            [game.core :as core]
            [game.core.toasts :refer [toast]]))

(add-encoder java.lang.Object encode-str)

(defn set-action-id
  "Creates a unique action id for each server response - used in client lock"
  [state side]
  (swap! state update-in [side :aid] (fnil inc 0)))

(defn handle-action
  "Ensures the user is allowed to do command they are trying to do"
  [state side command args]
  (when (core/process-action command state side args)
    (set-action-id state side)))

(defn handle-concede
  "Concedes victory from the given player."
  [state side]
  (when (and state side)
    (core/concede state side)))

(defn handle-say
  "Adds a message from a user to the chat log."
  [state side user message]
  (when (and state side)
    (core/command-parser state side {:user (select-keys user [:username :emailhash])
                                     :text message})))

(defn handle-notification
  ([state text]
   (when state
     (core/system-say state nil text)))
  ([state _ text] (handle-notification state text))
  ([state _ _ text] (handle-notification state text)))

(defn handle-announcement
  [state text]
  (when state
    (doseq [side [:runner :corp]]
      (toast state side text "warning" {:time-out 0 :close-button true}))))

(defn handle-rejoin
  [state {:keys [_id username] :as user}]
  (when-let [side (cond
                    (= _id (get-in @state [:corp :user :_id])) :corp
                    (= _id (get-in @state [:runner :user :_id])) :runner
                    :else nil)]
    (swap! state assoc-in [side :user] user)
    (handle-notification state (s/strcat username " rejoined the game."))))
