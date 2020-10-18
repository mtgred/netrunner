(ns game.core.say
  (:require [clojure.string :as string]
            [game.core.toasts :refer [toast]]))

(defn say
  "Prints a message to the log as coming from the given username. The special user string
  __system__ shows no user name."
  [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))
        text (if (= (string/trim text) "null") " null" text)]
    (swap! state update :log conj {:user author :text text})
    (swap! state assoc :typing (remove #{(:username author)} (:typing @state)))))

(defn typing
  "Updates game state list with username of whoever is typing"
  [state side {:keys [user]}]
  (let [author (:username (or user (get-in @state [side :user])))]
    (swap! state assoc :typing (distinct (conj (:typing @state) author)))
    ;; say something to force update in client side rendering
    (say state side {:user "__system__" :text "typing"})))

(defn typingstop
  "Clears typing flag from game state for user"
  [state side {:keys [user]}]
  (let [author (or user (get-in @state [side :user]))]
    (swap! state assoc :typing (remove #{(:username author)} (:typing @state)))
    ;; say something to force update in client side rendering
    (say state side {:user "__system__" :text "typing"})))

(defn system-say
  "Prints a system message to log (`say` from user __system__)"
  ([state side text] (system-say state side text nil))
  ([state side text {:keys [hr]}]
   (say state side {:user "__system__" :text (str text (when hr "[hr]"))})))

(defn system-msg
  "Prints a message to the log without a username."
  ([state side text] (system-msg state side text nil))
  ([state side text args]
   (let [username (get-in @state [side :user :username])]
     (system-say state side (str username " " text ".") args))))

(defn enforce-msg
  "Prints a message related to a rules enforcement on a given card.
  Example: 'Architect cannot be trashed while installed.'"
  [state card text]
  (say state nil {:user (get-in card [:title]) :text (str (:title card) " " text ".")}))

(defn indicate-action
  [state side _]
  (system-say state side
              (str "[!] Please pause, " (if (= side :corp) "Corp" "Runner") " is acting."))
  (toast state side
         "You have indicated action to your opponent"
         "info"
         {:time-out 2000 :close-button false})
  (toast state (if (= side :corp) :runner :corp)
         "Pause please, opponent is acting"
         "info"
         {:time-out 5000 :close-button true}))

(defn play-sfx
  "Adds a sound effect to play to the sfx queue.
  Each SFX comes with a unique ID, so each client can track for themselves which sounds have already been played.
  The sfx queue has size limited to 3 to limit the sound torrent tabbed out or lagged players will experience."
  [state _ sfx]
  (when-let [current-id (get-in @state [:sfx-current-id])]
    (swap! state update-in [:sfx] #(take 3 (conj % {:id (inc current-id) :name sfx})))
    (swap! state update-in [:sfx-current-id] inc)))
