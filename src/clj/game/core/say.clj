(ns game.core.say
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [game.core.toasts :refer [toast]]))

(defn make-message
  "Create a message map, along with timestamp if none is provided."
  [{:keys [user text timestamp]
    :or {timestamp (inst/now)}}]
  {:user (if (= "__system__" user) user (select-keys user [:username :emailhash]))
   :text (if (string? text) (str/trim text) text)
   :timestamp timestamp})

(defn make-system-message
  "Creates a message map from the __system__ user, which won't display a username."
  [text]
  (make-message {:user "__system__" :text text}))

(defn- select-pronoun
  "Selects an appropriate plurular pronoun
  'their' is neuter, so it's appropriate to everyone as a fallback"
  [user]
  (let [key (get-in user [:options :pronouns])]
    (case key
      "she" "her"
      "he" "his"
      "it" "its"
      "their")))

(defn- insert-pronouns
  "inserts pronouns into text based on the side speaking"
  [state side text]
  (let [corp-pronoun (select-pronoun (get-in @state [:corp :user]))
        runner-pronoun (select-pronoun (get-in @state [:runner :user]))
        user-pronoun (cond
                       (= side :corp) corp-pronoun
                       (= side :runner) runner-pronoun
                       :else "their")]
    (-> text
        (str/replace #"(\[pronoun\])|(\[their\])" user-pronoun)
        (str/replace #"\[corp-pronoun\]" corp-pronoun)
        (str/replace #"\[runner-pronoun\]" runner-pronoun))))

(defn say
  "Prints a message to the log as coming from the given user."
  [state side {:keys [user text]}]
  (let [author (or user (get-in @state [side :user]))
        message (make-message {:user author :text (insert-pronouns state side text)})]
    (swap! state update :log conj message)
    (swap! state assoc :typing false)))

(defn system-say
  "Prints a system message to log (`say` from user __system__)"
  ([state side text] (system-say state side text nil))
  ([state side text {:keys [hr]}]
   (say state side (make-system-message (str text (when hr "[hr]"))))))

(defn unsafe-say
  "Prints a reagent hiccup directly to the log. Do not use for any user-generated content!"
  [state text]
  (let [message (make-system-message text)]
    (swap! state update :log conj message)))

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
  (system-say state nil (str (:title card) " " text ".")))

(defn implementation-msg
  [state card]
  (when (not= :full (:implementation card))
    (system-say state nil (str "[!] " (:title card) " - " (:implementation card)))))

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
  (swap! state (fn [state]
                 (if-let [current-id (:sfx-current-id state)]
                   (-> state
                       (update :sfx conj {:id (inc current-id) :name sfx})
                       (update :sfx #(take 3 %))
                       (update :sfx-current-id inc))
                   state))))
