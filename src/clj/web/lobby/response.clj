(ns web.lobby.response
  "Holds all of the logic necessary to send lobby updates to clients"
  (:require
   [executor.core :as executor]
   [executor.loggers :refer [log]]
   [game.core.diffs :as diffs]
   [cheshire.core :as json]
   [jinteki.utils :refer [select-non-nil-keys side-from-str]]
   [monger.collection :as mc]
   [web.lobby.utils :refer [filter-lobby-list get-players-and-spectators
                            player?]]
   [web.stats :as stats]
   [web.ws :as ws]))

(defn- filter-lobby-user
  "Only take keys that are useful in the lobby from a user map"
  [user]
  (let [stats (select-keys (:stats user) [:games-started :games-completed])]
    (-> user
        (select-keys [:_id :username :emailhash])
        (assoc :stats stats))))

(defn strip-deck [player lobby]
  (if-let [{:keys [_id] :as deck} (:deck player)]
    (let [fmt (:format lobby)
          fmt-kw (keyword fmt)
          legal (get-in deck [:status fmt-kw :legal])
          status {:format fmt
                  fmt-kw {:legal legal}}
          deck (-> deck
                   (select-keys
                     (if (:started lobby)
                       [:name :date :identity :hash]
                       [:name :date]))
                   (assoc :_id (str _id) :status status))]
      (assoc player :deck deck))
    player))

(defn user-public-view
  "Strips private server information from a player map."
  [lobby player]
  (-> player
      (dissoc :uid)
      (update :user filter-lobby-user)
      (strip-deck lobby)))

(defn prepare-players [lobby players]
  (->> players
       (mapv #(user-public-view lobby %))
       (not-empty)))

(defn prepare-original-players [players]
  (map #(-> %
            (update :user select-keys [:username :emailhash])
            (select-keys [:user]))
       players))

(def lobby-keys
  [:allow-spectator
   :api-access
   :date
   :format
   :gameid
   :messages
   :mute-spectators
   :original-players
   :password
   :players
   :room
   :save-replay
   :spectators
   :spectatorhands
   :started
   :timer
   :title])

(defn lobby-summary
  "Strips private server information from a game map, preparing to send the game to clients."
  ([lobby] (lobby-summary lobby nil))
  ([lobby participating?]
   (-> lobby
       (update :password boolean)
       (update :players #(prepare-players lobby %))
       (update :spectators #(prepare-players lobby %))
       (update :original-players prepare-original-players)
       (update :messages #(when participating? %))
       (select-non-nil-keys lobby-keys))))

(defn prepare-lobby-state [lobby]
  (let [lobby-state (lobby-summary lobby true)]
    (for [user (get-players-and-spectators lobby)
          :let [uid (:uid user)]
          :when (some? uid)]
      [uid lobby-state])))

(executor/reg-fx
  :lobby/state
  (fn send-lobby-state! [lobby]
    (doseq [[uid payload] (prepare-lobby-state lobby)]
      (ws/chsk-send! uid [:lobby/state payload]))))

(defn summaries-for-lobbies [filtered-lobbies]
  (->> filtered-lobbies
       (map lobby-summary)
       (sort-by :date)
       (reverse)
       (sort-by :started)
       (into [])))

(comment
  (->> (for [x (range 5 10)]
         {:date (doto (java.util.Calendar/getInstance)
                  (.set (+ 2000 (+ (rand-int x) (rand-int x))) 1 2))
          :started (rand-nth [true false])})
       (summaries-for-lobbies)))

(defn prepare-lobby-list
  [db]
  (let [lobbies (vals (:lobbies db))]
    (for [user (vals (:users db))
          :let [uid (:uid user)
                filtered-lobbies (filter-lobby-list lobbies user)
                lobby-summaries (summaries-for-lobbies filtered-lobbies)]]
      [uid lobby-summaries])))

(executor/reg-fx
  :lobby/broadcast-list
  (fn send-lobby-list! [db]
    (doseq [[uid payload] (prepare-lobby-list db)
            :when (some? uid)]
      (ws/chsk-send! uid [:lobby/list payload]))))

(executor/reg-fx
  :lobby/ting
  (fn lobby-ting [lobby]
    (doseq [user (get-players-and-spectators lobby)
            :let [uid (:uid user)]]
      (ws/chsk-send! uid [:lobby/notification "ting"]))))

(defn clear-lobby-state [uid]
  (ws/chsk-send! uid [:lobby/state]))

(executor/reg-fx :lobby/clear #'clear-lobby-state)

(defn remove-player-from-game!
  [[lobby uid]]
  (let [state (:state lobby)
        side (side-from-str (:side (player? uid lobby) ""))]
    (when (some? side)
      (swap! state update side dissoc :user))))

(executor/reg-fx :game/remove-user #'remove-player-from-game!)

(defn close-lobby!
  "Closes the given game lobby, booting all players and updating stats.
  db is :system/db."
  ([db lobby] (close-lobby! db lobby nil))
  ([db {:keys [started on-close] :as lobby} skip-on-close]
   (when started
     (stats/game-finished db lobby)
     (stats/update-deck-stats db lobby)
     (stats/update-game-stats db lobby)
     (stats/push-stats-update db lobby))
   (doseq [uid (keep :uid (get-players-and-spectators lobby))]
     (clear-lobby-state uid))
   (when (and (not skip-on-close) on-close)
     (on-close lobby))))

(executor/reg-fx
  :lobby/close-lobby
  (fn [[system-db lobby]] (close-lobby! system-db lobby)))

(executor/reg-fx
  :db/mod-action
  (fn [[system-db mod-action]]
    (mc/insert system-db :moderator_actions mod-action)))

(executor/reg-fx
  :lobby/timeout
  (fn send-timeout! [lobby]
    (doseq [uid (keep :uid (get-players-and-spectators lobby))]
      (ws/chsk-send! uid [:lobby/timeout (:gameid lobby)])
      (when (:started lobby)
        (ws/chsk-send! uid [:game/timeout (:gameid lobby)])))))

(executor/reg-fx
  :stats/game-started
  (fn [[system-db lobby]]
    (stats/game-started system-db lobby)))

(defn select-state [side {:keys [runner-state corp-state spect-state]}]
  (json/generate-string
    (case side
      "Corp" corp-state
      "Runner" runner-state
      spect-state)))

(executor/reg-fx
  :game/start
  (fn [lobby]
    (let [diffs (diffs/public-states (:state lobby))]
      (doseq [{:keys [uid side]} (get-players-and-spectators lobby)
              :when (some? uid)]
        (ws/chsk-send! uid [:game/start (select-state side diffs)])))))

(defn game-diff-json
  "Converts the appropriate diff to json"
  [gameid side {:keys [runner-diff corp-diff spect-diff]}]
  (json/generate-string
    {:gameid gameid
     :diff (cond
             (= side "Corp") corp-diff
             (= side "Runner") runner-diff
             :else spect-diff)}))

(defn send-state-diffs
  "Sends diffs generated by public-diffs to all connected clients."
  [lobby diffs]
  (doseq [{:keys [uid side]} (get-players-and-spectators lobby)
          :when (some? uid)]
    (ws/chsk-send! uid [:game/diff (game-diff-json (:gameid lobby) side diffs)])))

(defn update-and-send-diffs!
    "Updates the old-states atom with the new game state, then sends a :game/diff
    message to game clients."
    [[f {state :state :as lobby} & args]]
    (when (and state @state)
      (let [old-state @state]
        (try
          (let [_ (apply f state args)
                diffs (diffs/public-diffs old-state state)]
            (swap! state update :history conj (:hist-diff diffs))
            (send-state-diffs lobby diffs))
          (catch Exception e
            (reset! state old-state)
            (log :error e)
            (doseq [uid (keep :uid (get-players-and-spectators lobby))]
              (ws/chsk-send! uid [:game/error])))))))

(executor/reg-fx
  :game/update-state
  #'update-and-send-diffs!)

(defn send-error-notification [uid] (ws/chsk-send! uid [:game/error]))

(executor/reg-fx
  :game/error
  #'send-error-notification)

(executor/reg-fx
  :game/typing
  (fn [[lobby uid typing]]
    (doseq [uid (keep :uid (remove #(= uid (:uid %)) (:players lobby)))]
      (ws/chsk-send! uid [:game/typing typing]))))

