(ns web.lobby
  (:require [clojure.string :refer [trim]]
            [web.mongodb :refer [object-id]]
            [web.ws :as ws]
            [web.stats :as stats]
            [web.diffs :refer [game-internal-view]]
            [game.utils :refer [remove-once]]
            [game.core :as core]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status legal-deck?]]
            [jinteki.utils :refer [superuser?]]
            [crypto.password.bcrypt :as bcrypt]
            [monger.collection :as mc]
            [monger.operators :refer [$or]]
            [differ.core :as differ]
            [medley.core :refer [find-first]]
            [clj-time.core :as t]))

(def log-collection "moderator_actions")

;; All games active on the server.
(defonce all-games (atom {}))

;; The most recent state of each game sent to clients.
(defonce old-states (atom {}))

;; A map from uid to gameid of the game the client is playing or spectating.
(defonce uid-gameids (atom {}))

(defn game-for-id
  "Returns the game map for the given gameid."
  [gameid]
  (get @all-games gameid))

(defn game-for-client
  "Returns the game map that the given uid is playing or spectating."
  [uid]
  (get @all-games (get @uid-gameids uid)))

(defn- username-is-player
  [username game]
  (some #(= username (get-in % [:user :username])) (:players game)))

(defn game-for-username
  "Returns the game map the given username is playing (but not spectating)"
  [username]
  (find-first #(username-is-player username %) (vals @all-games)))

(defn lobby-clients
  "Returns a seq of all uids playing or spectating a gameid."
  [gameid]
  (let [game (game-for-id gameid)]
    (keep :uid (concat (:players game) (:spectators game)))))

(def ^:private public-lobby-updates (atom {}))
(def ^:private game-lobby-updates (atom {}))
(def ^:private send-ready (atom true))
(def ^:private lobby-only-keys [:messages :spectators :mute-spectators :spectatorhands :timer :api-access])

(defn- game-public-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [gameid game]
  (game-internal-view (game-for-id gameid) (apply dissoc game lobby-only-keys)))

(defn game-lobby-view
  "Strips private server information from a game map, preparing to send the game to clients.
  Strips messages in addition to private info to keep payload size down"
  [gameid game]
  (game-internal-view (game-for-id gameid) (select-keys game lobby-only-keys)))

(defn update-lobbies-public-view
  "If public view keys exist, send to all connected"
  []
  (when (seq @public-lobby-updates)
    (let [[old] (reset-vals! public-lobby-updates {})]
      (ws/broadcast! :games/diff {:diff old}))
    (reset! send-ready false)))

(defn update-lobbies-joined-view
  "If private view exists, send only to those games clients"
  []
  (when (seq @game-lobby-updates)
    (let [[old] (reset-vals! game-lobby-updates {})]
      (doseq [[gameid update] (:update old)]
        (ws/broadcast-to! (lobby-clients gameid) :games/differ {:diff {:update {gameid update}}})))
    (reset! send-ready false)))

(defn- send-lobby
  "Called by a background thread to periodically send game lobby updates to all clients."
  []
  (when @send-ready
    (update-lobbies-public-view)
    (update-lobbies-joined-view)))

(defn refresh-lobby-update-in
  [gameid targets func]
  (let [target (first targets)
        [old] (swap-vals! all-games update-in (concat [gameid] targets) func)
        old-key-diff (select-keys (get old gameid) [target])
        key-diff (select-keys (game-for-id gameid) [target])
        lobby-updates (game-lobby-view gameid key-diff)
        public-updates (game-public-view gameid key-diff)]
    (when (seq lobby-updates)
      (swap! game-lobby-updates
             update-in [:update gameid] conj
             (differ/diff (game-lobby-view gameid old-key-diff) lobby-updates)))
    (when (seq public-updates)
      (swap! public-lobby-updates
             assoc-in [:update gameid target]
             (target (game-public-view gameid key-diff))))
    (send-lobby)))

(defn refresh-lobby-assoc-in
  [gameid targets val]
  (refresh-lobby-update-in gameid targets (fn [_] val)))

(defn refresh-lobby-dissoc
  [gameid]
  (swap! all-games dissoc gameid)
  (swap! public-lobby-updates update :delete #(conj % gameid))
  (send-lobby))

(defn refresh-lobby
  [gameid game]
  (swap! all-games assoc gameid game)
  (swap! public-lobby-updates assoc-in [:update gameid] (game-internal-view game game))
  (send-lobby))

(defn reset-send-lobby
  []
  (let [[old] (reset-vals! send-ready true)]
    (when-not old (send-lobby))))

(defn player?
  "True if the given uid is a player in the given game"
  [uid game]
  (some #(when (= uid (:uid %)) %) (:players game)))

(defn first-player?
  "True if the given uid is the first player in the given game"
  [uid game]
  (= uid (-> game :players first :uid)))

(defn spectator?
  "True if the given uid is a spectator in the given game"
  [uid game]
  (some #(when (= uid (:uid %)) %) (:spectators game)))

(defn player-or-spectator
  "True if the given uid is a player or spectator in the given game"
  [uid gameid]
  (when-let [game (game-for-id gameid)]
    (or (player? uid game)
        (spectator? uid game))))

(defn close-lobby
  "Closes the given game lobby, booting all players and updating stats."
  ([db game] (close-lobby db game nil))
  ([db {:keys [started gameid]} skip-on-close]
   (when started
     (stats/update-deck-stats db all-games gameid)
     (stats/update-game-stats db all-games gameid)
     (stats/push-stats-update db all-games gameid))
   (let [old-game (get @all-games gameid)
         callback (:on-close old-game)]
     (refresh-lobby-dissoc gameid)
     (swap! old-states dissoc gameid)
     (when (and (not skip-on-close) callback)
       (callback old-game)))))

(defn clear-inactive-lobbies
  "Called by a background thread to close lobbies that are inactive for some number of seconds."
  [db time-inactive]
  (doseq [{:keys [gameid last-update started] :as game} (vals @all-games)]
    (when (and gameid (t/after? (t/now) (t/plus last-update (t/seconds time-inactive))))
      (let [uids (lobby-clients gameid)]
        (if started
          (do (stats/game-finished db game)
              (ws/broadcast-to! uids :netrunner/timeout {:gameid gameid}))
          (ws/broadcast-to! uids :lobby/timeout {:gameid gameid}))
        (doseq [uid uids]
          (swap! uid-gameids dissoc uid))
        (close-lobby db game)))))

(defn remove-user
  "Removes the given uid from the given gameid, whether it is a player or a spectator.
  Deletes the game from the lobby if all players have left."
  [db uid gameid]
  (when-let [{:keys [players started state] :as game} (game-for-id gameid)]
    (cond (player? uid game)
          (refresh-lobby-update-in gameid [:players] #(remove-once (fn [p] (= uid (:uid p))) %))
          (spectator? uid game)
          (do
            (refresh-lobby-update-in gameid [:spectator-count] dec)
            (refresh-lobby-update-in gameid [:spectators] #(remove-once (fn [p] (= uid (:uid p))) %))))

    ;; update ending-players when someone drops to credit a completion properly.  Not if game is over.
    ; TODO add other player back in if other player rejoins

    (when state
      (let [winner (:winning-user @state)]
        (when (and (= 1 (count players)) started (not winner))
          (refresh-lobby-assoc-in gameid [:ending-players] players))))

    (let [{:keys [players] :as game} (game-for-id gameid)]
      (swap! uid-gameids dissoc uid)

      (when (empty? (filter identity players))
        (stats/game-finished db game)
        (close-lobby db game)))))

(defn already-in-game?
  "Checks if a user with the given username is already in the game"
  [{:keys [username]} {:keys [players spectators]}]
  (some #(= username (get-in % [:user :username])) (concat players spectators)))

(defn determine-player-side
  "Determines the side of a player based on their side and a requested side"
  [player request-side]
  (let [existing-side (:side player)]
    (if (= existing-side "Any Side")
      (if (= request-side "Any Side")
        (rand-nth ["Corp" "Runner"])
        (if (= "Corp" request-side) "Runner" "Corp"))
      existing-side)))

(defn join-game
  "Adds the given user as a player in the given gameid."
  [{:keys [username] :as user} uid gameid request-side]
  (let [{players :players :as game} (game-for-id gameid)
        existing-players-count (count (remove #(= username (get-in % [:user :username])) players))]
    (when (or (< existing-players-count 2)
              (already-in-game? user game))
      (let [existing-player (first (remove #(= username (get-in % [:user :username])) players))
            side (determine-player-side existing-player request-side)
            existing-player (assoc existing-player :side side)
            new-side (if (= "Corp" side) "Runner" "Corp")
            new-player {:user user
                        :uid uid
                        :side new-side}]
        (refresh-lobby-assoc-in gameid [:players] [existing-player new-player])
        (swap! uid-gameids assoc uid gameid)
        new-player))))

(defn spectate-game
  "Adds the given user as a spectator in the given gameid"
  [user uid gameid]
  (when (game-for-id gameid)
    (refresh-lobby-update-in gameid [:spectator-count] inc)
    (refresh-lobby-update-in gameid [:spectators]
           #(conj % {:user user
                     :uid uid}))
    (swap! uid-gameids assoc uid gameid)))

(defn swap-side
  "Returns a new player map with the player's :side switched"
  [player]
  (-> player
      (update :side #(if (= % "Corp")
                       "Runner"
                       "Corp"))
      (dissoc :deck)))

(defn change-side
  "Returns a new player map with the player's :side set to a new side"
  [player side]
  (-> player
      (assoc :side side)
      (dissoc :deck)))

(defn blocked-users
  [{:keys [players]}]
  (mapcat #(get-in % [:user :options :blocked-users]) players))

(defn superusers [db]
  (mc/find-maps db "users" {$or [{:isadmin true}
                                 {:ismoderator true}
                                 {:tournament-organizer true}]}))

(defn handle-lobby-list [{:keys [uid]}]
  (ws/broadcast-to! [uid] :games/list (mapv #(assoc (game-public-view (first %) (second %)) :selected (some (fn [id] (= id uid)) (lobby-clients (first %)))) @all-games)))

(defmethod ws/-msg-handler :lobby/list [event] (handle-lobby-list event))

(defmethod ws/-msg-handler :lobby/create
  [{{{:keys [username] :as user} :user} :ring-req
    uid :uid
    {:keys [title format timer allow-spectator save-replay api-access
            spectatorhands password room side]} :?data}]
  (when-not (game-for-client uid)
    (let [gameid (java.util.UUID/randomUUID)
          game {:date            (java.util.Date.)
                :gameid          gameid
                :title           title
                :allow-spectator allow-spectator
                :save-replay     save-replay
                :api-access      api-access
                :spectatorhands  spectatorhands
                :mute-spectators false
                :password        (when (not-empty password) (bcrypt/encrypt password))
                :room            room
                :format          format
                :players         [{:user user
                                   :uid uid
                                   :side side}]
                :spectators      []
                :spectator-count 0
                :timer           timer
                :messages        [{:user "__system__"
                                   :text (str username " has created the game.")}]
                :last-update     (t/now)}]
      (refresh-lobby gameid game)
      (swap! uid-gameids assoc uid gameid)
      (ws/broadcast-to! [uid] :lobby/select {:gameid gameid}))))

(defn- lobby-say
  [gameid {:keys [user text]}]
  (let [user-name (if (map? user) (select-keys user [:username :emailhash]) user)]
    (refresh-lobby-update-in gameid [:messages] #(conj % {:user user-name
                                                          :text (trim text)}))))

(defmethod ws/-msg-handler :lobby/leave
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    uid :uid}]
  (when-let [{gameid :gameid} (game-for-client uid)]
    (when (player-or-spectator uid gameid)
      (lobby-say gameid {:user "__system__" :text (str username " left the game.")})
      (remove-user db uid gameid))))

(defmethod ws/-msg-handler :lobby/say
  [{{user :user} :ring-req
    uid :uid
    {:keys [msg gameid]} :?data}]
  (when (player-or-spectator uid gameid)
    (lobby-say gameid {:user user
                       :text msg})))

(defmethod ws/-msg-handler :lobby/swap
  [{uid :uid
    {:keys [gameid side]} :?data}]
  (let [game (game-for-id gameid)
        first-player (first (:players game))]
    (when  (= (:uid first-player) uid)
      (if (< 1 (count (:players game)))
        (refresh-lobby-update-in gameid [:players] (partial mapv swap-side))
        (let [updated-player (change-side first-player side)]
          (refresh-lobby-assoc-in gameid [:players] [updated-player]))))))

(defn allowed-in-game
  [db game {:keys [username] :as user}]
  (or (superuser? user)
      (not-any? #(= username %) (blocked-users game))
      (some #(= username (:username %)) (superusers db))))

(defmethod ws/-msg-handler :lobby/join
  [{{db :system/db
     {:keys [username isadmin] :as user} :user} :ring-req
    uid :uid
    {:keys [gameid password request-side]} :?data
    reply-fn :?reply-fn}]
  (if-let [{game-password :password :as game} (@all-games gameid)]
    (when (and user game (allowed-in-game db game user))
      (if (or (empty? game-password)
              (bcrypt/check password game-password)
              isadmin)
        (do (ws/broadcast-to! [uid] :lobby/select {:gameid gameid})
            (ws/broadcast-to! [uid] :games/diff {:diff {:update {gameid (game-lobby-view gameid game)}}})
            (join-game user uid gameid request-side)
            (lobby-say gameid {:user "__system__"
                               :text (str username " joined the game.")})
            (ws/broadcast-to! (lobby-clients gameid) :lobby/notification "ting")
            (when reply-fn (reply-fn 200)))
        (when reply-fn (reply-fn 403))))
    (when reply-fn (reply-fn 404))))

(defn handle-lobby-watch
  ;; Handles a watch command when a game has not yet started.
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    uid :uid
    {:keys [gameid password]} :?data
    reply-fn :?reply-fn}]
  (if-let [{game-password :password started :started :as game}
           (@all-games gameid)]
    (when (and user game (allowed-in-game db game user))
      (if started
        false  ; don't handle this message, let game/handle-game-watch.
        (if (and (not (already-in-game? user game))
                 (or (empty? game-password)
                     (bcrypt/check password game-password)))
          (do (ws/broadcast-to! [uid] :games/diff {:diff {:update {gameid (game-lobby-view gameid game)}}})
              (ws/broadcast-to! [uid] :lobby/select {:gameid gameid})
              (spectate-game user uid gameid)
              (lobby-say gameid {:user "__system__"
                                 :text (str username " joined the game as a spectator.")})
              (ws/broadcast-to! (lobby-clients gameid) :lobby/notification "ting")
              (when reply-fn (reply-fn 200))
              true)
          (when reply-fn
            (reply-fn 403)
            false))))
    (when reply-fn
      (reply-fn 404)
      false)))

(defmethod ws/-msg-handler :lobby/deck
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    uid :uid
    deck-id :?data}]
  (let [game (game-for-client uid)
        first-player (if (= uid (:uid (first (:players game)))) 0 1)
        gameid (:gameid game)
        map-card (fn [c] (update-in c [:card] @all-cards))
        unknown-card (fn [c] (nil? (:card c)))
        deck (as-> (mc/find-one-as-map db "decks" {:_id (object-id deck-id) :username username}) d
                   (update-in d [:cards] #(mapv map-card %))
                   (update-in d [:cards] #(vec (remove unknown-card %)))
                   (update-in d [:identity] #(@all-cards (:title %)))
                   (assoc d :status (calculate-deck-status d)))]
    (when (and (:identity deck)
               (player? uid game)
               (or (= (:format game) "casual")
                   (legal-deck? deck (:format game))))
      (when (= "tournament" (:room game))
        (lobby-say gameid {:user "__system__"
                           :text (str username " has selected deck with tournament hash " (:hash deck))}))
      (refresh-lobby-assoc-in gameid [:players first-player :deck] deck))))

(defmethod ws/-msg-handler :lobby/rename-game
  [{{db :system/db
     {:keys [username isadmin ismoderator]} :user} :ring-req
    {:keys [gameid]} :?data}]
  (when-let [game (game-for-id gameid)]
    (when (and username
               (or isadmin ismoderator))
      (let [player-name (:username (:user (first (:players game))))
            bad-name (:title game)]
        (refresh-lobby-assoc-in gameid [:title] (str player-name "'s game"))
        (mc/insert db log-collection
                   {:moderator username
                    :action :rename-game
                    :game-name bad-name
                    :first-player player-name
                    :date (java.util.Date.)})))))

(defmethod ws/-msg-handler :lobby/delete-game
  [{{db :system/db
     {:keys [username isadmin ismoderator]} :user} :ring-req
    {:keys [gameid]} :?data}]
  (when-let [game (game-for-id gameid)]
    (when (and username
               (or isadmin ismoderator))
      (let [player-name (:username (:user (first (:players game))))
            uids (lobby-clients gameid)]
        (doseq [uid uids]
          (swap! uid-gameids dissoc uid))
        (close-lobby db game)
        (ws/broadcast-to! uids :lobby/timeout {:gameid gameid})
        (mc/insert db log-collection
                   {:moderator username
                    :action :delete-game
                    :game-name (:title game)
                    :first-player player-name
                    :date (java.util.Date.)})))))
