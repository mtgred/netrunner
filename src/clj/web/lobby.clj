(ns web.lobby
  (:require [clojure.string :refer [trim]]
            [web.db :refer [db object-id]]
            [web.utils :refer [response tick]]
            [web.ws :as ws]
            [web.stats :as stats]
            [web.diffs :refer [game-internal-view]]
            [game.utils :refer [remove-once]]
            [game.core :as core]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status]]
            [jinteki.utils :refer [str->int superuser?]]
            [crypto.password.bcrypt :as bcrypt]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [cheshire.core :as json]
            [differ.core :as differ]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId))

(def log-collection "moderator_actions")

;; All games active on the server.
(defonce all-games (atom {}))

;; The most recent state of each game sent to clients.
(defonce old-states (atom {}))

;; A map from client-id to gameid of the game the client is playing or spectating.
(defonce client-gameids (atom {}))

(defn game-for-id
  "Returns the game map for the given gameid."
  [gameid]
  (get @all-games gameid))

(defn game-for-client
  "Returns the game map that the given client-id is playing or spectating."
  [client-id]
  (get @all-games (get @client-gameids client-id)))

(defn lobby-clients
  "Returns a seq of all client-ids playing or spectating a gameid."
  [gameid]
  (let [game (game-for-id gameid)]
    (keep :ws-id (concat (:players game) (:spectators game)))))

(let [public-lobby-updates (atom {})
      game-lobby-updates (atom {})
      send-ready (atom true)]

  (def lobby-only-keys [:messages :spectators :mute-spectators :password :spectatorhands])

  (defn game-public-view
    "Strips private server information from a game map, preparing to send the game to clients."
    [gameid game]
    (game-internal-view (game-for-id gameid) (apply dissoc game lobby-only-keys)))

  (defn game-lobby-view
    "Strips private server information from a game map, preparing to send the game to clients. Strips messages in addition to private info to keep payload size down"
    [gameid game]
    (game-internal-view (game-for-id gameid) (select-keys game lobby-only-keys)))

  (defn send-lobby
    "Called by a background thread to periodically send game lobby updates to all clients."
    []
    ;; If public view keys exist, send to all connected
    (when @send-ready
      (when (seq @public-lobby-updates)
        (let [[old] (reset-vals! public-lobby-updates {})]
          (ws/broadcast! :games/diff {:diff old}))
          (reset! send-ready false))
      ;; If private view exists, send only to those games clients
      (when (seq @game-lobby-updates)
        (let [[old] (reset-vals! game-lobby-updates {})]
          (doseq [[gameid update] (:update old)]
              (ws/broadcast-to! (lobby-clients gameid) :games/differ {:diff {:update {gameid update}}}))
          (reset! send-ready false)))))

  (defn refresh-lobby-update-in
    [gameid targets func]
    (let [[old] (swap-vals! all-games update-in (concat [gameid] targets) func)
          old-key-diff (select-keys (get old gameid) [(first targets)])
          key-diff (select-keys (game-for-id gameid) [(first targets)])]
      
      (if (seq (game-lobby-view gameid key-diff))
        (swap! game-lobby-updates update-in [:update gameid]
          (fn [v]
            (conj v (differ/diff (game-lobby-view gameid old-key-diff) (game-lobby-view gameid key-diff)))))
        (swap! public-lobby-updates assoc-in (concat [:update gameid] [(first targets)]) ((first targets) (game-public-view gameid key-diff))))
      (send-lobby)))

  (defn refresh-lobby-assoc-in
    [gameid targets val]
    (refresh-lobby-update-in gameid targets (fn [n] val)))

  (defn refresh-lobby-dissoc
    [gameid]
    (swap! all-games dissoc gameid)
    (swap! public-lobby-updates update :delete #(conj % gameid))
    (send-lobby))

  (defn refresh-lobby
    [gameid game]
    (swap! all-games assoc gameid game)
    (swap! public-lobby-updates assoc-in [:update gameid] (game-public-view gameid game))
    (send-lobby))

  (defn reset-send-lobby
    []
    (let [[old] (reset-vals! send-ready true)]
      (if-not old (send-lobby)))))

(defn player?
  "True if the given client-id is a player in the given gameid"
  [client-id gameid]
  (when-let [game (game-for-id gameid)]
    (some #(when (= client-id (:ws-id %)) %) (:players game))))

(defn first-player?
  "True if the given client-id is the first player in the given gameid"
  [client-id gameid]
  (when-let [game (game-for-id gameid)]
    (= client-id (-> game :players first :ws-id))))

(defn spectator?
  "True if the given client-id is a spectator in the given gameid"
  [client-id gameid]
  (when-let [game (game-for-id gameid)]
    (not (some #(when (= client-id (:ws-id %)) %) (:players game))))) ; Faster to check players than to check spectators

(defn player-or-spectator
  "True if the given client-id is a player or spectator in the given gameid"
  [client-id gameid]
  (when-let [game (game-for-id gameid)]
    (or (player? client-id gameid)
        (spectator? client-id gameid))))

(defn close-lobby
  "Closes the given game lobby, booting all players and updating stats."
  [{:keys [started gameid] :as game}]
  (when started
    (stats/update-deck-stats all-games gameid)
    (stats/update-game-stats all-games gameid)
    (stats/push-stats-update all-games gameid))

  (let [callback (get-in @all-games [gameid :on-close])]
    (refresh-lobby-dissoc gameid)
    (swap! old-states dissoc gameid)
    (when callback
      (callback))))

(defn clear-inactive-lobbies
  "Called by a background thread to close lobbies that are inactive for some number of seconds."
  [time-inactive]
  (doseq [{:keys [gameid last-update started] :as game} (vals @all-games)]
    (when (and gameid (t/after? (t/now) (t/plus last-update (t/seconds time-inactive))))
      (let [clientids (lobby-clients gameid)]
        (if started
          (do (stats/game-finished game)
              (ws/broadcast-to! clientids :netrunner/timeout (json/generate-string
                                                               {:gameid gameid})))
          (ws/broadcast-to! clientids :lobby/timeout {:gameid gameid}))
        (doseq [client-id clientids]
          (swap! client-gameids dissoc client-id))
        (close-lobby game)))))

(defn remove-user
  "Removes the given client-id from the given gameid, whether it is a player or a spectator.
  Deletes the game from the lobby if all players have left."
  [client-id gameid]
  (when-let [{:keys [players started state] :as game} (game-for-id gameid)]
    (cond (player? client-id gameid)
          (swap! all-games update-in [gameid :players] #(remove-once (fn [p] (= client-id (:ws-id p))) %))

          (spectator? client-id gameid)
          (do 
            (refresh-lobby-update-in gameid [:spectator-count] dec)
            (refresh-lobby-update-in gameid [:spectators] #(remove-once (fn [p] (= client-id (:ws-id p))) %))))

    ;; update ending-players when someone drops to credit a completion properly.  Not if game is over.
    ; TODO add other player back in if other player rejoins

    (when state
      (let [winner (:winning-user @state)]
        (when (and (= 1 (count players)) started (not winner))
          (refresh-lobby-assoc-in gameid [:ending-players] players))))

    (let [{:keys [players] :as game} (game-for-id gameid)]
      (swap! client-gameids dissoc client-id)

      (if (empty? (filter identity players))
        (do
          (stats/game-finished game)
          (close-lobby game))))))

(defn already-in-game?
  "Checks if a user with the given database id (:_id) is already in the game"
  [{:keys [username] :as user} {:keys [players spectators] :as game}]
  (some #(= username (get-in % [:user :username])) (concat players spectators)))

(defn join-game
  "Adds the given user as a player in the given gameid."
  [{:keys [options _id username] :as user} client-id gameid]
  (let [{players :players :as game} (game-for-id gameid)
        existing-players-count (count (remove #(= username (get-in % [:user :username])) players))]
    (when (or (< existing-players-count 2)
              (already-in-game? user game))
      (let [remaining-player (first (remove #(= username (get-in % [:user :username])) players))
            side (:side remaining-player)
            new-side (if (= "Corp" side) "Runner" "Corp")
            new-player {:user    user
                        :ws-id   client-id
                        :side    new-side
                        :options options}]
        (refresh-lobby-assoc-in gameid [:players] [remaining-player new-player])
        (swap! client-gameids assoc client-id gameid)
        new-player))))

(defn spectate-game
  "Adds the given user as a spectator in the given gameid"
  [user client-id gameid]
  (when-let [{:keys [started spectators] :as game} (game-for-id gameid)]
    (refresh-lobby-update-in gameid [:spectator-count] inc)
    (refresh-lobby-update-in gameid [:spectators]
           #(conj % {:user  user
                     :ws-id client-id}))
    (swap! client-gameids assoc client-id gameid)))

(defn swap-side
  "Returns a new player map with the player's :side switched"
  [player]
  (-> player
      (update-in [:side] #(if (= % "Corp")
                            "Runner"
                            "Corp"))
      (dissoc :deck)))

(defn blocked-users
  [{:keys [players] :as game}]
  (mapcat #(get-in % [:user :options :blocked-users]) players))

(defn superusers []
  (mc/find-maps db "users" {$or [{:isadmin true}
                                 {:ismoderator true}
                                 {:tournament-organizer true}]}))

(defn allowed-in-game [game {:keys [username] :as user}]
  (or (superuser? user)
      (not-any? #(= username %) (blocked-users game))
      (some #(= username %) (map :username (superusers)))))

(defn handle-lobby-list [{:keys [client-id] :as msg}]
  (ws/broadcast-to! [client-id] :games/list (mapv #(game-public-view (first %) (second %)) @all-games)))

(defn handle-lobby-create
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [title format allow-spectator spectatorhands password room side options]} :?data :as event}]
  (let [gameid (java.util.UUID/randomUUID)
        game {
              :date            (java.util.Date.)
              :gameid          gameid
              :title           title
              :allow-spectator allow-spectator
              :spectatorhands  spectatorhands
              :mute-spectators false
              :password        (when (not-empty password) (bcrypt/encrypt password))
              :room            room
              :format          format
              :players         [{:user    user
                                 :ws-id   client-id
                                 :side    side
                                 :options options}]
              :spectators      []
              :spectator-count  0
              :messages        [{:user "__system__"
                                 :text (str username " has created the game.")}]
              :last-update     (t/now)}]
    (refresh-lobby gameid game)
    (swap! client-gameids assoc client-id gameid)
    (ws/broadcast-to! [client-id] :lobby/select {:gameid gameid})))

(defn lobby-say
  [gameid {:keys [user text]}]
  (refresh-lobby-update-in gameid [:messages] #(conj % {:user user
                                                        :text (trim text)})))

(defn handle-lobby-leave
  [{{{:keys [username]} :user} :ring-req
    client-id                  :client-id}]
  (when-let [{gameid :gameid} (game-for-client client-id)]
    (when (player-or-spectator client-id gameid)
      (lobby-say gameid {:user "__system__" :text (str username " left the game.")})
      (remove-user client-id gameid))))

(defn handle-lobby-say
  [{{user :user}         :ring-req
    client-id            :client-id
    {:keys [msg gameid]} :?data}]
  (when (player-or-spectator client-id gameid)
    (lobby-say gameid {:user user
                       :text msg})))

(defn handle-swap-sides
  [{client-id :client-id
    gameid    :?data}]
  (let [game (game-for-id gameid)
        fplayer (first (:players game))]
    (when (= (:ws-id fplayer) client-id)
      (refresh-lobby-update-in gameid [:players] (partial mapv swap-side)))))

(defn handle-lobby-join
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid password]}           :?data
    reply-fn                            :?reply-fn
    :as                                 msg}]
  (if-let [{game-password :password :as game} (@all-games gameid)]
    (when (and user game (allowed-in-game game user))
      (if (or (empty? game-password)
              (bcrypt/check password game-password))
        (do (join-game user client-id gameid)
            (lobby-say gameid {:user "__system__"
                               :text (str username " joined the game.")})
            (ws/broadcast-to! (lobby-clients gameid) :lobby/notification "ting")
            (ws/broadcast-to! [client-id] :lobby/select {:gameid gameid})
            (ws/broadcast-to! [client-id] :games/diff {:diff {:update {gameid (game-lobby-view gameid game)}}})
            (when reply-fn (reply-fn 200)))
        (when reply-fn (reply-fn 403))))
    (when reply-fn (reply-fn 404))))

(defn handle-lobby-watch
  "Handles a watch command when a game has not yet started."
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid password]}           :?data
    reply-fn                            :?reply-fn}]
  (if-let [{game-password :password state :state started :started :as game}
           (@all-games gameid)]
    (when (and user game (allowed-in-game game user))
      (if started
        false  ; don't handle this message, let game/handle-game-watch.
        (if (and (not (already-in-game? user game))
                 (or (empty? game-password)
                     (bcrypt/check password game-password)))
          (do 
              (spectate-game user client-id gameid)
              (lobby-say gameid {:user "__system__"
                                 :text (str username " joined the game as a spectator.")})
              (ws/broadcast-to! [client-id] :games/diff {:diff {:update {gameid (game-lobby-view gameid game)}}})
              (ws/broadcast-to! (lobby-clients gameid) :lobby/notification "ting")
              (ws/broadcast-to! [client-id] :lobby/select {:gameid gameid :started started})
              (when reply-fn (reply-fn 200))
              true)
          (when reply-fn
            (reply-fn 403)
            false))))
    (when reply-fn
      (reply-fn 404)
      false)))

(defn handle-select-deck
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    deck-id                             :?data}]
  (let [game (game-for-client client-id)
        first-player (if (= client-id (:ws-id (first (:players game)))) 0 1)
        gameid (:gameid game)

        map-card (fn [c] (update-in c [:card] @all-cards))
        unknown-card (fn [c] (nil? (:card c)))
        deck (as-> (mc/find-one-as-map db "decks" {:_id (object-id deck-id) :username username}) d
                   (update-in d [:cards] #(mapv map-card %))
                   (update-in d [:cards] #(vec (remove unknown-card %)))
                   (update-in d [:identity] #(@all-cards (:title %)))
                   (assoc d :status (calculate-deck-status d)))]
    (when (and (:identity deck)
               (player? client-id gameid))
      (refresh-lobby-assoc-in gameid [:players first-player :deck] deck))))

(defn handle-rename-game
  [{{{:keys [username isadmin ismoderator] :as user} :user} :ring-req
    client-id                                     :client-id
    {:keys [gameid]} :?data :as event}]
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

(ws/register-ws-handlers!
  :chsk/uidport-open #'handle-lobby-list
  :lobby/list #'handle-lobby-list
  :lobby/create #'handle-lobby-create
  :lobby/leave #'handle-lobby-leave
  :lobby/join #'handle-lobby-join
  :lobby/watch #'handle-lobby-watch
  :lobby/say #'handle-lobby-say
  :lobby/swap #'handle-swap-sides
  :lobby/deck #'handle-select-deck
  :lobby/rename-game #'handle-rename-game)
