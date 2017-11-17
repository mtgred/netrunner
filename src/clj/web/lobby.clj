(ns web.lobby
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response tick remove-once]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [game.main]
            [game.core :as core]
            [crypto.password.bcrypt :as bcrypt]
            [game.main :as main])
  (:import org.bson.types.ObjectId))

;; All games active on the server.
(defonce all-games (atom {}))

;; The most recent state of each game sent to clients.
(defonce old-states (atom {}))

;; A map from client-id to gameid of the game the client is playing or spectating.
(defonce client-gameids (atom {}))

(defn game-for-client
  "Returns the game map that the given client-id is playing or spectating."
  [client-id]
  (get @all-games (get @client-gameids client-id)))

(defn game-public-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [game]
  (-> game
      (dissoc :state)))

(let [lobby-update (atom true)
      lobby-updates (atom {})]

  (defn refresh-lobby
    "Schedules the given gameid to be included in the next push of game lobby updates.
    type is :create, :delete, or :update"
    [type gameid]
    (reset! lobby-update true)
    (swap! lobby-updates assoc-in [type gameid]
           (if (= type :delete)
             "0"
             (game-public-view (get @all-games gameid)))))

  (defn send-lobby
    "Called by a background thread to periodically send game lobby updates to all clients."
    []
    (when @lobby-update
      (reset! lobby-update false)
      (let [[old _] (reset-vals! lobby-updates {})]
        (ws/broadcast! :games/diff {:diff old})))))

(defn player?
  "True if the given client-id is a player in the given gameid"
  [client-id gameid]
  (when-let [game (get @all-games gameid)]
    (some #(= client-id (:id %)) (:players game))))

(defn first-player?
  "True if the given client-id is the first player in the given gameid"
  [client-id gameid]
  (when-let [game (get @all-games gameid)]
    (= client-id (-> game :players first :id))))

(defn spectator?
  "True if the given client-id is a spectator in the given gameid"
  [client-id gameid]
  (when-let [game (get @all-games gameid)]
    (some #(= client-id (:id %)) (:spectators game))))

(defn player-or-spectator
  "True if the given client-id is a player or spectator in the given gameid"
  [client-id gameid]
  (when-let [game (get @all-games gameid)]
    (or (player? client-id gameid)
        (spectator? client-id gameid))))

(defn game-started?
  "Returns true if game has started"
  [gameid]
  (get-in @all-games [gameid :started]))

(defn inc-deck-stats
  "Update deck stats for a given counter"
  [deck-id key]
  (mc/update db "decks" {:_id (object-id deck-id)} {"$inc" {key 1}})
  (response 200 {:message "OK"}))

(defn inc-game-stats
  "Update user's game stats for a given counter"
  [user-id key]
  (mc/update db "users" {:_id (object-id user-id)} {"$inc" {key 1}})
  (response 200 {:message "OK"}))

(defn update-player-stats
  "Update stats for player decks on game ending"
  [gameid]
  (let [orig-players (get-in @all-games [gameid :original-players])
        end-players (get-in @all-games [gameid :ending-players])
        no-id (response 409 {:message "Deck is missing _id"})] ;likely needs to be ending playe

    (doseq [p orig-players]
      (when-let [enable-deckstats (get-in p [:user :options :deckstats])]
        (if-let [deck-id (get-in p [:deck :_id])]
          (inc-deck-stats deck-id :stats.games-started)
          no-id)))
    (doseq [p end-players]
      (when-let [enable-deckstats (get-in p [:user :options :deckstats])]
        (if-let [deck-id (get-in p [:deck :_id])]
          (inc-deck-stats deck-id :stats.games-completed)
          no-id)))))

;; if response.state.corp.user and response.state.runner.user # have two users in the game
;; room = response.state.room
;; inc_corp_game_start(response.state.corp, room)
;; inc_runner_game_start(response.state.runner, room)
;; if response.state.winner # and someone won
;; inc_game_win(response.state[response.state.winner], room)
;; inc_game_loss(response.state[response.state.loser], room)
;; else if response.state["final-user"] # someone left before the game was won
;; final_side = response.state["final-user"].side
;; inc_game_final_user(response.state[final_side], room)

(defn remove-user
  "Removes the given client-id from the given gameid, whether it is a player or a spectator.
  Deletes the game from the lobby if all players have left."
  [client-id gameid]
  (cond (player? client-id gameid)
        (swap! all-games update-in [gameid :players] #(remove-once (fn [p] (not= client-id (:id p))) %))

        (spectator? client-id gameid)
        (swap! all-games update-in [gameid :spectators] #(remove-once (fn [p] (not= client-id (:id p))) %)))

  (when-let [{:keys [players spectators] :as game} (get @all-games gameid)]
    (swap! client-gameids dissoc client-id)

    ;; TODO: danhut
    ; create finalUser when dropping to one player to credit a completion
    ; delete this if other player rejoints
    (when (and (= 1 (count players)) (game-started? gameid))
      (update-player-stats gameid)
      )

    (if (and (empty? players) (empty? spectators))
      (do
        ;; TODO: stats update goes here
        (update-player-stats gameid)

        ;; TODO: send "remove" to game server to get the "player has left the game" note

        (refresh-lobby :delete gameid)
        (swap! all-games dissoc gameid)
        (swap! old-states dissoc gameid))
      (refresh-lobby :update gameid))))

(defn lobby-clients
  "Returns a seq of all client-ids playing or spectating a gameid."
  [gameid]
  (let [game (get @all-games gameid)]
    (map :id (concat (:players game) (:spectators game)))))


(defn join-game
  "Adds the given user as a player in the given gameid."
  ; TODO: does this need a lock to prevent simultaneous joins? Wasn't a problem in single-threaded Node.
  [{options :options :as user} client-id gameid]
  (let [{players :players :as game} (get @all-games gameid)]
    (when (< (count players) 2)
      (let [{side :side :as fplayer} (first players)
            new-side (if (= "Corp" side) "Runner" "Corp")]
        (swap! all-games update-in [gameid :players]
               #(conj % {:user    user
                         :id      client-id
                         :side    new-side
                         :options options}))
        (swap! client-gameids assoc client-id gameid)
        (refresh-lobby :update gameid)))))

(defn spectate-game
  "Adds the given user as a spectator in the given gameid"
  [user client-id gameid]
  (when-let [{:keys [started spectators] :as game} (get @all-games gameid)]
    (swap! all-games update-in [gameid :spectators]
           #(conj % {:user    user
                         :id      client-id}))
    (swap! client-gameids assoc client-id gameid)
    (refresh-lobby :update gameid)))

(defn swap-side
  "Returns a new player map with the player's :side switched"
  [player]
  (-> player
      (update-in [:side] #(if (= % "Corp")
                            "Runner"
                            "Corp"))
      (dissoc :deck)))

(defn allowed-in-game [user game]
  true)

(defn handle-ws-connect [{:keys [client-id] :as msg}]
  (println client-id " connected")
  (ws/send! client-id [:games/list (map game-public-view @all-games)]))

(defn handle-ws-close [{:keys [client-id] :as msg}]
  (println client-id " disconnected")
  ;; this can be improved somehow
  (when-let [game (game-for-client client-id)]
    (remove-user client-id (:gameid game))))

(defn handle-lobby-create
  [{{{:keys [username emailhash] :as user} :user} :ring-req
    client-id                                     :client-id
    {:keys [title allowspectator spectatorhands password room side options]} :?data :as event}]
  (let [gameid (java.util.UUID/randomUUID)
        game {:date           (java.util.Date.)
              :gameid         gameid
              :title          title
              :allowspectator allowspectator
              :spectatorhands spectatorhands
              :mutespectators false
              :password       (when (not-empty password) (bcrypt/encrypt password))
              :room           room
              :players        [{:user    user
                                :id      client-id
                                :side    side
                                :options options}]
              :spectators     []}]
    (swap! all-games assoc gameid game)
    (swap! client-gameids assoc client-id gameid)
    (ws/send! client-id [:lobby/select {:gameid gameid}])
    (refresh-lobby :create gameid)))

(defn handle-lobby-leave
  [{{{:keys [username emailhash] :as user} :user} :ring-req
    client-id                                     :client-id
    gameid                                        :?data}]
  (when (player-or-spectator client-id gameid)
    (remove-user client-id gameid)
    (ws/broadcast-to! (lobby-clients gameid)
                      :lobby/message
                      {:user "__system__"
                       :text (str username " left the game.")})))

(defn handle-lobby-say
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [msg gameid]}                :?data}]
  (when (player-or-spectator client-id gameid)
    (let [game (get @all-games gameid)]
      (ws/broadcast-to!
        (map :id (concat (:players game) (:spectators game)))
        :lobby/message
        {:user user
         :text msg}))))

(defn handle-swap-sides
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    gameid                              :?data}]
  (let [game (get @all-games gameid)
        fplayer (first (:players game))]
    (when (= (:id fplayer) client-id)
      (swap! all-games update-in [gameid :players] (partial mapv swap-side))
      (refresh-lobby :update gameid)
      (ws/broadcast-to! (lobby-clients gameid)
                        :games/diff
                        {:diff {:update {gameid (game-public-view (get @all-games gameid))}}}))))

(defn handle-lobby-join
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid password options]}   :?data
    reply-fn                            :?reply-fn
    :as                                 msg}]
  (if-let [{game-password :password :as game} (@all-games gameid)]
    (when (and user game (allowed-in-game game user))
      (if (or (empty? game-password)
              (bcrypt/check password game-password))
        (do (join-game user client-id gameid)
            (ws/broadcast-to! (lobby-clients gameid)
                              :lobby/message
                              {:user         "__system__"
                               :notification "ting"
                               :text         (str username " joined the game.")})
            (ws/send! client-id [:lobby/select {:gameid gameid}])
            (when reply-fn (reply-fn 200)))
        (when reply-fn (reply-fn 403))))
    (when reply-fn (reply-fn 404))))

(defn handle-lobby-watch
  "Handles a watch command when a game has not yet started."
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [gameid password options]}   :?data
    reply-fn                            :?reply-fn}]
  (prn "watch lobby")
  (if-let [{game-password :password state :state started :started :as game}
           (@all-games gameid)]
    (when (and user game (allowed-in-game game user))
      (if started
        false                                               ; don't handle this message, let game/handle-game-watch.
        (if (or (empty? game-password)
                (bcrypt/check password game-password))
          (do (spectate-game user client-id gameid)

              (ws/broadcast-to! (lobby-clients gameid)
                                :lobby/message
                                {:user         "__system__"
                                 :notification "ting"
                                 :text         (str username " joined the game as a spectator.")})
              (ws/send! client-id [:lobby/select {:gameid gameid :started started}])
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
    deck                                :?data}]
  (let [game (game-for-client client-id)
        fplayer (first (:players game))
        gameid (:gameid game)]
    (when (player? client-id gameid)
      (swap! all-games update-in [gameid :players
                              (if (= client-id (:id fplayer)) 0 1)]
             (fn [p] (assoc p :deck deck)))
      (ws/broadcast-to! (lobby-clients gameid)
                        :games/diff
                        {:diff {:update {gameid (game-public-view (get @all-games gameid))}}}))))


(ws/register-ws-handlers!
  :chsk/uidport-open handle-ws-connect
  :chsk/uidport-close handle-ws-close
  :lobby/create handle-lobby-create
  :lobby/leave handle-lobby-leave
  :lobby/join handle-lobby-join
  :lobby/watch handle-lobby-watch
  :lobby/say handle-lobby-say
  :lobby/swap handle-swap-sides
  :lobby/deck handle-select-deck)