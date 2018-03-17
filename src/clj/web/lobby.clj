(ns web.lobby
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response tick remove-once]]
            [web.ws :as ws]
            [web.stats :as stats]
            [game.main]
            [game.core :as core]
            [crypto.password.bcrypt :as bcrypt]
            [game.main :as main]
            [monger.collection :as mc]
            [jinteki.cards :refer [all-cards]]
            [jinteki.decks :as decks]
            [clj-time.core :as t])
  (:import org.bson.types.ObjectId))

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

(defn user-public-view
  "Strips private server information from a player map."
  [started? player]
  (as-> player p
        (dissoc p :ws-id)
        (if-let [{:keys [_id] :as deck} (:deck p)]
          (assoc p :deck (select-keys (assoc deck :_id (str _id))
                                      (if started?
                                        [:_id :status :name :identity]
                                        [:_id :status :name])))
          p)))

(defn game-public-view
  "Strips private server information from a game map, preparing to send the game to clients."
  [{:keys [started] :as game}]
  (-> game
      (dissoc :state :last-update)
      (update-in [:players] #(map (partial user-public-view started) %))
      (update-in [:original-players] #(map (partial user-public-view started) %))
      (update-in [:ending-players] #(map (partial user-public-view started) %))
      (update-in [:spectators] #(map (partial user-public-view started) %))))

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
             (game-public-view (game-for-id gameid)))))

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
    (some #(when (= client-id (:ws-id %)) %) (:spectators game))))

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

  (refresh-lobby :delete gameid)
  (swap! all-games dissoc gameid)
  (swap! old-states dissoc gameid))


(defn clear-inactive-lobbies
  "Called by a background thread to close lobbies that are inactive for some number of seconds."
  [time-inactive]
  (doseq [{:keys [gameid last-update] :as game} (vals @all-games)]
    (when (and gameid (t/after? (t/now) (t/plus last-update (t/seconds time-inactive))))
      (close-lobby game))))

(defn remove-user
  "Removes the given client-id from the given gameid, whether it is a player or a spectator.
  Deletes the game from the lobby if all players have left."
  [client-id gameid]
  (when-let [{:keys [players started state] :as game} (game-for-id gameid)]
    (cond (player? client-id gameid)
          (swap! all-games update-in [gameid :players] #(remove-once (fn [p] (not= client-id (:ws-id p))) %))

          (spectator? client-id gameid)
          (swap! all-games update-in [gameid :spectators] #(remove-once (fn [p] (not= client-id (:ws-id p))) %)))

    ;; update ending-players when someone drops to credit a completion properly.  Not if game is over.
    ; TODO add other player back in if other player rejoins

    (when state
      (let [winner (:winning-user @state)]
        (when (and (= 1 (count players)) started (not winner))
          (swap! all-games assoc-in [gameid :ending-players] players))))

    (let [{:keys [players] :as game} (game-for-id gameid)]
      (swap! client-gameids dissoc client-id)

      (if (empty? players)
        (close-lobby game)
        (refresh-lobby :update gameid)))))

(defn lobby-clients
  "Returns a seq of all client-ids playing or spectating a gameid."
  [gameid]
  (let [game (game-for-id gameid)]
    (map :ws-id (concat (:players game) (:spectators game)))))

(defn join-game
  "Adds the given user as a player in the given gameid."
  [{options :options :as user} client-id gameid]
  (let [{players :players :as game} (game-for-id gameid)]
    (when (< (count players) 2)
      (let [{side :side :as fplayer} (first players)
            new-side (if (= "Corp" side) "Runner" "Corp")
            new-player {:user    user
                        :ws-id   client-id
                        :side    new-side
                        :options options}]
        (swap! all-games update-in [gameid :players] #(conj % new-player))
        (swap! client-gameids assoc client-id gameid)
        (refresh-lobby :update gameid)
        new-player))))

(defn spectate-game
  "Adds the given user as a spectator in the given gameid"
  [user client-id gameid]
  (when-let [{:keys [started spectators] :as game} (game-for-id gameid)]
    (swap! all-games update-in [gameid :spectators]
           #(conj % {:user  user
                     :ws-id client-id}))
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

(defn blocked-users
  [{:keys [players] :as game}]
  (mapcat #(get-in % [:user :options :blocked-users]) players))

(defn allowed-in-game [{:keys [username]} game]
  (not (some #(= username (:username %)) (blocked-users game))))

(defn handle-ws-connect [{:keys [client-id] :as msg}]
  (ws/send! client-id [:games/list (mapv game-public-view (vals @all-games))]))

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
              :mute-spectators false
              :password       (when (not-empty password) (bcrypt/encrypt password))
              :room           room
              :players        [{:user    user
                                :ws-id      client-id
                                :side    side
                                :options options}]
              :spectators     []
              :last-update    (t/now)}]
    (swap! all-games assoc gameid game)
    (swap! client-gameids assoc client-id gameid)
    (ws/send! client-id [:lobby/select {:gameid gameid}])
    (refresh-lobby :create gameid)))

(defn handle-lobby-leave
  [{{{:keys [username emailhash] :as user} :user} :ring-req
    client-id                                     :client-id}]
  (when-let [{gameid :gameid} (game-for-client client-id)]
    (when (player-or-spectator client-id gameid)
      (remove-user client-id gameid)
      (ws/broadcast-to! (lobby-clients gameid)
                        :lobby/message
                        {:user "__system__"
                         :text (str username " left the game.")}))))

(defn handle-lobby-say
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    {:keys [msg gameid]}                :?data}]
  (when (player-or-spectator client-id gameid)
    (let [game (game-for-id gameid)]
      (ws/broadcast-to!
        (map :ws-id (concat (:players game) (:spectators game)))
        :lobby/message
        {:user user
         :text msg}))))

(defn handle-swap-sides
  [{{{:keys [username] :as user} :user} :ring-req
    client-id                           :client-id
    gameid                              :?data}]
  (let [game (game-for-id gameid)
        fplayer (first (:players game))]
    (when (= (:ws-id fplayer) client-id)
      (swap! all-games update-in [gameid :players] (partial mapv swap-side))
      (refresh-lobby :update gameid)
      (ws/broadcast-to! (lobby-clients gameid)
                        :games/diff
                        {:diff {:update {gameid (game-public-view (game-for-id gameid))}}}))))

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
    deck-id                             :?data}]
  (let [game (game-for-client client-id)
        fplayer (first (:players game))
        gameid (:gameid game)

        map-card (fn [c] (update-in c [:card] @all-cards))
        deck (as-> (mc/find-one-as-map db "decks" {:_id (object-id deck-id) :username username}) d
                   (update-in d [:cards] #(mapv map-card %))
                   (update-in d [:identity] #(@all-cards (:title %)))
                   (assoc d :status (decks/calculate-deck-status d)))]
    (when (and deck (player? client-id gameid))
      (swap! all-games update-in [gameid :players
                              (if (= client-id (:ws-id fplayer)) 0 1)]
             (fn [p] (assoc p :deck deck)))
      (ws/broadcast-to! (lobby-clients gameid)
                        :games/diff
                        {:diff {:update {gameid (game-public-view (game-for-id gameid))}}}))))


(ws/register-ws-handlers!
  :chsk/uidport-open handle-ws-connect
  :lobby/create handle-lobby-create
  :lobby/leave handle-lobby-leave
  :lobby/join handle-lobby-join
  :lobby/watch handle-lobby-watch
  :lobby/say handle-lobby-say
  :lobby/swap handle-swap-sides
  :lobby/deck handle-select-deck)
