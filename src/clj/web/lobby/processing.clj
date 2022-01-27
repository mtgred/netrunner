(ns web.lobby.processing
  (:refer-clojure :exclude [random-uuid])
  (:require
   [cond-plus.core :refer [cond+]]
   [cljc.java-time.instant :as inst]
   [crypto.password.bcrypt :as bcrypt]
   [game.core.set-up :as set-up]
   [executor.core :as executor]
   [executor.loggers :refer [log]]
   [game.core.say :refer [make-message make-system-message]]
   [game.main :as main]
   [game.utils :refer [server-card]]
   [jinteki.utils :refer [side-from-str superuser?]]
   [jinteki.validator :as validator]
   [medley.core :refer [find-first random-uuid]]
   [monger.collection :as mc]
   [web.lobby.utils :refer [filter-lobby-list first-player?
                            get-players-and-spectators uid->lobby uid->user
                            player? spectator?
                            uid-player->lobby uid-original-player->lobby]]
   [web.mongodb :as mongodb]))

(executor/reg-event-fx
  :chsk/ws-ping
  (fn [_ _]))

(executor/reg-cofx
  :inst/now
  (fn inst-now [cofx]
    (assoc cofx :inst/now (inst/now))))

(executor/reg-cofx
  :lobby/new-gameid
  (fn lobby-new-gameid [cofx]
    (assoc cofx :lobby/new-gameid (random-uuid))))

(def set-last-update
  "After interceptor that modifies the :last-update value of the lobby"
  (executor/enrich
    (fn set-last-update
      [{lobbies :lobbies :as db} {uid :uid}]
      (when (uid-player->lobby lobbies uid)
        (let [lobby (uid->lobby lobbies uid)
              gameid (:gameid lobby)]
          (assoc-in db [:lobbies gameid :last-update] (inst/now)))))))

(defn make-options-for-lobby
  [cofx data]
  {:user (uid->user (:db cofx) (:uid data))
   :uid (:uid data)
   :options (-> (:?data data)
                (assoc :now (:inst/now cofx))
                (assoc :gameid (:lobby/new-gameid cofx)))})

(defn create-new-lobby
  [{uid :uid
    user :user
    {:keys [gameid now
            allow-spectator api-access format mute-spectators password room save-replay
            side spectatorhands timer title]} :options}]
  (let [player {:user user
                :uid uid
                :side side}]
    {:gameid gameid
     :date now
     :last-update now
     :players [player]
     :spectators []
     :messages [(make-system-message
                  (str (:username user) " has created the game."))]
     ;; options
     :allow-spectator allow-spectator
     :api-access api-access
     :format format
     :mute-spectators mute-spectators
     :password (when (not-empty password) (bcrypt/encrypt password))
     :room room
     :save-replay save-replay
     :spectatorhands spectatorhands
     :timer timer
     :title title}))

(defn register-new-lobby
  "Convert [:lobby/create data] into effect handler calls.
  Doesn't do anything if the player is in a game.
  Relies on the :inst/now and :lobby/new-gameid cofx to be injected."
  [{{lobbies :lobbies :as db} :db
    new-gameid :lobby/new-gameid :as cofx}
   {uid :uid :as data}]
  (when-not (uid-player->lobby lobbies uid)
    (let [options (make-options-for-lobby cofx data)
          lobby (create-new-lobby options)
          db (assoc-in db [:lobbies new-gameid] lobby)]
      {:db db
       :fx [[:lobby/state lobby]
            [:lobby/broadcast-list db]]})))


(executor/reg-event-fx
  :lobby/create
  [executor/unwrap
   (executor/inject-cofx :inst/now)
   (executor/inject-cofx :lobby/new-gameid)
   set-last-update]
  #'register-new-lobby)

(defn send-lobby-list
  "Converts [:lobby/list] into effect handler calls.
  Doesn't do anything if the user doesn't have a uid.
  Uses :lobby/broadcast-list but only passes a single element in the list."
  [{{lobbies :lobbies :as db} :db} {uid :uid}]
  (when uid
    {:fx [(if-let [lobby (uid->lobby lobbies uid)]
            [:lobby/state lobby]
            [:lobby/clear uid])
          [:lobby/broadcast-list
           (assoc db :users {uid (uid->user db uid)})]]}))

(executor/reg-event-fx
  :lobby/list
  [executor/unwrap set-last-update]
  #'send-lobby-list)

(defn send-message [lobby message]
  (update lobby :messages conj message))

(defn leave-lobby
  "Converts [:lobby/leave] into effect handler calls.
  Doesn't do anything if the user isn't in a game.
  Uses :lobby/broadcast-list but only passes a single element in the list."
  [{lobbies :lobbies :as db} lobby uid]
  (let [user (uid->user db uid)
        leave-message (make-system-message
                        (str (:username user) " left the game."))
        players (remove #(= uid (:uid %)) (:players lobby))
        spectators (remove #(= uid (:uid %)) (:spectators lobby))
        gameid (:gameid lobby)
        lobbies (if (pos? (count players))
                  (-> lobbies
                      (update gameid send-message leave-message)
                      (assoc-in [gameid :players] players)
                      (assoc-in [gameid :spectators] spectators))
                  (dissoc lobbies gameid))]
    (assoc db :lobbies lobbies)))

(defn handle-leave-lobby
  [{{lobbies :lobbies :as db} :db}
   {{system-db :system/db} :ring-req
    uid :uid
    reply-fn :?reply-fn}]
  (if-let [lobby (uid-player->lobby lobbies uid)]
    (let [
          db (leave-lobby db lobby uid)
          updated-lobby (get-in db [:lobbies (:gameid lobby)])
          still-active? (->> (:players updated-lobby)
                             (count)
                             (pos?))
          fx (cond-> []
               ;; either remove the user and update the participant's state
               ;; or close the whole damn thing
               still-active?
               (conj [:game/remove-user [updated-lobby uid]]
                     [:lobby/state updated-lobby]
                     [:lobby/clear uid])
               (not still-active?)
               (conj [:lobby/close-lobby [system-db lobby]])
               true
               (conj [:lobby/broadcast-list db]
                     [:ws/reply-fn [reply-fn true]]))]
      {:db db
       :fx fx})
    {:fx [[:lobby/clear uid]
          [:lobby/broadcast-list db]
          [:ws/reply-fn [reply-fn false]]]}))


(executor/reg-event-fx
  :lobby/leave
  [executor/unwrap set-last-update]
  #'handle-leave-lobby)

(defn update-deck-for-player-in-lobby [players uid deck]
  (mapv (fn [p] (if (= uid (:uid p))
                  (assoc p :deck deck)
                  p))
        players))

(defn valid-deck-for-lobby? [lobby deck]
  (and (:identity deck)
       (or (= "casual" (:format lobby))
           (validator/legal-deck? deck (:format lobby)))))

(defn prepare-deck
  "Queries the db for the deck and then converts it to the right format"
  [deck]
  (let [identity-card (server-card (-> deck :identity :title) false)
        cards (->> (:cards deck)
                   (keep #(when-let [card (server-card (:card %) false)]
                            (assoc % :card card))))
        deck (assoc deck
                    :identity identity-card
                    :cards cards)
        status (validator/calculate-deck-status deck)]
    (-> deck
        (assoc :status status)
        (dissoc :parsed?))))

(defn handle-select-deck
  [{{lobbies :lobbies :as db} :db deck :lobby/deck}
   {uid :uid reply-fn :?reply-fn}]
  (let [lobby (uid-player->lobby lobbies uid)
        deck (prepare-deck deck)]
    (if (and lobby (not-empty deck) (valid-deck-for-lobby? lobby deck))
      (let [gameid (:gameid lobby)
            db (update-in db [:lobbies gameid :players] update-deck-for-player-in-lobby uid deck)
            lobby (get-in db [:lobbies gameid])]
        {:db db
         :fx [[:lobby/state lobby]
              [:lobby/broadcast-list db]
              [:ws/reply-fn [reply-fn true]]]})
      {:ws/reply-fn [reply-fn false]})))

(defn find-deck! [db deck-id {username :username}]
  (mc/find-one-as-map db :decks {:_id (mongodb/->object-id deck-id)
                                 :username username}))

(executor/reg-cofx
  :lobby/find-deck
  (fn lobby-find-deck [cofx]
    (let [{{{system-db :system/db} :ring-req
            uid :uid
            {deck-id :deck-id} :?data} :event} cofx
          user (uid->user (:db cofx) uid)
          deck (find-deck! system-db deck-id user)]
      (assoc cofx :lobby/deck deck))))

(executor/reg-event-fx
  :lobby/deck
  [executor/unwrap set-last-update (executor/inject-cofx :lobby/find-deck)]
  #'handle-select-deck)

(defn handle-lobby-say
  [{{lobbies :lobbies :as db} :db}
   {uid :uid
    {:keys [gameid text]} :?data}]
  (assert (string? text) "Message must be a string")
  (when (uid-player->lobby lobbies uid)
    (let [user (uid->user db uid)
          message (make-message {:user user :text text})
          lobby (-> (get lobbies gameid)
                    (send-message message))
          lobbies (assoc lobbies gameid lobby)]
      {:db (assoc db :lobbies lobbies)
       :fx [[:lobby/state lobby]]})))

(executor/reg-event-fx
  :lobby/say
  [executor/unwrap set-last-update]
  #'handle-lobby-say)

(defn check-password [lobby user password]
  (or (empty? (:password lobby))
      (superuser? user)
      (bcrypt/check password (:password lobby))))

(defn allowed-in-lobby
  [user lobby]
  (or (superuser? user)
      (seq (filter-lobby-list [lobby] user))))

(defn already-in-game?
  "Checks if a user with the given username is already in the game"
  [{:keys [username]} lobby]
  (some #(= username (get-in % [:user :username])) (get-players-and-spectators lobby)))

(defn determine-player-side
  "Determines the side of a player based on their side and a requested side"
  [player request-side]
  (let [side (:side player)]
    (if (and (some? side) (not= side "Any Side"))
      side
      (case request-side
        "Corp" "Runner"
        "Runner" "Corp"
        ; else
        (rand-nth ["Corp" "Runner"])))))

(defn insert-user-as-player [lobby uid user request-side]
  (let [existing-player (-> lobby :players first)
        existing-player-side (determine-player-side existing-player request-side)
        existing-player (assoc existing-player :side existing-player-side)
        user-side (if (= "Corp" existing-player-side) "Runner" "Corp")
        user-as-player {:uid uid
                        :user user
                        :side user-side}]
    (assoc lobby :players [existing-player user-as-player])))

(defn join-lobby
  "Updates db"
  [{{lobbies :lobbies :as db} :db}
   {uid :uid
    {:keys [gameid password request-side]} :?data}]
  (let [lobby (get lobbies gameid)
        user (uid->user db uid)]
    (when (and user lobby
               (not (already-in-game? user lobby))
               (allowed-in-lobby user lobby)
               (check-password lobby user password))
      (let [message (make-system-message (str (:username user) " joined the game."))
            lobby (-> lobby
                      (insert-user-as-player uid user request-side)
                      (send-message message))]
        (assoc-in db [:lobbies gameid] lobby)))))

(defn handle-join-lobby
  [cofx data]
  (if-let [{lobbies :lobbies :as db} (join-lobby cofx data)]
    (let [{uid :uid
           {gameid :gameid} :?data
           reply-fn :?reply-fn} data
          lobby (get lobbies gameid)
          user (uid->user db uid)
          player (player? uid lobby)
          side (side-from-str (:side player))]
      {:db db
       :fx [(when (and lobby player side)
              [:game/update-state [#(swap! % assoc-in [side :user] user) lobby]])
            [:lobby/state lobby]
            [:lobby/broadcast-list db]
            [:lobby/ting lobby]
            [:ws/reply-fn [reply-fn true]]]})
    {:ws/reply-fn [(:?reply-fn data) false]}))

(executor/reg-event-fx
  :lobby/join
  [executor/unwrap set-last-update]
  #'handle-join-lobby)

(defn swap-side
  "Returns a new player map with the player's :side switched"
  [player]
  (-> player
      (update :side #(if (= % "Corp") "Runner" "Corp"))
      (dissoc :deck)))

(defn change-side
  "Returns a new player map with the player's :side set to a new side"
  [player side]
  (-> player
      (assoc :side side)
      (dissoc :deck)))

(defn update-sides [lobby uid side]
  (let [first-player (first (:players lobby))]
    (cond
      (not= (:uid first-player) uid) lobby
      (some? side) (update-in lobby [:players 0] change-side side)
      :else (update lobby :players #(mapv swap-side %)))))

(defn swap-text [players]
  (let [[player1 player2] (mapv swap-side players)
        player1-username (-> player1 :user :username)
        player2-username (-> player2 :user :username)]
    (str player1-username " has swapped sides. "
         player1-username " is now " (:side player1) ". "
         player2-username " is now " (:side player2) ".")))

(defn handle-swap
  [{{lobbies :lobbies :as db} :db}
   {uid :uid {:keys [gameid side]} :?data}]
  (let [lobby (uid-player->lobby lobbies uid)]
    (when (and lobby (= gameid (:gameid lobby)) (first-player? uid lobby))
      (let [user (uid->user db uid)
            message (make-message {:user user
                                   :text (swap-text (:players lobby))})
            lobby (-> lobby
                      (update-sides uid side)
                      (send-message message))
            db (assoc-in db [:lobbies gameid] lobby)]
        {:db db
         :fx [[:lobby/state lobby]
              [:lobby/broadcast-list db]]}))))

(executor/reg-event-fx
  :lobby/swap
  [executor/unwrap set-last-update]
  #'handle-swap)

(defn handle-rename
  [{{lobbies :lobbies :as db} :db now :inst/now}
   {{system-db :system/db} :ring-req
    uid :uid
    {gameid :gameid} :?data}]
  (let [user (uid->user db uid)
        lobby (get lobbies gameid)]
    (when (and lobby (superuser? user))
      (let [player-name (or (-> lobby :original-players first :user :username)
                            (-> lobby :players first :user :username))
            bad-name (:title lobby)
            lobby (assoc lobby :title (str player-name "'s game"))
            db (assoc-in db [:lobbies gameid] lobby)
            mod-action {:moderator (:username user)
                        :action :rename-game
                        :game-name bad-name
                        :first-player player-name
                        :date now}]
        {:db db
         :fx [[:lobby/state lobby]
              [:lobby/broadcast-list db]
              [:db/mod-action [system-db mod-action]]]}))))

(executor/reg-event-fx
  :lobby/rename-game
  [executor/unwrap (executor/inject-cofx :inst/now) set-last-update]
  #'handle-rename)

(defn handle-delete-game
  [{{lobbies :lobbies :as db} :db now :inst/now}
   {{system-db :system/db} :ring-req
    uid :uid
    {gameid :gameid} :?data}]
  (let [user (uid->user db uid)
        lobby (get lobbies gameid)]
    (when (and lobby (superuser? user))
      (let [player-name (or (-> lobby :original-players first :user :username)
                            (-> lobby :players first :user :username))
            mod-action {:moderator (:username user)
                        :action :delete-game
                        :game-name (:title lobby)
                        :first-player player-name
                        :date now}
            db (update db :lobbies dissoc gameid)
            fx (concat [[:lobby/close-lobby [system-db lobby]]
                        [:lobby/broadcast-list db]
                        [:db/mod-action [system-db mod-action]]]
                       (for [uid (keep :uid (get-players-and-spectators lobby))]
                         [:lobby/clear uid]))]
        {:db db
         :fx fx}))))

(executor/reg-event-fx
  :lobby/delete-game
  [executor/unwrap (executor/inject-cofx :inst/now)]
  #'handle-delete-game)

(defn insert-user-as-spectator
  [lobby uid user]
  (update lobby :spectators conj {:uid uid
                                  :user user}))

(defn watch-lobby
  [db lobby uid]
  (let [user (uid->user db uid)
        message (make-system-message (str (:username user) " joined the game as a spectator."))
        lobby (-> lobby
                  (insert-user-as-spectator uid user)
                  (send-message message))]
    (assoc-in db [:lobbies (:gameid lobby)] lobby)))

(defn handle-watch-lobby
  [{{lobbies :lobbies :as db} :db}
   {uid :uid
    {:keys [gameid password]} :?data
    reply-fn :?reply-fn}]
  (let [lobby (get lobbies gameid)
        user (uid->user db uid)
        correct-password? (check-password lobby user password)]
    (when (and user lobby)
      (cond
        (and (not (already-in-game? user lobby))
             (allowed-in-lobby user lobby)
             correct-password?)
        (let [db (watch-lobby db lobby uid)
              lobby (get (:lobbies db) gameid)]
          {:db db
           :fx [[:lobby/state lobby]
                [:lobby/ting lobby]
                [:ws/reply-fn [reply-fn 200]]]})
        (false? correct-password?)
        {:ws/reply-fn [reply-fn 403]}
        :else
        {:ws/reply-fn [reply-fn 404]}))))

(executor/reg-event-fx
  :lobby/watch
  [executor/unwrap (executor/inject-cofx :inst/now) set-last-update]
  #'handle-watch-lobby)

(defn- is-starter-deck?
  [player]
  (let [id (get-in player [:deck :identity :title])
        card-cnt (reduce + (map :qty (get-in player [:deck :cards])))]
    (or (and (= id "The Syndicate: Profit over Principle")
             (= card-cnt 34))
        (and (= id "The Catalyst: Convention Breaker")
             (= card-cnt 30)))))

(defn- check-for-starter-decks
  "Starter Decks can require 6 or 7 agenda points"
  [game]
  (if (and (= (:format game) "system-gateway")
           (every? is-starter-deck? (:players game)))
    (do
      (swap! (:state game) assoc-in [:runner :agenda-point-req] 6)
      (swap! (:state game) assoc-in [:corp :agenda-point-req] 6)
      game)
    game))

(defn summarize-deck [player]
  (-> player
      (update :deck select-keys [:_id :identity :name :hash])
      (update-in [:deck :_id] str)
      (update-in [:deck :identity] select-keys [:title :faction])))

(defn start-game-for-lobby
  [lobby now]
  (let [players (:players lobby)]
    (-> lobby
        (assoc :started true
               :original-players players
               :ending-players players
               :start-date now
               :last-update now
               :state (set-up/init-game lobby))
        (check-for-starter-decks)
        (update :players #(mapv summarize-deck %)))))

(defn handle-start-game
  [{{lobbies :lobbies :as db} :db now :inst/now}
   {{system-db :system/db} :ring-req
    uid :uid
    {gameid :gameid} :?data}]
  (let [lobby (uid-player->lobby lobbies uid)]
    (when (and lobby (= gameid (:gameid lobby)) (first-player? uid lobby) (not (:started lobby)))
      (let [lobby (start-game-for-lobby lobby now)
            db (assoc-in db [:lobbies gameid] lobby)]
        {:db db
         :fx [[:stats/game-started [system-db lobby]]
              [:lobby/state lobby]
              [:lobby/broadcast-list db]
              [:game/start lobby]]}))))

(executor/reg-event-fx
  :game/start
  [executor/unwrap (executor/inject-cofx :inst/now) set-last-update]
  #'handle-start-game)

(defn handle-leave-game
  [{{lobbies :lobbies :as db} :db :as cofx}
   {{system-db :system/db} :ring-req
    uid :uid
    {gameid :gameid} :?data :as event}]
  (let [lobby (uid-player->lobby lobbies uid)
        user (uid->user db uid)]
    (if (and lobby (= gameid (:gameid lobby)) (:started lobby))
      (let [db (leave-lobby db lobby uid)
            updated-lobby (get (:lobbies db) gameid)
            still-active? (->> (:players updated-lobby)
                               (count)
                               (pos?))
            player (player? uid updated-lobby)
            side (side-from-str (:side player))
            fx (cond-> []
                 still-active?
                 (conj [:game/update-state [main/handle-notification updated-lobby (str (:username user) " has left the game.")]]
                       [:game/update-state [#(swap! % update side dissoc :user) updated-lobby]]
                       [:lobby/clear uid])
                 (not still-active?)
                 (conj [:lobby/close-lobby [system-db lobby]])
                 true
                 (conj [:lobby/state updated-lobby]
                       [:lobby/broadcast-list db]))]
        {:db db
         :fx fx})
      (send-lobby-list cofx event))))

(executor/reg-event-fx
  :game/leave
  [executor/unwrap set-last-update]
  #'handle-leave-game)

(defn handle-rejoin-game
  [{{lobbies :lobbies} :db :as cofx}
   {uid :uid :as data}]
  (let [lobby (uid-original-player->lobby lobbies uid)
        original-player (find-first #(= uid (:uid %)) (:original-players lobby))]
    (when (and (:started lobby)
               original-player
               (< (count (remove #(= uid (:uid %)) (:players lobby))) 2))
      (let [data (assoc-in data [:?data :request-side] "Any Side")
            db (join-lobby cofx data)
            lobby (uid-player->lobby (:lobbies db) uid)
            user (uid->user db uid)]
        {:db db
         :fx [[:lobby/state lobby]
              [:lobby/broadcast-list db]
              [:game/update-state [main/handle-notification lobby (str (:username user) " has left the game.")]]
              [:game/start lobby]]}))))

(executor/reg-event-fx
  :game/rejoin
  [executor/unwrap set-last-update]
  #'handle-rejoin-game)

(defn handle-concede-game
  [{{lobbies :lobbies} :db}
   {uid :uid}]
  (let [lobby (uid-player->lobby lobbies uid)
        player (player? uid lobby)
        side (side-from-str (:side player))]
    (when (and lobby player)
      {:fx [[:game/update-state [main/handle-concede lobby side]]]})))

(executor/reg-event-fx
  :game/concede
  [executor/unwrap set-last-update]
  #'handle-concede-game)

(defn handle-game-action
  [{{lobbies :lobbies} :db}
   {uid :uid {:keys [command args]} :?data}]
  (let [lobby (uid-player->lobby lobbies uid)
        state (:state lobby)
        player (player? uid lobby)
        side (side-from-str (:side player))
        spectator (spectator? uid lobby)]
    (cond
      (and state player)
      {:fx [[:game/update-state [main/handle-action lobby side command args]]]}
      (and (not spectator) (not= command "toast"))
      (let [ex (ex-info "handle-game-action unknown state or side"
                        {:gameid (:gameid lobby)
                         :uid uid
                         :players (map #(select-keys % [:uid :side]) (:players lobby))
                         :spectators (map #(select-keys % [:uid]) (:spectators lobby))
                         :command command
                         :args args})]
        (log :error ex)
        {:fx [[:game/error uid]]}))))

(executor/reg-event-fx
  :game/action
  [executor/unwrap set-last-update]
  #'handle-game-action)

(defn handle-resync
  [{{lobbies :lobbies :as db} :db}
   {uid :uid}]
  (let [lobby (uid-player->lobby lobbies uid)
        state (:state lobby)]
    (if state
      {:fx [[:lobby/state lobby]
            [:lobby/broadcast-list db]
            [:game/start lobby]]}
      {:game/error uid})))

(executor/reg-event-fx
  :game/resync
  [executor/unwrap]
  #'handle-resync)

(defn handle-watch-game
  [{{lobbies :lobbies :as db} :db}
   {uid :uid
    {:keys [gameid password]} :?data
    reply-fn :?reply-fn}]
  (let [lobby (get lobbies gameid)
        user (uid->user db uid)
        correct-password? (check-password lobby user password)]
    (when (and user lobby)
      (cond
        (and (not (already-in-game? user lobby))
             (allowed-in-lobby user lobby)
             correct-password?)
        (let [db (watch-lobby db lobby uid)
              lobby (get (:lobbies db) gameid)
              message (make-system-message (str (:username user) " joined the game as a spectator."))]
          {:db db
           :fx [[:game/update-state [main/handle-notification lobby message]]
                [:lobby/ting lobby]
                [:lobby/state lobby]
                [:lobby/broadcast-list db]
                [:game/start lobby]
                [:ws/reply-fn [reply-fn 200]]]})
        (false? correct-password?)
        {:ws/reply-fn [reply-fn 403]}
        :else
        {:ws/reply-fn [reply-fn 404]}))))

(executor/reg-event-fx
  :game/watch
  [executor/unwrap set-last-update]
  #'handle-watch-game)

(defn handle-mute-spectators
  [{{lobbies :lobbies :as db} :db}
   {uid :uid
    {gameid :gameid} :?data}]
  (let [lobby (uid-player->lobby lobbies uid)
        user (uid->user db uid)]
    (when (and lobby (= gameid (:gameid lobby)))
      (let [db (update-in db [:lobbies gameid :mute-spectators] not)
            lobby (get-in db [:lobbies gameid])
            message (str (:username user) " "
                         (if (:mute-spectators lobby) "unmuted" "muted")
                         " spectators.")]
        {:db db
         :fx [[:game/update-state [main/handle-notification lobby message]]
              [:lobby/state lobby]]}))))

(executor/reg-event-fx
  :game/mute-spectators
  [executor/unwrap set-last-update]
  #'handle-mute-spectators)

(defn handle-game-say
  [{{lobbies :lobbies :as db} :db}
   {uid :uid
    {:keys [gameid msg]} :?data}]
  (let [{:keys [state mute-spectators] :as lobby} (uid-player->lobby lobbies uid)
        user (uid->user db uid)
        side (cond+
               [(player? uid lobby) :> #(side-from-str (:side %))]
               [(and (not mute-spectators) (spectator? uid lobby)) :spectator])]
    (when (and lobby (= gameid (:gameid lobby)) state side)
      {:game/update-state [main/handle-say lobby side user msg]})))

(executor/reg-event-fx
  :game/say
  [executor/unwrap set-last-update]
  #'handle-game-say)

(defn handle-typing
  [{{lobbies :lobbies} :db}
   {uid :uid
    {:keys [gameid typing]} :?data}]
  (let [lobby (uid-player->lobby lobbies uid)]
    (when (and lobby (= gameid (:gameid lobby)) (:state lobby))
      {:game/typing [lobby uid typing]})))

(executor/reg-event-fx
  :game/typing
  [executor/unwrap set-last-update]
  #'handle-typing)

(defn handle-uidport-close
  [{{lobbies :lobbies :as db} :db}
   {{system-db :system/db} :ring-req
    uid :uid
    reply-fn :?reply-fn}]
  (let [{:keys [started state] :as lobby} (uid-player->lobby lobbies uid)]
    (when (and state started)
      (let [db (leave-lobby db lobby uid)
            lobby (get (:lobbies db) (:gameid lobby))
            user (uid->user db uid)
            message (str (:username user) " has left the game.")
            still-active? (->> (get (:lobbies db) (:gameid lobby))
                               (:players)
                               (count)
                               (pos?))
            fx (cond-> [[:game/update-state [main/handle-notification lobby message]]]
                 (not still-active?)
                 (conj [:lobby/close-lobby [system-db lobby]])
                 true
                 (conj [:lobby/state lobby]
                       [:lobby/broadcast-list db]
                       [:ws/reply-fn [reply-fn true]]))]
        {:db db
         :fx fx}))))

(executor/reg-event-fx
  :chsk/uidport-close
  [executor/unwrap set-last-update]
  #'handle-uidport-close)
