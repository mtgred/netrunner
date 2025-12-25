(ns web.lobby
  (:require
   [clojure.core.async :refer [<! go timeout]]
   [cljc.java-time.instant :as inst]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [clojure.set :as set]
   [clojure.string :as str]
   [com.climate.claypoole :as cp]
   [crypto.password.bcrypt :as bcrypt]
   [game.core :as core]
   [game.utils :refer [server-card]]
   [jinteki.utils :refer [select-non-nil-keys side-from-str superuser? to?]]
   [jinteki.preconstructed :refer [all-matchups]]
   [jinteki.validator :as validator]
   [medley.core :refer [find-first]]
   [monger.collection :as mc]
   [time-literals.read-write :as read-write]
   [web.app-state :as app-state]
   [web.mongodb :as mongodb]
   [web.stats :as stats]
   [web.ws :as ws]
   [taoensso.encore :as enc]
   [taoensso.timbre :as timbre]))

(read-write/print-time-literals-clj!)

(defonce telemetry-buckets (agent {}))
(defn log-delay!
  [timestamp id]
  (let [now (inst/now)
        start timestamp
        key (or id :unknown)
        diff (duration/between start now)
        total-ms (quot (duration/get diff chrono/nanos) 1000000)
        create-or-update (fn [map]
                           (if (contains? map key)
                             (assoc map key (conj (key map) total-ms))
                             (assoc map key (seq [total-ms]))))]
    (send telemetry-buckets create-or-update)))
(defn fetch-delay-log!
  []
  (let [res @telemetry-buckets]
    (send telemetry-buckets (constantly {}))
    res))

;; Oracle guidance for active threads is ~cores+2
(defonce pool-size (+ 2 (.availableProcessors (Runtime/getRuntime))))

(defonce game-pools
  (letfn [(new-pool [x]
            {:pool (cp/threadpool 1 {:name (str "game-thread-" x)})
             :occupants (atom #{})})] ;; note that occupants is just for load balancing purposes
    (vec (map new-pool (range pool-size)))))

(defn pool-occupants-info [] (map #(deref (:occupants %)) game-pools))

(defn join-pool!
  "Returns one of the pools at random with the least occupants. Updates pool occupancy"
  [gameid]
  (let [pool (first (sort-by #(-> % :occupants deref count) (shuffle game-pools)))]
    (swap! (:occupants pool) conj gameid)
    pool))

(defn leave-pool!
  "Leaves a pool. This just decrements the occupants, so we can reassign it smartly"
  [pool gameid]
  (swap! (:occupants pool) disj gameid))

(def pool-cleaning-frequency (enc/ms :mins 30))
(defonce clean-pools
  (go (while true
        (<! (timeout pool-cleaning-frequency))
        (let [cleaned! (atom 0)
              lobby-ids (set (map :gameid (app-state/get-lobbies)))]
          (doseq [pool game-pools]
            (let [occupants @(:occupants pool)
                  stale (set/difference occupants lobby-ids)]
              (when (seq stale)
                (swap! cleaned! + (count stale))
                (swap! (:occupants pool) set/difference stale))))
          (if (pos? @cleaned!)
            (timbre/info (str "cleaned up" @cleaned! "stale pool occupants!"))
            (timbre/info "all pools are tidy!"))))))

(defonce lobby-pool (cp/threadpool 1 {:name "lobbies-thread"}))
(defmacro lobby-thread [& expr] `(cp/future lobby-pool ~@expr))
(defmacro game-thread [lobby & expr]
  "Note: if the lobby isn't actually real, or has been nulled somehow, executing on the lobby thread is safe"
  `(cp/future (get-in ~lobby [:pool :pool] lobby-pool) ~@expr))

(defmulti assign-tournament-properties identity)

(defn validate-precon
  [format client-precon client-gateway-type]
  (let [target (if (= format "system-gateway") client-gateway-type client-precon)
        precon (and target (keyword (str/lower-case target)))]
    (when (or (and (= format "system-gateway")
                   (contains? #{:beginner :intermediate} precon))
              (and (= (str format) "preconstructed")
                   (contains? all-matchups precon)))
      precon)))

(defn create-new-lobby
  [{uid :uid
    user :user
    {:keys [gameid now
            allow-spectator api-access format mute-spectators password room save-replay
            precon gateway-type side singleton spectatorhands timer title open-decklists description]
     :or {gameid (random-uuid)
          now (inst/now)}} :options}]
  (let [player {:user user
                :uid uid
                :side side}]
    {:gameid gameid
     :date now
     :last-update now
     :players [player]
     :spectators []
     :corp-spectators []
     :runner-spectators []
     :messages []
     :pool (join-pool! gameid)
     ;; options
     :precon (validate-precon format precon gateway-type)
     :open-decklists (or open-decklists (when (validate-precon format precon gateway-type) true))
     :allow-spectator allow-spectator
     :api-access api-access
     :format format
     :description description
     :mute-spectators mute-spectators
     :password (when (not-empty password) (bcrypt/encrypt password))
     :room room
     :save-replay save-replay
     :spectatorhands spectatorhands
     :singleton (when (some #{format} `("standard" "startup" "casual" "eternal")) singleton)
     :timer timer
     :title title}))

(defn get-players-and-spectators [lobby]
  (concat (:players lobby) (:spectators lobby)))

(defn lobby-ting [lobby]
  (for [user (get-players-and-spectators lobby)
        :let [uid (:uid user)]]
    [uid [:lobby/notification "ting"]]))

(defn send-lobby-ting [lobby]
  (when lobby
    (doseq [[uid ev] (lobby-ting lobby)]
      (when uid
        (ws/chsk-send! uid ev)))))

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
                       [:name :date :identity]
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
  (map (fn [p] (-> p
                   (update :user select-keys [:username :emailhash])
                   (select-keys [:user])))
       players))

(def lobby-keys
  [:allow-spectator
   :api-access
   :date
   :format
   :gameid
   :precon
   :messages
   :mute-spectators
   :original-players
   :open-decklists
   :password
   :players
   :room
   :save-replay
   :singleton
   :spectators
   :corp-spectators
   :runner-spectators
   :spectatorhands
   :started
   :timer
   :title
   :old
   :description
   ;; for tournament system
   :time-extension
   :excluded?
   :round-end-time
   ])

(defn maybe-round-end-time
  [lobby]
  (when (= (:room lobby) "competitive")
    (:round-end (app-state/tournament-state) nil)))

(defn lobby-summary
  "Strips private server information from a game map, preparing to send the game to clients."
  ([lobby] (lobby-summary lobby nil))
  ([lobby participating?]
   (-> lobby
       (assoc :old (> (count (or (:messages lobby) [])) 10))
       (update :password boolean)
       (update :players #(prepare-players lobby %))
       (update :spectators #(prepare-players lobby %))
       (update :corp-spectators #(prepare-players lobby %))
       (update :runner-spectators #(prepare-players lobby %))
       (update :original-players prepare-original-players)
       (update :messages #(when participating? %))
       ;; if there's a current tournament, insert the end time for the round
       (assoc :round-end-time (maybe-round-end-time lobby))
       (select-non-nil-keys lobby-keys))))

(defn get-blocked-list [user]
  (->> (app-state/get-user (:username user)) :options :blocked-users (map str/lower-case)))

(defn filter-lobby-list
  [lobbies user]
  (let [user-block-list (set (get-blocked-list user))]
    (filter
      (fn [lobby]
        (let [player-usernames (->> (:players lobby)
                                    (keep #(get-in % [:user :username]))
                                    (map str/lower-case)
                                    (set))
              user-blocked-players?
              (if (seq user-block-list)
                (seq (set/intersection user-block-list player-usernames))
                false)
              players-blocked-user?
              (-> (mapcat get-blocked-list (map :user (:players lobby)))
                  (set)
                  (contains? (str/lower-case (:username user ""))))]
          (not (or user-blocked-players? players-blocked-user?))))
      lobbies)))

(defn categorize-lobby
  "Categorizes the lobby into one of the following categoris:
  open-recent, open-old, started-allowing-spectators, or started-no-spectators"
  [lobby]
  (cond
    (not (:started lobby)) (if (:old lobby) :open-old :open-recent)
    (:allow-spectator lobby) :allowing-spectators
    :else :no-spectators))

(defn sorted-lobbies
  "Sorts `lobbies` into a list with opened games on top, and other games below.
  Open games will be sorted oldest to newest, other games will be sorted newest to oldest."
  [lobbies]
  (let [grouped-lobbies (group-by #(categorize-lobby %)
                                  (->> (map lobby-summary lobbies)
                                       (sort-by :date)))
        {:keys [open-recent open-old allowing-spectators no-spectators]} grouped-lobbies]
    (concat open-recent open-old (reverse allowing-spectators) (reverse no-spectators))))

(defn prepare-lobby-list
  [lobbies users]
  (let [in-order-lobbies (sorted-lobbies lobbies)]
    (for [user users
          :let [uid (:uid user)]]
      (let [filtered-lobbies (into [] (filter-lobby-list in-order-lobbies user))]
        [uid [:lobby/list filtered-lobbies]]))))

(defn lobby-update-uids
  []
  (filter #(app-state/receive-lobby-updates? %) (ws/connected-uids)))

(defn broadcast-lobby-list
  "Sends the lobby list to all users or a given list of users.
  Filters the list per each users block list."
  ([]
   (let [user-cache (:users @app-state/app-state)
         uids (lobby-update-uids)
         users (map #(get user-cache %) uids)]
     (broadcast-lobby-list users)))
  ([users]
   (assert (or (sequential? users) (nil? users)) (str "Users must be a sequence: " (pr-str users)))
   (let [lobbies (app-state/get-lobbies)]
     (doseq [[uid ev] (prepare-lobby-list lobbies users)]
       (when uid
         (ws/chsk-send! uid ev))))))

(defn prepare-lobby-state [lobby]
  (let [lobby-state (lobby-summary lobby true)]
    (for [user (get-players-and-spectators lobby)
          :let [uid (:uid user)]]
      [uid [:lobby/state lobby-state]])))

(defn send-lobby-state [lobby]
  (when lobby
    (doseq [[uid ev] (prepare-lobby-state lobby)]
      (when uid
        (ws/chsk-send! uid ev)))))

(defn register-lobby
  [lobbies lobby uid]
  (let [gameid (:gameid lobby)]
    (if (app-state/uid-in-lobby-as-player? uid lobbies)
      lobbies
      (assoc lobbies gameid lobby))))

(defn send-message [lobby message]
  (update lobby :messages conj message))

(defn try-create-lobby
  [uid user ?data]
  (let [lobby (-> (create-new-lobby {:uid uid :user user :options ?data})
                  (send-message
                   (core/make-system-message (str (:username user) " has created the game."))))
        new-app-state (swap! app-state/app-state update :lobbies
                             register-lobby lobby uid)
        lobby? (get-in new-app-state [:lobbies (:gameid lobby)])]
    (when lobby?
      (assign-tournament-properties lobby?)
      (send-lobby-state lobby?)
      (broadcast-lobby-list))))

(defmethod ws/-msg-handler :lobby/create
  lobby--create
  [{{user :user} :ring-req
    uid :uid
    ?data :?data
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (if (:block-game-creation @app-state/app-state)
      (ws/chsk-send! uid [:lobby/toast {:message :lobby_creation-paused
                                        :type "error"}])
      (try-create-lobby uid user ?data))
    (log-delay! timestamp id)))

(defn clear-lobby-state [uid]
  (when uid
    (ws/chsk-send! uid [:lobby/state])))

(defn send-lobby-list [uid]
  (when uid
    (let [[[_ ev]] (prepare-lobby-list (app-state/get-lobbies)
                                       [(app-state/get-user uid)])]
      (ws/chsk-send! uid ev))
    (if-let [lobby (app-state/uid->lobby uid)]
      (send-lobby-state lobby)
      (clear-lobby-state uid))))

(defmethod ws/-msg-handler :lobby/list
  lobby--list
  [{uid :uid
    id :id
    timestamp :timestamp}]
  (lobby-thread (send-lobby-list uid)
                (ws/chsk-send! uid [:lobby/block-game-creation
                                    (:block-game-creation @app-state/app-state)])
                (log-delay! timestamp id)))

(defmethod ws/-msg-handler :lobby/block-game-creation
  lobby--block-game-creation
  [{uid :uid
    id :id
    timestamp :timestamp}]
  (lobby-thread (ws/chsk-send! uid [:lobby/block-game-creation
                                    (:block-game-creation @app-state/app-state)])
                (log-delay! timestamp id)))

(defn player?
  "Returns player if the uid is a player in a given lobby"
  [uid lobby]
  (find-first #(= uid (:uid %)) (:players lobby)))

(defn first-player? [uid lobby]
  (let [player (first (:players lobby))]
    (when (= uid (:uid player))
      player)))

(defn spectator?
  "Returns player if the uid is a spectator in the given lobby"
  [uid lobby]
  (find-first #(= uid (:uid %)) (:spectators lobby)))

(defn in-lobby?
  "Returns player if the uid is a player or spectator in the given lobby"
  [uid lobby]
  (or (player? uid lobby)
      (spectator? uid lobby)))

(defn handle-set-last-update [lobbies gameid uid]
  (let [lobby (get lobbies gameid)]
    (if (and lobby (in-lobby? uid lobby))
      (assoc-in lobbies [gameid :last-update] (inst/now))
      lobbies)))

(defn handle-leave-lobby [lobbies uid leave-message]
  (if-let [lobby (app-state/uid->lobby lobbies uid)]
    (let [gameid (:gameid lobby)
          players (remove #(= uid (:uid %)) (:players lobby))
          spectators (remove #(= uid (:uid %)) (:spectators lobby))
          corp-spectators (remove #(= uid (:uid %)) (:corp-spectators lobby))
          runner-spectators (remove #(= uid (:uid %)) (:runner-spectators lobby))]
      (if (pos? (count players))
        (-> lobbies
            (update gameid send-message leave-message)
            (assoc-in [gameid :players] players)
            (assoc-in [gameid :spectators] spectators)
            (assoc-in [gameid :runner-spectators] runner-spectators)
            (assoc-in [gameid :corp-spectators] corp-spectators))
        (dissoc lobbies gameid)))
    lobbies))

(defn close-lobby!
  "Closes the given game lobby, booting all players and updating stats."
  ([db lobby] (close-lobby! db lobby nil))
  ([db {:keys [gameid pool started on-close] :as lobby} skip-on-close]
   (when started
     (stats/game-finished db lobby)
     (stats/update-deck-stats db lobby)
     (stats/update-game-stats db lobby)
     (stats/push-stats-update db lobby))
   (swap! app-state/app-state update :lobbies dissoc gameid)
   (doseq [uid (keep :uid (get-players-and-spectators lobby))]
     (clear-lobby-state uid))
   (leave-pool! pool gameid)
   (when (and (not skip-on-close) on-close)
     (on-close lobby))))

(defn leave-lobby! [db user uid ?reply-fn lobby]
  (let [leave-message (core/make-system-message (str (:username user) " left the game."))
        new-app-state (swap! app-state/app-state update :lobbies
                             #(handle-leave-lobby % uid leave-message))
          lobby? (get-in new-app-state [:lobbies (:gameid lobby)])]
      (if lobby?
        (when-let [state (:state lobby?)]
          (let [side (side-from-str (:side (player? uid lobby) ""))]
            (swap! state update side dissoc :user)))
        (close-lobby! db lobby))
      (send-lobby-state lobby?)
      (broadcast-lobby-list)
      (when ?reply-fn (?reply-fn true))
      lobby?))

(defmethod ws/-msg-handler :lobby/leave
  lobby--leave
  [{{db :system/db user :user} :ring-req
    uid :uid
    {gameid :gameid} :?data
    ?reply-fn :?reply-fn
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (let [lobby (app-state/get-lobby gameid)]
      (when (and lobby (in-lobby? uid lobby))
        (leave-lobby! db user uid ?reply-fn lobby))
      (log-delay! timestamp id))))

(defn find-deck
  [db opts]
  (assert (:_id opts) ":_id is required")
  (mc/find-one-as-map db "decks" opts))

(defn find-deck-for-user [db deck-id user]
  (let [username (:username user)]
    (find-deck db {:_id (mongodb/->object-id deck-id)
                   :username username})))

(defn process-deck [raw-deck]
  (let [identity-card (server-card (-> raw-deck :identity :title) false)
        cards (->> (:cards raw-deck)
                   (keep #(when-let [card (server-card (:card %) false)]
                            (assoc % :card card))))
        deck (assoc raw-deck
                    :identity identity-card
                    :cards cards)
        status (validator/calculate-deck-status deck)]
    (-> deck
        (assoc :status status)
        (dissoc :parsed?))))

(defn valid-deck-for-lobby? [lobby deck]
  (and (:identity deck)
       (or (= "casual" (:format lobby))
           (validator/legal-deck? deck (:format lobby)))))

(defn update-deck-for-player-in-lobby [players uid deck]
  (mapv (fn [p] (if (= uid (:uid p))
                  (assoc p :deck deck)
                  p))
        players))

(defn handle-select-deck [lobbies uid deck]
  (let [lobby (app-state/uid-player->lobby lobbies uid)
        gameid (:gameid lobby)]
    (if (and gameid (valid-deck-for-lobby? lobby deck))
      (update-in lobbies [gameid :players] update-deck-for-player-in-lobby uid deck)
      lobbies)))

(defmethod ws/-msg-handler :lobby/deck
  lobby--deck
  [{{db :system/db user :user} :ring-req
    uid :uid
    {:keys [gameid deck-id]} :?data
    ?reply-fn :?reply-fn
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (let [lobby (app-state/get-lobby gameid)]
      (if (and lobby (in-lobby? uid lobby))
        (let [raw-deck (find-deck-for-user db deck-id user)
              processed-deck (process-deck raw-deck)
              new-app-state
              (swap! app-state/app-state
                     update :lobbies #(-> %
                                          (handle-select-deck uid processed-deck)
                                          (handle-set-last-update (:gameid lobby) uid)))
              lobby? (get-in new-app-state [:lobbies (:gameid lobby)])]
          (send-lobby-state lobby?)
          ;;(broadcast-lobby-list)
          (?reply-fn (some #(= processed-deck (:deck %)) (:players lobby?))))
        (?reply-fn false))
      (log-delay! timestamp id))))

(defn handle-send-message [lobbies gameid message]
  (if-let [lobby (get lobbies gameid)]
    (-> lobby
        (send-message message)
        (->> (assoc lobbies gameid)))
    lobbies))

(defmethod ws/-msg-handler :lobby/say
  lobby--say
  [{{user :user} :ring-req
    uid :uid
    {:keys [gameid text]} :?data
    id :id
    timestamp :timestamp}]
  (assert (string? text) "Message must be a string")
  (lobby-thread
    (let [lobby (app-state/get-lobby gameid)]
      (when (and lobby (in-lobby? uid lobby))
        (let [message (core/make-message {:user user :text text})
              new-app-state (swap! app-state/app-state
                                   update :lobbies #(-> %
                                                        (handle-send-message gameid message)
                                                        (handle-set-last-update gameid uid)))
              lobby? (get-in new-app-state [:lobbies gameid])]
          (send-lobby-state lobby?))))
    (log-delay! timestamp id)))

(defn check-password [lobby user password]
  (or (empty? (:password lobby))
      (superuser? user)
      (bcrypt/check (or password "") (:password lobby))))

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
  (if (or (not= 1 (-> lobby :players count))
          (already-in-game? user lobby))
    lobby
    (let [existing-player (-> lobby :players first)
          existing-player-side (determine-player-side existing-player request-side)
          existing-player (assoc existing-player :side existing-player-side)
          user-side (if (= "Corp" existing-player-side) "Runner" "Corp")
          user-as-player {:uid uid
                          :user user
                          :side user-side}]
      (assoc lobby :players [existing-player user-as-player]))))

(defn handle-join-lobby [lobbies ?data uid user correct-password? join-message]
  (let [{:keys [gameid request-side]} ?data
        lobby (get lobbies gameid)]
    (if (and user lobby (allowed-in-lobby user lobby) correct-password?)
      (-> lobby
          (insert-user-as-player uid user request-side)
          (send-message join-message)
          (->> (assoc lobbies gameid)))
      lobbies)))

(defn join-lobby! [user uid ?data ?reply-fn lobby]
  (let [correct-password? (check-password lobby user (:password ?data))
        join-message (core/make-system-message (str (:username user) " joined the game."))
        new-app-state (swap! app-state/app-state update :lobbies
                             #(handle-join-lobby % ?data uid user correct-password? join-message))
        lobby? (get-in new-app-state [:lobbies (:gameid ?data)])]
    (cond
      (and lobby? correct-password?)
      (do (when-let [player (player? uid lobby?)]
            (let [side (side-from-str (:side player))]
              (when-let [state (:state lobby?)]
                (swap! state assoc-in [side :user] user))))
          (send-lobby-state lobby?)
          (send-lobby-ting lobby?)
          (broadcast-lobby-list)
          (when ?reply-fn (?reply-fn 200))
          lobby?)
      (false? correct-password?)
      (when ?reply-fn (?reply-fn 403) nil)
      :else
      (when ?reply-fn (?reply-fn 404) nil))))

(defmethod ws/-msg-handler :lobby/join
  lobby--join
  [{{user :user} :ring-req
    uid :uid
    {gameid :gameid :as ?data} :?data
    ?reply-fn :?reply-fn
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (when-let [lobby (app-state/get-lobby gameid)]
      (join-lobby! user uid ?data ?reply-fn lobby))
    (log-delay! timestamp id)))

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
      (some? side) (update lobby :players (fn [x] (mapv #(change-side % side) x)))
      :else (update lobby :players #(mapv swap-side %)))))

(defn handle-swap-sides [lobbies gameid uid side swap-message]
  (if-let [lobby (get lobbies gameid)]
    (-> lobby
        (update-sides uid side)
        (send-message swap-message)
        (->> (assoc lobbies gameid)))
    lobbies))

(defn swap-text
  "Returns an appropriate message indicating that the players have swapped sides, 
  where `player1-side` is the side that host is switching to"
  [players player1-side]
  (let [[player1 player2] (if (> (count players) 1)
                            (mapv swap-side players)
                            (list (change-side (first players) player1-side)))
        player1-username (-> player1 :user :username)
        player2-username (-> player2 :user :username)]
    (str player1-username " has swapped sides. "
         (if (= player1-side "Any Side")
           "Waiting for opponent."
           (str player1-username " is now " (:side player1) ". "))
         (when player2
           (str player2-username " is now " (:side player2) ".")))))

(defmethod ws/-msg-handler :lobby/swap
  lobby--swap
  [{{user :user} :ring-req
    uid :uid
    {:keys [gameid side]} :?data
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (let [lobby (app-state/get-lobby gameid)]
      (when (and lobby (first-player? uid lobby))
        (let [swap-message (core/make-message {:user user
                                               :text (swap-text (:players lobby) side)})
              new-app-state (swap! app-state/app-state
                                   update :lobbies
                                   #(-> %
                                        (handle-swap-sides gameid uid side swap-message)
                                        (handle-set-last-update gameid uid)))
              lobby? (get-in new-app-state [:lobbies gameid])]
          (send-lobby-state lobby?)
          (broadcast-lobby-list))))
    (log-delay! timestamp id)))

(defmethod ws/-msg-handler :lobby/shift-game
  lobby--shift-game
  [{{db :system/db user :user} :ring-req
    {:keys [gameid room]} :?data
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (when-let [lobby (app-state/get-lobby gameid)]
      (when (or (superuser? user) (to? user))
        (let [player-name (-> lobby :original-players first :user :username)
              game-name (:title lobby)
              new-app-state (swap! app-state/app-state assoc-in [:lobbies gameid :room] room)]
          (send-lobby-state (get-in new-app-state [:lobbies (:gameid lobby)]))
          (broadcast-lobby-list)
          (broadcast-lobby-list [id])
          (mc/insert db "moderator_actions"
                     {:moderator (:username user)
                      :action :shift-game
                      :game-name game-name
                      :first-player player-name
                      :target-room room
                      :date (inst/now)}))))
    (log-delay! timestamp id)))

(defmethod ws/-msg-handler :lobby/rename-game
  lobby--rename-game
  [{{db :system/db user :user} :ring-req
    {:keys [gameid]} :?data
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (when-let [lobby (app-state/get-lobby gameid)]
      (when (superuser? user)
        (let [player-name (-> lobby :original-players first :user :username)
              bad-name (:title lobby)
              new-app-state (swap! app-state/app-state assoc-in [:lobbies gameid :title] (str player-name "'s game"))]
          (send-lobby-state (get-in new-app-state [:lobbies (:gameid lobby)]))
          (broadcast-lobby-list)
          (broadcast-lobby-list [id])
          (mc/insert db "moderator_actions"
                     {:moderator (:username user)
                      :action :rename-game
                      :game-name bad-name
                      :first-player player-name
                      :date (inst/now)}))))
    (log-delay! timestamp id)))

(defmethod ws/-msg-handler :lobby/delete-game
  lobby--delete-game
  [{{db :system/db user :user} :ring-req
    {:keys [gameid]} :?data
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (let [lobby (app-state/get-lobby gameid)
          player-name (-> lobby :original-players first :user :username)
          bad-name (:title lobby)]
      (when (and (superuser? user) lobby)
        (close-lobby! db lobby)
        (broadcast-lobby-list)
        (broadcast-lobby-list [id])
        (mc/insert db "moderator_actions"
                   {:moderator (:username user)
                    :action :delete-game
                    :game-name bad-name
                    :first-player player-name
                    :date (inst/now)})))
    (log-delay! timestamp id)))

(defn clear-inactive-lobbies
  "Called by a background thread to close lobbies that are inactive for some number of seconds."
  [db time-inactive]
  (let [changed? (volatile! false)]
    (doseq [{:keys [gameid last-update started] :as lobby} (app-state/get-lobbies)]
      (when (and gameid
                 (inst/is-after (inst/now) (inst/plus-seconds last-update (- time-inactive 30)))
                 (not (inst/is-after (inst/now) (inst/plus-seconds last-update (- time-inactive 29)))))
        (let [uids (keep :uid (get-players-and-spectators lobby))]
          (doseq [uid uids]
            (when uid (ws/chsk-send! uid [:game/timeout-soon gameid])))))
      (when (and gameid (inst/is-after (inst/now) (inst/plus-seconds last-update time-inactive)))
        (let [uids (keep :uid (get-players-and-spectators lobby))]
          (vreset! changed? true)
          (when started
            (stats/game-finished db lobby)
            (doseq [uid uids]
              (when uid
                (ws/chsk-send! uid [:game/timeout gameid]))))
          (close-lobby! db lobby)
          (doseq [uid uids]
            (send-lobby-list uid)))))
    (when @changed?
      (broadcast-lobby-list))))

(defn watch-lobby [lobby uid user request-side]
  (if (already-in-game? user lobby)
    lobby
    (let [lobby (update lobby :spectators conj {:uid uid
                                                :user user})]
      (cond
        (= request-side "Corp")
        (update lobby :corp-spectators conj {:uid uid :user user})
        (= request-side "Runner")
        (update lobby :runner-spectators conj {:uid uid :user user})
        :else lobby))))

(defn handle-watch-lobby [lobbies gameid uid user correct-password? watch-message request-side]
  (let [lobby (get lobbies gameid)]
    (if (and user lobby (allowed-in-lobby user lobby) correct-password?)
      (-> lobby
          (watch-lobby uid user request-side)
          (send-message watch-message)
          (->> (assoc lobbies gameid)))
      lobbies)))

(defmethod ws/-msg-handler :lobby/watch
  lobby--watch
  [{{user :user} :ring-req
    uid :uid
    {:keys [gameid password request-side]} :?data
    ?reply-fn :?reply-fn
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (let [lobby (app-state/get-lobby gameid)]
      (when (and lobby (allowed-in-lobby user lobby))
        (let [correct-password? (check-password lobby user password)
              watch-message (core/make-system-message (str (:username user) " joined the game as a spectator" (when request-side (str " (" request-side " perspective)")) "."))
              new-app-state (swap! app-state/app-state
                                   update :lobbies #(-> %
                                                        (handle-watch-lobby gameid uid user correct-password? watch-message request-side)
                                                        (handle-set-last-update gameid uid)))
              lobby? (get-in new-app-state [:lobbies gameid])]
          (cond
            (and lobby? correct-password? (allowed-in-lobby user lobby?))
            (do (send-lobby-state lobby?)
                (send-lobby-ting lobby?)
                (broadcast-lobby-list)
                (when ?reply-fn (?reply-fn 200)))
            (false? correct-password?)
            (when ?reply-fn (?reply-fn 403))
            :else
            (when ?reply-fn (?reply-fn 404))))))
    (log-delay! timestamp id)))

(defn handle-toggle-spectator-mute [lobbies gameid uid]
  (let [lobby (get lobbies gameid)]
    (if (and lobby (in-lobby? uid lobby))
      (update-in lobbies [gameid :mute-spectators] not)
      lobbies)))

(defmethod ws/-msg-handler :lobby/pause-updates
  lobby--pause-updates
  [{uid :uid
    id :id
    timestamp :timestamp}]
  (lobby-thread (app-state/pause-lobby-updates uid)
                (log-delay! timestamp id)))

(defmethod ws/-msg-handler :lobby/continue-updates
  lobby--continue-updates
  [{uid :uid
    id :id
    timestamp :timestamp}]
  (lobby-thread
    (app-state/continue-lobby-updates uid)
    (send-lobby-list uid)
    (log-delay! timestamp id)))
