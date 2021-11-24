(ns web.new-lobby
  (:require
   [clj-uuid :as uuid]
   [clojure.set :as set]
   [clojure.string :as str]
   [crypto.password.bcrypt :as bcrypt]
   [game.utils :refer [server-card]]
   [jinteki.utils :refer [select-non-nil-keys superuser?]]
   [jinteki.validator :as validator]
   [monger.collection :as mc]
   [web.app-state :as app-state]
   [web.mongodb :as mongodb]
   [web.ws :as ws]))

(defn create-new-lobby
  [{:keys [ring-req uid ?data]}]
  (let [{user :user} ring-req
        {:keys [title format timer allow-spectator save-replay api-access
                spectatorhands password room side]} ?data
        now (java.util.Date.)
        players [{:user user
                  :uid uid
                  :side side}]]
    {:gameid (uuid/v4)
     :date now
     :title title
     :allow-spectator allow-spectator
     :save-replay save-replay
     :api-access api-access
     :spectatorhands spectatorhands
     :mute-spectators false
     :password (when (not-empty password) (bcrypt/encrypt password))
     :room room
     :format format
     :players players
     :original-players players
     :spectators []
     :spectator-count 0
     :timer timer
     :messages [{:user "__system__"
                 :text (str (:username user) " has created the game.")}]
     :last-update now}))

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
  (->> players
       (keep (fn [p] (not-empty (select-keys p [:username :emailhash]))))
       (not-empty)))

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

(defn get-blocked-list [user]
  (->> user :options :blocked-users (map str/lower-case)))

(defn filter-lobby-list
  [lobbies user]
  (let [user-block-list (set (get-blocked-list user))]
    (filter
      (fn [lobby]
        (let [player-usernames (->> (:players lobby)
                                    (keep :username)
                                    (map str/lower-case)
                                    (set))
              user-blocked-players?
              (if (seq user-block-list)
                (seq (set/intersection user-block-list player-usernames))
                false)
              players-blocked-user?
              (-> (mapcat get-blocked-list (:players lobby))
                  (set)
                  (contains? (:username user)))]
          (not (or user-blocked-players? players-blocked-user?))))
      lobbies)))

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
  [lobbies users]
  (for [user users
        :let [uid (:username user)]]
     (let [filtered-lobbies (filter-lobby-list lobbies user)
           lobby-summaries (summaries-for-lobbies filtered-lobbies)]
       [uid [:lobby/list lobby-summaries]])))

(defn broadcast-lobby-list
  "Sends the lobby list to all users or a given list of users.
  Filters the list per each users block list."
  ([] (broadcast-lobby-list (app-state/get-users)))
  ([users]
   (assert (sequential? users) (str "Users must be a sequence: " (pr-str users)))
   (let [lobbies (app-state/get-lobbies)]
     (doseq [[uid ev] (prepare-lobby-list lobbies users)]
       (ws/chsk-send! uid ev)))))

(defn get-players-and-spectators [lobby]
  (concat (:players lobby) (:spectators lobby)))

(defn prepare-lobby-state [lobby]
  (let [lobby-state (lobby-summary lobby true)]
    (for [user (get-players-and-spectators lobby)
          :let [uid (:uid user)]]
      [uid [:lobby/state lobby-state]])))

(defn send-lobby-state [lobby]
  (when lobby
    (doseq [[uid ev] (prepare-lobby-state lobby)]
      (ws/chsk-send! uid ev))))

(defmethod ws/-msg-handler :lobby/create
  [{uid :uid :as event}]
  (let [lobby (create-new-lobby event)
        ;; perform the swap and only send updates if not already in a lobby
        new-app-state (app-state/register-lobby! lobby uid)]
    (when (-> new-app-state :lobbies (:gameid lobby))
      (send-lobby-state lobby)
      (broadcast-lobby-list))))

(defn clear-lobby-state [uid]
  (ws/chsk-send! uid [:lobby/state nil]))

(defn send-lobby-list [uid]
  (broadcast-lobby-list [(app-state/get-user uid)])
  (when uid
    (if-let [lobby (app-state/uid-in-lobby? uid)]
      (send-lobby-state lobby)
      (clear-lobby-state uid))))

(defmethod ws/-msg-handler :lobby/list
  [{uid :uid}]
  (send-lobby-list uid))

(defmethod ws/-msg-handler :lobby/leave
  [{uid :uid ?reply-fn :?reply-fn}]
  (app-state/remove-uid-from-lobby! uid)
  (broadcast-lobby-list)
  (?reply-fn true))

(defn find-deck
  [db opts]
  (assert (:_id opts) ":_id is required")
  (mc/find-one-as-map db "decks" opts))

(defn find-deck-for-user [db deck-id user]
  (let [username (:username user)]
    (find-deck db {:_id (mongodb/object-id deck-id)
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

(defn valid-deck-for-game? [deck game]
  (and (:identity deck)
       (or (= "casual" (:format game))
           (validator/legal-deck? deck (:format game)))))

(defmethod ws/-msg-handler :lobby/deck
  [{{db :system/db user :user} :ring-req
    uid :uid
    deck-id :?data
    ?reply-fn :?reply-fn}]
  (if-let [lobby (app-state/uid-in-lobby-as-player? uid)]
    (let [raw-deck (find-deck-for-user db deck-id user)
          processed-deck (process-deck raw-deck)]
      (if (valid-deck-for-game? processed-deck lobby)
        (do (app-state/update-deck-for-player! uid processed-deck)
            (send-lobby-state (app-state/uid-in-lobby-as-player? uid))
            (broadcast-lobby-list)
            (?reply-fn true))
        (?reply-fn false)))
    (?reply-fn false)))

(defn make-message [user text]
  {:user (if (= "__system__" user) user (select-keys user [:username :emailhash]))
   :text (str/trim text)})

(defmethod ws/-msg-handler :lobby/say
  [{{user :user} :ring-req
    uid :uid
    text :?data}]
  (assert (string? text) "Message must be a string")
  (when (app-state/uid-in-lobby? uid)
    (let [message (make-message user text)]
      (app-state/uid-say! uid message)
      (send-lobby-state (app-state/uid-in-lobby-as-player? uid)))))

(defn allowed-in-lobby
  [user lobby]
  (or (superuser? user)
      (seq (filter-lobby-list [lobby] user))))

(defn join-message [user]
  (make-message "__system__" (str (:username user) " joined the game.")))

(defn send-message [lobby message]
  (update lobby :messages conj message))

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

(defn join-lobby [lobby uid user request-side]
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

(defn lobby-ting [lobby]
  (for [user (get-players-and-spectators lobby)
        :let [uid (:uid user)]]
    [uid [:lobby/notification "ting"]]))

(defn send-lobby-ting [lobby]
  (when lobby
    (doseq [[uid ev] (lobby-ting lobby)]
      (ws/chsk-send! uid ev))))

(defn handle-join-lobby [lobbies ?data uid user correct-password?]
  (let [{:keys [gameid request-side]} ?data
        lobby (get lobbies gameid)]
    (if (and user lobby (allowed-in-lobby user lobby) correct-password?)
      (-> lobby
          (join-lobby uid user request-side)
          (send-message (join-message user))
          (->> (assoc lobbies gameid)))
      lobbies)))

(defmethod ws/-msg-handler :lobby/join
  [{{user :user} :ring-req
    uid :uid
    {:keys [gameid password] :as ?data} :?data
    ?reply-fn :?reply-fn}]
  (let [lobby (app-state/get-lobby gameid)
        correct-password? (or (superuser? user)
                              (empty? (:password lobby))
                              (bcrypt/check password (:password lobby)))
        new-app-state (swap! app-state/app-state
                             update :lobbies handle-join-lobby ?data uid user correct-password?)
        lobby (get-in new-app-state [:lobbies gameid])]
    (cond
      (and lobby correct-password?)
      (do (send-lobby-state lobby)
          (send-lobby-ting lobby)
          (broadcast-lobby-list)
          (when ?reply-fn (?reply-fn 200)))
      (false? correct-password?)
      (when ?reply-fn (?reply-fn 403))
      :else
      (when ?reply-fn (?reply-fn 404)))))

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

(defn update-sides [lobby uid side]
  (let [first-player (first (:players lobby))]
    (cond
      (not= (:uid first-player) uid) lobby
      (some? side) (update-in lobby [:players 0] change-side side)
      :else (update lobby :players #(mapv swap-side %)))))

(defmethod ws/-msg-handler :lobby/swap
  [{uid :uid
    {:keys [gameid side]} :?data}]
  (when-let [lobby (app-state/get-lobby gameid)]
    (let [new-app-state (swap! app-state/app-state update-in [:lobbies gameid] update-sides uid side)]
      (send-lobby-state (get-in new-app-state [:lobbies (:gameid lobby)]))
      (broadcast-lobby-list))))
