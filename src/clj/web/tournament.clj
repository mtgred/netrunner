(ns web.tournament
  (:require
   [cheshire.core :as json]
   [clj-uuid :as uuid]
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [jinteki.utils :refer [str->int]]
   [monger.operators :refer :all]
   [org.httpkit.client :as http]
   ; [web.lobby :refer [all-games refresh-lobby close-lobby]]
   [web.mongodb :refer [find-maps-case-insensitive]]
   [web.app-state :as app-state]
   [web.stats :refer [fetch-elapsed]]
   [web.utils :refer [response]]
   [web.ws :as ws]))

(defn auth [_]
  (response 200 {:message "ok"}))

(defn parse-response
  [body]
  (json/parse-string body true))

(defn download-cobra-data
  [id]
  (let [data (http/get (str "http://cobr.ai/tournaments/" id ".json"))
        {:keys [status body error headers]} @data]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (and
        (= 200 status)
        (str/includes? (:content-type headers) "application/json"))
      (assoc (parse-response body) :cobra-link id)
      :else (throw (Exception. (str "Failed to download file, status " status))))))

(defn build-players
  [data]
  (into {} (for [player (:players data)] [(:id player) player])))

(defn get-player-name
  [players player]
  (:name (get players (:id player))))

(defn transform-player
  [players player]
  (-> player
      (assoc :name (get-player-name players player)
             :score (:combinedScore player))
      (dissoc :corpScore :runnerScore :combinedScore)))

(defn determine-winner
  [table]
  (let [player1-score (get-in table [:player1 :score])
        player2-score (get-in table [:player2 :score])]
    (cond
      (or (nil? player1-score)
          (nil? player2-score)) :not-finished
      (> player1-score player2-score) :player1
      (< player1-score player2-score) :player2
      :else :tie)))

(defn process-table
  [table players]
  (let [player1 (transform-player players (:player1 table))
        player2 (transform-player players (:player2 table))]
    (when (and (:name player1) (:name player2))
      (as-> table table
        (assoc table :player1 player1 :player2 player2)
        (assoc table :winner (determine-winner table))
        (dissoc table :intentionalDraw :eliminationGame)))))

(defn process-round
  [round players]
  (keep #(process-table % players) round))

(defn process-all-rounds
  [data players]
  (map #(process-round % players) (:rounds data)))

(defn create-tournament-lobby
  [db {:keys [tournament-name tournament-format selected-round table
              username1 username2 allow-spectator on-close cobra-link
              save-replay timer]}]
  (let [gameid (uuid/v4)
        title (str tournament-name ", Round " selected-round
                   ", Table " table ": " username1 " vs " username2)
        query (into [] (for [username [username1 username2]] {:username username}))
        players (->> (find-maps-case-insensitive db "users" {$or query})
                     (map #(select-keys % [:_id :username :emailhash :isadmin :options :stats]))
                     (map #(update % :_id str))
                     (map #(hash-map :user %)))
        players (->> (if (= username1 (get-in (first players) [:user :username]))
                       [(first players) (second players)]
                       [(second players) (first players)])
                     (filter identity)
                     (into []))
        now (inst/now)
        game {:gameid gameid
              :title title
              :room "tournament"
              :format tournament-format
              :tournament-name tournament-name
              :cobra-link cobra-link
              :players players
              :spectators []
              :spectator-count 0
              :messages [{:user "__system__"
                          :text "The game has been created."}]
              :allow-spectator allow-spectator
              :save-replay save-replay
              :timer timer
              :spectatorhands false
              :mute-spectators true
              :date now
              :last-update now
              :on-close on-close}]
    (when (= 2 (count players))
      ; (refresh-lobby gameid game)
      game)))

(defn create-lobbies-for-tournament
  [db data selected-round {:keys [timer save-replays? single-sided?]}]
  (let [players (build-players data)
        rounds (process-all-rounds data players)
        round (nth rounds selected-round (count rounds))]
    (keep
      (fn [table]
        (let [username1 (get-in table [:player1 :name])
              username2 (get-in table [:player2 :name])
              base {:tournament-name (:name data)
                    :tournament-format "standard"
                    :cobra-link (:cobra-link data)
                    :selected-round (inc selected-round)
                    :table (:table table)
                    :timer timer
                    :username1 username1
                    :username2 username2
                    :save-replay save-replays?
                    :allow-spectator true}
              callback (fn [first-game]
                         (create-tournament-lobby
                           db (assoc base
                                     :username1 username2
                                     :username2 username1
                                     :timer (when-let
                                              [elapsed (fetch-elapsed db (:gameid first-game))]
                                              (max 0 (- timer elapsed))))))]
          (create-tournament-lobby
            db (assoc base :on-close (when-not single-sided? callback)))))
      round)))

(defn load-tournament
  [{{db :system/db} :ring-req
    {:keys [cobra-link]} :?data
    uid :uid}]
  (let [data (download-cobra-data cobra-link)
        player-names (keep :name (:players data))
        query (into [] (for [username player-names] {:username username}))
        db-players (find-maps-case-insensitive db "users" {$or query})
        found-player-names #(seq (filter (fn [e] (= (str/lower-case %) (str/lower-case e))) (map :username db-players)))
        missing-players (remove found-player-names player-names)
        players (build-players data)
        rounds (process-all-rounds data players)]
    (ws/broadcast-to! [uid] :tournament/loaded {:data {:players players
                                                       :missing-players missing-players
                                                       :rounds rounds
                                                       :cobra-link cobra-link
                                                       :tournament-name (:name data)}})))

(defn wrap-with-to-handler
  "Wrap a function in a handler which checks that the user is a tournament organizer."
  [handler]
  (fn [{{user :user} :ring-req
        reply-fn :?reply-fn
        :as msg}]
    (if (:tournament-organizer user)
      (do (handler msg)
          (when reply-fn (reply-fn 200)))
      (when reply-fn (reply-fn 403)))))

(defn create-tables
  [{{db :system/db} :ring-req
    {:keys [cobra-link selected-round save-replays? single-sided? timer]} :?data
    uid :uid}]
  (let [data (download-cobra-data cobra-link)
        created-rounds (create-lobbies-for-tournament
                         db data
                         (str->int selected-round)
                         {:timer timer
                          :save-replays? save-replays?
                          :single-sided? single-sided?})]
    (ws/broadcast-to! [uid] :tournament/created {:data {:created-rounds (count created-rounds)}})))

(defmethod ws/-msg-handler :tournament/create
  tournament--create
  [event]
  ((wrap-with-to-handler create-tables) event))

(defn close-tournament-tables
  [cobra-link]
  (when cobra-link
    (let [tables (for [game (vals {})
                       :when (= cobra-link (:cobra-link game))]
                   {:started false
                    :gameid (:gameid game)})]
      ; (map #(close-lobby % true) tables)
      )))

(defmethod ws/-msg-handler :tournament/fetch
  tournament--fetch
  [event]
  ((wrap-with-to-handler load-tournament) event))

(defn- delete-tables
  [{{:keys [cobra-link]} :?data
    uid :uid}]
  (let [deleted-rounds (close-tournament-tables cobra-link)]
    (ws/broadcast-to! [uid] :tournament/deleted {:data {:deleted-rounds (count deleted-rounds)}})))

(defmethod ws/-msg-handler :tournament/delete
  tournament--delete
  [event]
  ((wrap-with-to-handler delete-tables) event))

(defn- view-tables
  [{uid :uid}]
  ;; find all tables in the tournament lobbie
  ;; strip them to just:
  ;;   id, player1, player2, title, time-extension
  (let [strip-players (fn [players] (mapv #(select-keys % [:uid :side]) players))
        comp-lobbies (->> (app-state/get-lobbies)
                          (filter #(= (:room %) "competitive"))
                          (map #(select-keys % [:gameid :title :players :time-extension :excluded?]))
                          (map #(update % :players strip-players)))]
    (ws/broadcast-to! [uid] :tournament/view-tables {:competitive-lobbies (vec comp-lobbies)
                                                     :tournament-state (app-state/tournament-state)})))

;; gets a list of all competitive lobbies
(defmethod ws/-msg-handler :tournament/view-tables
  tournament--view-tables
  [event]
  ((wrap-with-to-handler view-tables) event))

(defn- update-tables
  [{{:keys [competitive-lobbies]} :?data
    uid :uid}]
  (let [competitive-lobbies (mapv #(select-keys % [:gameid :excluded? :time-extension]) competitive-lobbies)
        to-update (into {} (map (juxt :gameid identity) competitive-lobbies))]
    (swap! app-state/app-state update :lobbies
           #(merge-with merge % (select-keys to-update (keys %))))
    (view-tables {:uid uid})))

(defmethod ws/-msg-handler :tournament/update-tables
  tournament--update-tables
  [event]
  ((wrap-with-to-handler update-tables) event))
