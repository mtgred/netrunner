(ns web.tournament
  (:require [web.db :refer [db object-id]]
            [web.lobby :refer [all-games refresh-lobby close-lobby]]
            [web.utils :refer [response]]
            [web.ws :as ws]
            [jinteki.utils :refer [str->int]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-uuid :as uuid]))

(defn auth [req]
  (response 200 {:message "ok"}))

(defn parse-response
  [body]
  (json/parse-string body true))

(defn download-cobra-data
  [id]
  (let [data (http/get (str "http://cobr.ai/tournaments/" id ".json"))
        {:keys [status body error] :as resp} @data]
    (cond
      error (throw (Exception. (str "Failed to download file " error)))
      (= 200 status) (parse-response body)
      :else (throw (Exception. (str "Failed to download file, status " status))))))

(defn build-players
  [data]
  (into {} (for [player (:players data)]
             [(:id player) player])))

(defn latest-round
  [data]
  (last (:rounds data)))

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
  [{:keys [tournament-name tournament-format table username1 username2 allow-spectator on-close]}]
  (let [gameid (uuid/v4)
        title (str tournament-name ", Table " table ": " username1 " vs " username2)
        players (->> (mc/find-maps db "users" {:username {$in [username1 username2]}})
                     (map #(select-keys % [:_id :username :emailhash :isadmin :options :stats]))
                     (map #(update % :_id str))
                     (map #(hash-map :user %))
                     (into []))
        game {:gameid gameid
              :title title
              :room "tournament"
              :format tournament-format
              :tournament-name tournament-name
              :players players
              :spectators []
              :messages [{:user "__system__"
                          :text "The game has been created."}]
              :allow-spectator allow-spectator
              :spectatorhands false
              :mute-spectators true
              :date (java.util.Date.)
              :last-update (t/now)
              :on-close on-close}]
    (when (= 2 (count players))
      (swap! all-games assoc gameid game)
      (refresh-lobby :create gameid)
      game)))

(defn create-lobbies-for-tournament
  [data selected-round]
  (let [players (build-players data)
        rounds (process-all-rounds data players)
        selected-round (nth rounds selected-round (count rounds))]
    (keep
      (fn [table]
        (let [base {:tournament-name (:name data)
                    :tournament-format "standard"
                    :table (:table table)
                    :username1 (get-in table [:player1 :name])
                    :username2 (get-in table [:player2 :name])
                    :allow-spectator true}]
          (create-tournament-lobby
            (assoc base :on-close #(create-tournament-lobby base)))))
      selected-round)))

(defn load-tournament
  [{{user :user} :ring-req
    {:keys [cobra-link]} :?data
    client-id :client-id
    reply-fn :?reply-fn
    :as msg}]
  (if (:tournament-organizer user)
    (let [data (download-cobra-data cobra-link)
          player-count (count (:players data))
          player-names (keep :name (:players data))
          db-players (mc/find-maps db "users" {:username {$in player-names}})
          missing-players (remove #(seq (filter (fn [e] (= % e)) (map :username db-players))) player-names)

          players (build-players data)
          rounds (process-all-rounds data players)]
      (ws/send! client-id [:tournament/loaded {:data {:players players
                                                      :missing-players missing-players
                                                      :rounds rounds
                                                      :tournament-name (:name data)}}])
      (when reply-fn (reply-fn 200)))
    (when reply-fn (reply-fn 403))))

(defn create-tables
  [{{user :user} :ring-req
    {:keys [cobra-link selected-round]} :?data
    client-id :client-id
    reply-fn :?reply-fn
    :as msg}]
  (if (:tournament-organizer user)
    (let [data (download-cobra-data cobra-link)
          created-rounds (create-lobbies-for-tournament data (str->int selected-round))]
      (ws/send! client-id [:tournament/created {:data {:created-rounds (count created-rounds)}}])
      (when reply-fn (reply-fn 200)))
    (when reply-fn (reply-fn 403))))

(defn close-tournament-tables
  [tournament-name]
  (let [tables (for [game (vals @all-games)
                     :when (= tournament-name (:tournament-name game))]
                 {:started false
                  :gameid (:gameid game)})]
    (map close-lobby tables)))

(defn delete-tables
  [{{user :user} :ring-req
    {:keys [tournament-name]} :?data
    client-id :client-id
    reply-fn :?reply-fn
    :as msg}]
  (if (:tournament-organizer user)
    (let [deleted-rounds (close-tournament-tables tournament-name)]
      (ws/send! client-id [:tournament/deleted {:data {:deleted-rounds (count deleted-rounds)}}])
      (when reply-fn (reply-fn 200)))
    (when reply-fn (reply-fn 403))))

(swap! ws/ws-handlers assoc :tournament/fetch [#'load-tournament])
(swap! ws/ws-handlers assoc :tournament/create [#'create-tables])
(swap! ws/ws-handlers assoc :tournament/delete [#'delete-tables])
