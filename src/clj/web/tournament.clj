(ns web.tournament
  (:require [web.db :refer [db object-id]]
            [web.lobby :refer [all-games refresh-lobby]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-uuid :as uuid]
            ))

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
  [{:keys [tournament-name tournament-format table username1 username2 allow-spectator]}]
  (let [gameid (uuid/v4)
        title (str tournament-name ", Table " table ": " username1 " vs " username2)
        players (->> (mc/find-maps db "users" {:username {$in [username1 username2]}})
                     (map #(select-keys % [:_id :username :emailhash :isadmin :options :stats]))
                     (map #(update % :_id str))
                     (map #(hash-map :user %))
                     (into []))
        game {
              :gameid gameid
              :title title
              :room "tournament"
              :format tournament-format
              :players players
              :spectators []
              :messages [{:user "__system__"
                          :text "The game has been created."}]
              :allow-spectator allow-spectator
              :spectatorhands false
              :mute-spectators true
              :date (java.util.Date.)
              :last-update (t/now)
              }
        ]
    (when (pos? (count players))
      (swap! all-games assoc gameid game)
      (refresh-lobby :create gameid))
    ))

(defn create-lobbies-for-tournament
  [data]
  (let [players (build-players data)
        rounds (process-all-rounds data players)
        current-round (last rounds)]
    (doseq [table current-round]
      (create-tournament-lobby
        {:tournament-name (:name data)
         :tournament-format "standard"
         :table (:table table)
         :username1 (get-in table [:player1 :name])
         :username2 (get-in table [:player2 :name])
         :allow-spectator true
         }))))

(defn auth [req]
  (response 200 {:message "ok"}))

(defn abs [n] (max n (- n)))

(defn load-tournament [{{username :username} :user
                        {id :id}             :params}]
  (if (and username id)
    (try
      (let [data (download-cobra-data id)
            player-count (count (:players data))
            player-names (keep :name (:players data))
            players (mc/find-maps db "users" {:username {$in (vec player-names)}})]
        (cond
          (and data (= player-count (count players)))
          (response 200 {:message "ok"})
          data
          (response 401 {:message (str "Missing players: " (abs (- player-count (count players))))
                         :player-names (remove #(seq (filter (fn [e] (= % e)) (map :username players))) player-names)})
          :else
          (response 403 {:message "Incorrect url"})))
      (catch Exception e
        (response 403 {:message "Incorrect url"})))))

(defn create-tournament [{{username :username} :user
                          {id :id}             :params}]
  (if (and username id)
    (try
      (let [data (download-cobra-data id)]
        (create-lobbies-for-tournament data)
        (response 200 {:message "ok"}))
      (catch Exception e
        (response 403 {:message "Incorrect url"})))
    (response 403 {:message "Forbidden"})))
