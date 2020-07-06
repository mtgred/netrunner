(ns web.admin
  (:require [web.db :refer [db object-id]]
            [web.lobby :refer [all-games refresh-lobby]]
            [web.utils :refer [response]]
            [web.config :refer [frontend-version]]
            [game.main :as main]
            [tasks.nrdb :refer [fetch-data]]
            [crypto.password.bcrypt :as bcrypt]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clj-time.core :as t]
            [clj-uuid :as uuid]
            ))

(defn wrap-version [handler]
  (fn [request]
    (handler (assoc request :version @frontend-version))))

(defn announcement-handler
  [{{:keys [message]} :params :as req}]
  (doseq [{state :state} (vals @all-games)]
    (when state
      (main/handle-announcement state message)))
  (response 200 {:message "ok"}))

(defn version-handler
  [{{:keys [version]} :params :as req}]
  (reset! frontend-version version)
  (mc/update db "config" {} {$set {:version version}})
  (response 200 {:message "ok" :version version}))

(defn fetch-handler
  "Provide an admin endpoint for fetching card data. Options to fetch can be supplied as parameters to the fetch endpoint."
  [{params :params :as req}]
  (try
    (fetch-data params)
    (response 200 {:message "ok"})
    (catch Exception e (do
                         (println "fetch-handler failed:" (.getMessage e))
                         (.printStackTrace e)
                         (response 500 {:message (str "Import data failed: " (.getMessage e))})))))






(defn parse-id
  [value]
  (let [link (first (next (re-find #"(?:tournaments/)(\d+)" value)))
        number (re-find #"\d+" value)]
    (or link number)))

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

(defn get-player-name
  [players player]
  (:name (get players (:id player))))

(defn latest-round
  [data]
  (last (:rounds data)))

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
          (nil? player2-score)) :drop
      (> player1-score player2-score) :player1
      (< player1-score player2-score) :player2
      :else :tie)))

(defn process-table
  [table players]
  (as-> table table
      (assoc table
             :player1 (transform-player players (:player1 table))
             :player2 (transform-player players (:player2 table)))
      (assoc table :winner (determine-winner table))
      (dissoc table :intentionalDraw :eliminationGame)))

(defn process-round
  [round players]
  (map #(process-table % players) round))

(defn process-all-rounds
  [data players]
  (map #(process-round % players) (:rounds data)))

(defn create-tournament-lobby
  [{:keys [player1 player2 allow-spectator tournament-name tournament-format]}]
  (let [gameid (uuid/v4)
        title (str tournament-name ": " player1 " vs " player2)
        players (->> (mc/find-maps db "users" {:username {$in [player1 player2]}})
                     (map #(select-keys % [:_id :username :emailhash :isadmin :options :stats]))
                     (map #(update-in [% :_id] str))
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
    (swap! all-games assoc gameid game)
    (refresh-lobby :create gameid)
    ))

(comment
  (let [event {:player1 "noah"
               :player2 "bogart"
               :allow-spectator true
               :tournament-name "continentals"
               :tournament-format "standard"
               }]
    (create-tournament-lobby event)
    ))

; (defn
;   [{{{:keys [username] :as user} :user} :ring-req
;     client-id                                     :client-id
;     {:keys [title format allow-spectator spectatorhands password room side options]} :?data :as event}]
;   (let [gameid (uuid/v4)
;         game {:date           (java.util.Date.)
;               :gameid         gameid
;               :title          title
;               :allow-spectator allow-spectator
;               :spectatorhands spectatorhands
;               :mute-spectators false
;               :password       (when (not-empty password) (bcrypt/encrypt password))
;               :room           room
;               :format         format
;               :players        [{:user    user
;                                 :ws-id   client-id
;                                 :side    side
;                                 :options options}]
;               :spectators     []
;               :messages       [{:user "__system__"
;                                 :text (str username " has created the game.")}]
;               :last-update    (t/now)}]
;     (swap! all-games assoc gameid game)
;     (swap! client-gameids assoc client-id gameid)
;     (ws/send! client-id [:lobby/select {:gameid gameid}])
;     (refresh-lobby :create gameid)))
