(ns web.stats
  (:require [web.db :refer [db object-id]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [monger.operators :refer :all]
            [web.ws :as ws]
            [web.utils :refer [response]]
            [game.utils :refer [dissoc-in]]
            [clojure.set :refer [rename-keys]]
            [clojure.string :refer [lower-case]]
            [clj-time.core :as t])

  (:import org.bson.types.ObjectId))

(defn clear-userstats-handler
      "Clear any statistics for a given user-id contained in a request"
      [{{:keys [username _id]} :user}]
  (if (acknowledged? (mc/update db "users" {:_id (object-id _id)} {"$unset" {:stats ""}}))
    (response 200 {:message "Deleted"})
    (response 403 {:message "Forbidden"})))

(defn clear-deckstats-handler
  "Clear any statistics for a given deck-id contained in a request"
  [{{id :id} :params}]
  (if id
    (if (acknowledged? (mc/update db "decks" {:_id (object-id id)} {"$unset" {:stats ""}}))
      (response 200 {:message "Deleted"})
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))

(defn stats-for-deck
  "Get statistics for a given deck id"
  [deck-id]
  (mc/find-one-as-map db "decks" {:_id (object-id deck-id)} ["stats"]))

(defn stats-for-user
  "Get statistics for a given user id"
  [user-id]
  (mc/find-one-as-map db "users" {:_id (object-id user-id)} ["stats"]))

(defn game-started?
  "Returns true if game has started"
  [all-games gameid]
  (get-in @all-games [gameid :started]))

(defn build-stats-kw
  "Take a stats prefix and add a side to it"
  [prefix side]
  (keyword (apply str prefix (lower-case side))))

(defn inc-deck-stats
  "Update deck stats for a given counter"
  [deck-id record]
  (when record
    (mc/update db "decks" {:_id (object-id deck-id)} {"$inc" record})))

(defn deck-record-end
  [all-games gameid p]
  (let [end-players (get-in @all-games [gameid :ending-players])
        game-state (get-in @all-games [gameid :state])
        enable-deckstats (get-in p [:user :options :deckstats])
        deck-id (get-in p [:deck :_id])
        winning-deck (:winning-deck-id @game-state)
        losing-deck (:losing-deck-id @game-state)
        record (merge (when (and enable-deckstats deck-id)
                        {:stats.games-completed 1})
                      (when (and enable-deckstats deck-id (= winning-deck deck-id))
                        {:stats.wins 1})
                      (when (and enable-deckstats deck-id (= losing-deck deck-id))
                        {:stats.loses 1}))]
    record))

(defn update-deck-stats
  "Update stats for player decks on game ending"
  [all-games gameid]
  (let [start-players (get-in @all-games [gameid :original-players])
        end-players (get-in @all-games [gameid :ending-players])]
    (doseq [p start-players]
      (let [enable-deckstats (get-in p [:user :options :deckstats])
            deck-id (get-in p [:deck :_id])]
        (when (and enable-deckstats deck-id)
          (inc-deck-stats deck-id '{:stats.games-started 1}))))
    (doseq [p end-players]
      (inc-deck-stats (get-in p [:deck :_id]) (deck-record-end all-games gameid p)))))

(defn inc-game-stats
  "Update user's game stats for a given counter"
  [user-id record]
  (mc/update db "users" {:_id (object-id user-id)} {"$inc" record}))

(defn game-record-start
  [all-games gameid p]
  (let [user-id (get-in p [:user :_id])
        side-str (:side p)
        record (merge {:stats.games-started 1}
                      {(build-stats-kw "stats.games-started-" side-str) 1})]
    record))

(defn game-record-end
  [all-games gameid p]
  (let [game-state (get-in @all-games [gameid :state])
        username (get-in p [:user :username])
        enable-userstats (get-in p [:user :options :gamestats])
        winning-user (:winning-user @game-state)
        losing-user (:losing-user @game-state)
        side-str (:side p)
        record (merge {:stats.games-completed 1}
                      {(build-stats-kw "stats.games-completed-" side-str) 1}
                      (when (and (= username winning-user) enable-userstats)
                                 {:stats.wins 1 (build-stats-kw "stats.wins-" side-str) 1})
                      (when (and (= username losing-user) enable-userstats)
                        {:stats.loses 1 (build-stats-kw "stats.loses-" side-str) 1}))]
    record))

(defn update-game-stats
  "Update game stats for users on game ending"
  [all-games gameid]
  (let [start-players (get-in @all-games [gameid :original-players])
        end-players (get-in @all-games [gameid :ending-players])]
    (doseq [p start-players]
      (inc-game-stats (get-in p [:user :_id]) (game-record-start all-games gameid p)))
    (doseq [p end-players]
      (inc-game-stats (get-in p [:user :_id]) (game-record-end all-games gameid p)))))


(defn push-stats-update
  "Gather updated deck and user stats and send via web socket to clients"
  [all-games gameid]
      ;; TODO Test again once we don't need to refresh page to end game session
    (let [end-players (get-in @all-games [gameid :ending-players])]
      (doseq [p end-players]
        (let [user-id   (get-in p [:user :_id])
              deck-id   (get-in p [:deck :_id])
              userstats (:stats (stats-for-user user-id))
              deckstats (:stats (stats-for-deck deck-id))]
        (ws/send! (:ws-id p) [:stats/update {:userstats userstats
                                             :deck-id   (str deck-id)
                                             :deckstats deckstats}])))))

(defn game-started [{:keys [gameid date start-date title room players format]}]
  (let [corp (some #(when (= "Corp" (:side %)) %) players)
        runner (some #(when (= "Runner" (:side %)) %) players)]
    (mc/insert db :game-logs {:gameid (str gameid)
                              :title title
                              :room room
                              :creation-date date
                              :start-date start-date
                              :format format
                              :corp {:player (select-keys (:user corp) [:username :emailhash])
                                     :deck-name (get-in corp [:deck :name])
                                     :identity (get-in corp [:deck :identity :title])}
                              :runner {:player (select-keys (:user runner) [:username :emailhash])
                                       :deck-name (get-in runner [:deck :name])
                                       :identity (get-in runner [:deck :identity :title])}})))

(defn game-finished [{:keys [state gameid]}]
  (when state
    (mc/update db :game-logs
               {:gameid (str gameid)}
               {"$set" {:winner (:winner @state)
                        :reason (:reason @state)
                        :end-date (java.util.Date.)
                        :stats (-> (:stats @state)
                                   (dissoc-in [:time :started])
                                   (dissoc-in [:time :ended]))
                        :turn (:turn @state)
                        :corp.agenda-points (get-in @state [:corp :agenda-point])
                        :runner.agenda-points (get-in @state [:runner :agenda-point])
                        :log (:log @state)}})))

(defn history [{{username :username} :user}]
  (if username
    (let [games (->> (mc/find-maps db :game-logs {$or [{:corp.player.username username}
                                                       {:runner.player.username username}]})
                     (map #(dissoc % :_id :log))
                     (into []))]
      (response 200 games))
    (response 401 {:message "Unauthorized"})))

(defn fetch-log [{{username :username} :user
                  {:keys [gameid]}     :params}]
  (if username
    (let [{:keys [log]} (mc/find-one-as-map db :game-logs {:gameid gameid} ["log"])
          log (or log {})]
      (response 200 log))
    (response 401 {:message "Unauthorized"})))
