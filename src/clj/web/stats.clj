(ns web.stats
  (:require [web.db :refer [db object-id]]
            [monger.collection :as mc]
            [web.utils :refer [response]])
  (:import org.bson.types.ObjectId))

(defn clear-user-stats
  "Clear any statistics for a given user-id contained in a request"
  [req]
  (when-let [user-id (-> req :user :_id)]
    (mc/update db "users" {:_id (object-id user-id)} {"$unset" {:stats ""}})
    (response 200 {:message "OK"})))

(defn clear-deck-stats
  "Clear any statistics for a given deck-id contained in a request"
  [req]
  (when-let [deck-id (-> req :body :_id)]
    (mc/update db "decks" {:_id (object-id deck-id)} {"$unset" {:stats ""}})
    (response 200 {:message "OK"})))

(defn game-started?
  "Returns true if game has started"
  [all-games gameid]
  (get-in @all-games [gameid :started]))

(defn build-stats-kw
  "Take a stats prefix and add a side to it"
  [prefix side]
  (keyword (apply str prefix (clojure.string/lower-case side))))

(defn inc-deck-stats
  "Update deck stats for a given counter"
  [deck-id record]
  (mc/update db "decks" {:_id (object-id deck-id)} {"$inc" record})
  (response 200 {:message "OK"}))

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

    ;; Increment games commenced stats for each deck that started the game
    (doseq [p start-players]
      (let [enable-deckstats (get-in p [:user :options :deckstats])
            deck-id (get-in p [:deck :_id])]
        (when (and enable-deckstats deck-id)
          (inc-deck-stats deck-id '{:stats.games-started 1}))))

    ;; Increment games completed stats for each deck that ended the game
    (doseq [p end-players]
      (inc-deck-stats (get-in p [:deck :_id]) (deck-record-end all-games gameid p)))))

(defn inc-game-stats
  "Update user's game stats for a given counter"
  [user-id record]
  (mc/update db "users" {:_id (object-id user-id)} {"$inc" record})
  (response 200 {:message "OK"}))

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

    ;; Increment games commenced stats for each player that started the game
    (doseq [p start-players]
      (inc-game-stats (get-in p [:user :_id]) (game-record-start all-games gameid p)))

    ;; Increment games completed stats for each player that ended the game
    (doseq [p end-players]
      (inc-game-stats (get-in p [:user :_id]) (game-record-end all-games gameid p)))))
