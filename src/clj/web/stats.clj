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
  [deck-id key]
  (mc/update db "decks" {:_id (object-id deck-id)} {"$inc" {key 1}})
  (response 200 {:message "OK"}))

(defn inc-game-stats
  "Update user's game stats for a given counter"
  [user-id key]
  (mc/update db "users" {:_id (object-id user-id)} {"$inc" {key 1}})
  (response 200 {:message "OK"}))

(defn update-deck-stats
  "Update stats for player decks on game ending"
  [all-games gameid]
  (let [start-players (get-in @all-games [gameid :original-players])
        end-players (get-in @all-games [gameid :ending-players])
        game-state (get-in @all-games [gameid :state])]

    ;; Increment games commenced stats for each deck that started the game
    (doseq [p start-players]
      (let [enable-deckstats (get-in p [:user :options :deckstats])
            deck-id (get-in p [:deck :_id])]
        (when (and enable-deckstats deck-id)
          (inc-deck-stats deck-id :stats.games-started))))

    ;; Increment games completed stats for each deck that ended the game
    ;; TODO Merge these operations rather than multiple MONGO writes
    (doseq [p end-players]
      (let [enable-deckstats (get-in p [:user :options :deckstats])
            deck-id (get-in p [:deck :_id])
            winning-deck (:winning-deck-id @game-state)
            losing-deck (:losing-deck-id @game-state)]
        (when (and enable-deckstats deck-id)
          (inc-deck-stats deck-id :stats.games-completed)
          (when (= winning-deck deck-id)
            (inc-deck-stats deck-id :stats.wins))
          (when (= losing-deck deck-id)
            (inc-deck-stats deck-id :stats.loses)))))))

(defn update-game-stats
  "Update game stats for users on game ending"
  [all-games gameid]
  (let [start-players (get-in @all-games [gameid :original-players])
        end-players (get-in @all-games [gameid :ending-players])
        game-state (get-in @all-games [gameid :state])]

    ;; Increment games commenced stats for each player that started the game
    ;; TODO Merge these operations rather than multiple MONGO writes
    (doseq [p start-players]
      (let [user-id (get-in p [:user :_id])
            side-str (:side p)]
        (inc-game-stats user-id :stats.games-started)
        (inc-game-stats user-id (build-stats-kw "stats.games-started-" side-str))))

    ;; Increment games completed stats for each player that ended the game
    (doseq [p end-players]
      (let [enable-userstats (get-in p [:user :options :gamestats])
            user-id (get-in p [:user :_id])
            username (get-in p [:user :username])
            side-str (:side p)
            winning-user (:winning-user @game-state)
            losing-user (:losing-user @game-state)]
        (inc-game-stats user-id :stats.games-completed)
        (inc-game-stats user-id (build-stats-kw "stats.games-completed-" side-str))
        (when enable-userstats)
          (when (= username winning-user)
            (inc-game-stats user-id :stats.wins)
            (inc-game-stats user-id (build-stats-kw "stats.wins-" side-str)))
          (when (= username losing-user)
            (inc-game-stats user-id :stats.loses)
            (inc-game-stats user-id (build-stats-kw "stats.loses-" side-str)))))))
