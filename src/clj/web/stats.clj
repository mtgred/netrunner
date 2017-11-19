(ns web.stats
  (:require [web.db :refer [db object-id]]
            [monger.collection :as mc]
            [web.utils :refer [response]])
  (:import org.bson.types.ObjectId))

(defn clear-user-stats
  "Clear any statistics for a given user-id contained in a request"
  [req]
  (when-let [user-id (-> req :user :_id)]
    (response 200 (mc/update db "users" {:_id (object-id user-id)} {"$unset" {:stats ""}}))))

(defn clear-deck-stats
  "Clear any statistics for a given deck-id contained in a request"
  [req]
  (when-let [deck-id (-> req :body :_id)]
    (response 200 (mc/update db "decks" {:_id (object-id deck-id)} {"$unset" {:stats ""}}))))

(defn game-started?
  "Returns true if game has started"
  [all-games gameid]
  (get-in @all-games [gameid :started]))

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

(defn update-player-stats
  "Update stats for player decks on game ending"
  [all-games gameid]
  (let [orig-players (get-in @all-games [gameid :original-players])
        end-players (get-in @all-games [gameid :ending-players])
        no-id (response 409 {:message "Deck is missing _id"})] ;likely needs to be ending playe

    (doseq [p orig-players]
      (when-let [enable-deckstats (get-in p [:user :options :deckstats])]
        (if-let [deck-id (get-in p [:deck :_id])]
          (inc-deck-stats deck-id :stats.games-started)
          no-id)))
    (doseq [p end-players]
      (when-let [enable-deckstats (get-in p [:user :options :deckstats])]
        (if-let [deck-id (get-in p [:deck :_id])]
          (inc-deck-stats deck-id :stats.games-completed)
          no-id)))))

;; if response.state.corp.user and response.state.runner.user # have two users in the game
;; room = response.state.room
;; inc_corp_game_start(response.state.corp, room)
;; inc_runner_game_start(response.state.runner, room)
;; if response.state.winner # and someone won
;; inc_game_win(response.state[response.state.winner], room)
;; inc_game_loss(response.state[response.state.loser], room)
;; else if response.state["final-user"] # someone left before the game was won
;; final_side = response.state["final-user"].side
;; inc_game_final_user(response.state[final_side], room)