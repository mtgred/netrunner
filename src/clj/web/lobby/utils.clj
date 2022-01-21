(ns web.lobby.utils
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [medley.core :refer [find-first]]))

(defn uid->lobby
  "Returns the lobby if uid is a player or spectator"
  [lobbies uid]
  (find-first
    (fn [lobby]
      (some #(= uid (:uid %)) (into (:players lobby) (:spectators lobby))))
    (vals lobbies)))

(defn uid-player->lobby
  "Returns the lobby if uid is a player"
  [lobbies uid]
  (find-first
    (fn [lobby]
      (some #(= uid (:uid %)) (:players lobby)))
    (vals lobbies)))

(defn uid-original-player->lobby
  "Returns the lobby if uid is an original player"
  [lobbies uid]
  (find-first
    (fn [lobby]
      (some #(= uid (:uid %)) (:original-players lobby)))
    (vals lobbies)))

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

(defn uid->user
  [db uid]
  (-> db :users (get uid)))

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

(defn get-players-and-spectators [lobby]
  (concat (:players lobby) (:spectators lobby)))
