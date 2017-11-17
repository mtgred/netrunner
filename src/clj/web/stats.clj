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