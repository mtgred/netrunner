(ns web.stats
  (:require [web.db :refer [db object-id]]
            [monger.collection :as mc]
            [web.utils :refer [response]])
  (:import org.bson.types.ObjectId))

(defn clear-user-stats
  [req]
  (when-let [user-id (-> req :user :_id)]
    (mc/update db "users" {:_id (object-id user-id)} {"$unset" {:stats ""}})
    (response 200 {:message "OK"})))