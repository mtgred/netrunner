(ns web.api-keys
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]])
  (:import org.bson.types.ObjectId))

(defn api-keys-handler [req]
  (if-let [user (:user req)]
    (response 200 (mc/find-maps db "api-keys" {:username (:username user)}))
    (response 401 {:message "Unauthorized"})))

(defn api-keys-create-handler [{{username :username} :user}]
  (if username
    (let [new-key (java.util.UUID/randomUUID)
          new-entry (mc/insert db "api-keys"
                               {:username username
                                :date (java.util.Date.)
                                :api-key new-key})]
      (if (acknowledged? new-entry)
        (if (acknowledged? (mc/update db "users" {:username username} {"$set" {:has-api-keys true}}))
          (response 201 {:message "Created API Key"})
          (response 500 {:message "Failed to update user info"}))
        (response 500 {:message "Failed to create API Key"})))
    (response 401 {:message "Unauthorized"})))

(defn api-keys-delete-handler [{{username :username} :user
                                {id :id}             :params}]
  (try
    (if (and username id)
      (if (acknowledged? (mc/remove db "api-keys" {:_id (object-id id) :username username}))
        (let [key-count (mc/count db "api-keys" {:username username})]
          (when-not (pos? key-count)
            (mc/update db "users" {:username username} {"$set" {:has-api-keys false}}))
          (response 200 {:message "Deleted"}))
        (response 403 {:message "Forbidden"}))
      (response 401 {:message "Unauthorized"}))
    (catch Exception ex
      (response 409 {:message "Unknown API Key"}))))
