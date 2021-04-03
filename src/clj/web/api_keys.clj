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
    (let [new-key (java.util.UUID/randomUUID)]
      (response 200 (mc/insert-and-return db "api-keys"
                                          {:username username
                                           :date (java.util.Date.)
                                           :api-key new-key})))
    (response 401 {:message "Unauthorized"})))

(defn api-keys-delete-handler [{{username :username} :user
                                {id :id}             :params}]
  (try
    (if (and username id)
      (if (acknowledged? (mc/remove db "api-keys" {:_id (object-id id) :username username}))
        (response 200 {:message "Deleted"})
        (response 403 {:message "Forbidden"}))
      (response 401 {:message "Unauthorized"}))
    (catch Exception ex
      (response 409 {:message "Unknown API Key"}))))
