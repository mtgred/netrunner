(ns web.api-keys
  (:require
   [cljc.java-time.instant :as inst]
   [monger.collection :as mc]
   [monger.result :refer [acknowledged?]]
   [web.mongodb :refer [->object-id]]
   [web.utils :refer [response]]))

(defn api-keys-handler [{db :system/db
                         {username :username} :user}]
  (if username
    (response 200 (mc/find-maps db "api-keys" {:username username}))
    (response 401 {:message "Unauthorized"})))

(defn api-keys-create-handler [{db :system/db
                                {username :username} :user}]
  (if username
    (let [new-key (java.util.UUID/randomUUID)
          new-entry (mc/insert db "api-keys"
                               {:username username
                                :date (inst/now)
                                :api-key new-key})]
      (if (acknowledged? new-entry)
        (if (acknowledged? (mc/update db "users" {:username username} {"$set" {:has-api-keys true}}))
          (response 201 {:message "Created API Key"})
          (response 500 {:message "Failed to update user info"}))
        (response 500 {:message "Failed to create API Key"})))
    (response 401 {:message "Unauthorized"})))

(defn api-keys-delete-handler [{db :system/db
                                {username :username} :user
                                {id :id}             :params}]
  (try
    (if (and username id)
      (if (acknowledged? (mc/remove db "api-keys" {:_id (->object-id id) :username username}))
        (let [key-count (mc/count db "api-keys" {:username username})]
          (when-not (pos? key-count)
            (mc/update db "users" {:username username} {"$set" {:has-api-keys false}}))
          (response 200 {:message "Deleted"}))
        (response 403 {:message "Forbidden"}))
      (response 401 {:message "Unauthorized"}))
    (catch Exception _
      (response 409 {:message "Unknown API Key"}))))
