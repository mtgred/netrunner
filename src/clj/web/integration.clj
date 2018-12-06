(ns web.integration
  (:require [clojure.string :as s]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [monger.query :as q]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [web.db :refer [db]]
            [web.utils :refer [response]]
            [cheshire.core :as json]
            [web.ws :as ws])
  (:import org.bson.types.ObjectId))

(def api-key-collection "api_keys")

(defn- send-keys
  [username client-id]
  (when client-id
    (let [raw-keys (reverse (q/with-collection db api-key-collection
                              (q/find {:username username})
                              (q/sort (array-map :date -1))
                              (q/limit 10)))
          api-keys (->> raw-keys
                        (map #(select-keys % [:name :_id]))
                        (map #(update % :_id (fn [x] (.toString x))))
                        vec)]
      (ws/send! client-id [:integration/key-list {:keys api-keys}]))))

(defn list-keys
  [{{{:keys [username]} :user} :ring-req
    client-id :client-id}]
  (send-keys username client-id))

(defn create-key
  [{{{:keys [username]} :user} :ring-req
    client-id :client-id}]
  (mc/insert db api-key-collection
             {:username username
              :date (java.util.Date.)
              :name "A key value"})
  (send-keys username client-id))

(defn delete-key
  [{{{:keys [username]} :user} :ring-req
    client-id :client-id
  {:keys [_id]} :?data :as event}]
  (mc/remove db api-key-collection {:_id (ObjectId. _id)
                                    :username username})
  (send-keys username client-id))

(ws/register-ws-handlers!
  :integration/list-keys list-keys
  :integration/delete-key delete-key
  :integration/create-key create-key)
