(ns web.integration
  (:require [monger.query :as q]
            [monger.collection :as mc]
            [web.db :refer [db]]
            [web.tokens :as tokens]
            [web.ws :as ws])
  (:import org.bson.types.ObjectId))

(def api-key-collection "api_keys")
(def ^:const max-keys 5)

(defn- send-keys
  [emailhash client-id]
  (when client-id
    (let [raw-keys (reverse (q/with-collection db api-key-collection
                              (q/find {:emailhash emailhash})
                              (q/sort (array-map :date -1))
                              (q/limit max-keys)))
          api-keys (->> raw-keys
                        (map #(select-keys % [:token :_id :issued :expires]))
                        (map #(update % :_id (fn [x] (.toString x))))
                        vec)]
      (ws/send! client-id [:integration/key-list {:keys api-keys}]))))

(defn list-keys
  [{{{:keys [emailhash]} :user} :ring-req
    client-id :client-id}]
  (send-keys emailhash client-id))

(defn delete-key
  [{{{:keys [emailhash]} :user} :ring-req
    client-id :client-id
  {:keys [_id]} :?data :as event}]
  (mc/remove db api-key-collection {:_id (ObjectId. _id)
                                    :emailhash emailhash})
  (send-keys emailhash client-id))

(defn create-key
  [{{{:keys [emailhash]} :user} :ring-req
    client-id :client-id}]
  (when (< (mc/count db api-key-collection {:emailhash emailhash}) max-keys)
    (let [{:keys [token public-key issued expires]} (tokens/create-api-token emailhash)]
      (mc/insert db api-key-collection
                 {:emailhash emailhash
                  :token token
                  :public-key public-key
                  :issued (.toString issued)
                  :expires (.toString expires)})
      (send-keys emailhash client-id))))

(ws/register-ws-handlers!
  :integration/list-keys list-keys
  :integration/delete-key delete-key
  :integration/create-key create-key)
