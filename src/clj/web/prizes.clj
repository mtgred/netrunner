(ns web.prizes
  (:require
   [cheshire.core :as json]
   [clj-uuid :as uuid]
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [jinteki.utils :refer [str->int]]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.result :refer [acknowledged?]]
   [org.httpkit.client :as http]
   [web.user :refer [active-user?]]
   [web.mongodb :refer [find-maps-case-insensitive]]
   [web.stats :refer [fetch-elapsed]]
   [web.utils :refer [response]]
   [web.ws :as ws]))

(defn auth [_]
  (response 200 {:message "ok"}))

(defmethod ws/-msg-handler :prizes/load-user
  prizes--load-user
  [{{db :system/db user :user} :ring-req
    {:keys [username] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (if-let [user (mc/find-one-as-map db "users" {:username username} [:_id :username :options])]
      (let [user (dissoc (assoc user :prizes (-> user :options :prizes)) :options)]
        (ws/broadcast-to! [uid] :prizes/load-user {:success (update user :_id str)}))
      (ws/broadcast-to! [uid] :prizes/load-user {:error "No such user"}))
    (ws/broadcast-to! [uid] :prizes/load-user {:error "Not allowed"})))

(defmethod ws/-msg-handler :prizes/update-user
  prizes--update-user
  [{{db :system/db user :user} :ring-req
    {:keys [username prizes] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (if-let [user (mc/find-one-as-map db "users" {:username username} [:_id :username :options])]
      (let [new-options (assoc (:options user) :prizes prizes)]
        (if (acknowledged? (mc/update db "users"
                                      {:username username}
                                      {"$set" {:options new-options}}))
          (ws/broadcast-to! [uid] :prizes/update-user {:success (str "updated prizes for " username)})
          (ws/broadcast-to! [uid] :prizes/update-user {:error (str "failed updating prizes for " username)})))
      (ws/broadcast-to! [uid] :prizes/update-user {:error (str "failed updating prizes for " username)}))
    (ws/broadcast-to! [uid] :prizes/update-user {:error (str "failed updating prizes for " username)})))
