(ns web.prizes
  (:require
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.result :refer [acknowledged?]]
   [web.user :refer [active-user?]]
   [web.utils :refer [response]]
   [web.ws :as ws]))

(defn auth [_]
  (response 200 {:message "ok"}))

(defmethod ws/-msg-handler :prizes/load-user
  prizes--load-user
  [{{:system/keys [db ws] user :user} :ring-req
    {:keys [username] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (if-let [user (mc/find-one-as-map db "users" {:username username} [:_id :username :options])]
      (let [user (dissoc (assoc user :prizes (-> user :options :prizes)) :options)]
        (ws/broadcast-to! ws [uid] :prizes/load-user {:success (update user :_id str)}))
      (ws/broadcast-to! ws [uid] :prizes/load-user {:error "No such user"}))
    (ws/broadcast-to! ws [uid] :prizes/load-user {:error "Not allowed"})))

(defmethod ws/-msg-handler :prizes/update-user
  prizes--update-user
  [{{:system/keys [db ws] user :user} :ring-req
    {:keys [username prizes] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (if-let [user (mc/find-one-as-map db "users" {:username username} [:_id :username :options])]
      (let [new-options (assoc (:options user) :prizes prizes)]
        (if (acknowledged? (mc/update db "users"
                                      {:username username}
                                      {"$set" {:options new-options}}))
          (ws/broadcast-to! ws [uid] :prizes/update-user {:success (str "updated prizes for " username)})
          (ws/broadcast-to! ws [uid] :prizes/update-user {:error (str "failed updating prizes for " username)})))
      (ws/broadcast-to! ws [uid] :prizes/update-user {:error (str "failed updating prizes for " username)}))
    (ws/broadcast-to! ws [uid] :prizes/update-user {:error (str "failed updating prizes for " username)})))
