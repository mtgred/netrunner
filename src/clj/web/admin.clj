(ns web.admin
  (:require
   [cljc.java-time.instant :as inst]
   [jinteki.utils :refer [superuser?]]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.result :refer [acknowledged? updated-existing?]]
   [web.app-state :as app-state]
   [web.mongodb :refer [->object-id]]
   [web.user :refer [active-user?]]
   [web.utils :refer [response]]
   [web.versions :refer [frontend-version]]
   [web.ws :as ws]))

(defmethod ws/-msg-handler :admin/announce
  [{{user :user} :ring-req
    {message :message} :?data
    reply-fn :?reply-fn}]
  (cond
    (not (superuser? user)) (reply-fn 403)
    (empty? message) (reply-fn 400)
    :else
    (do
      (doseq [u (app-state/get-users)
              :let [uid (:uid u)]]
        (ws/chsk-send! uid [:lobby/toast {:message message
                                          :type "warning"}]))
      (reply-fn 200))))

(defn news-create-handler [{db :system/db
                            {item :item} :body}]
  (if-not (empty? item)
    (do
      (mc/insert db "news" {:_id (->object-id)
                            :item item
                            :date (inst/now)})
      (response 200 {:message "ok"}))
    (response 400 {:message "Missing news item"})))

(defn news-delete-handler
  [{db :system/db
    {id :id} :path-params}]
  (try
    (if id
      (if (acknowledged? (mc/remove db "news" {:_id (->object-id id)}))
        (response 200 {:message "Deleted"})
        (response 403 {:message "Forbidden"}))
      (response 400 {:message "Missing new items id"}))
    (catch Exception _
      (response 409 {:message "Unknown news item id"}))))

(defn version-handler [{db :system/db}]
  (let [config (mc/find-one-as-map db "config" nil)
        version (:version config "0.0")]
    (response 200 {:message "ok" :version version})))

(defn version-update-handler [{db :system/db
                               {version :version} :body}]
  (if-not (empty? version)
    (do
      (reset! frontend-version version)
      (mc/update db "config" {} {$set {:version version}})
      (response 200 {:message "ok" :version version}))
    (response 400 {:message "Missing version item"})))

(def user-collection "users")

(def user-type->field
  {:mods :ismoderator
   :specials :special
   :tos :tournament-organizer
   :banned :banned})

(defmethod ws/-msg-handler :admin/edit-user
  [{{db :system/db user :user} :ring-req
    {:keys [action user-type username] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:isadmin user)
               (and (:ismoderator user)
                    (or (= user-type :specials)
                        (= user-type :banned))))
           (not-empty username))
    (let [field (user-type->field user-type)
          value (case action
                  :admin/add-user true
                  :admin/remove-user false
                  nil)
          updated (when (and field (some? value))
                    (updated-existing? (mc/update db user-collection {:username username} {$set {field value}})))
          user (when updated
                 (-> (mc/find-one-as-map db user-collection {:username username} [:_id :username])
                     (update :_id str)))]
      (if user
        (ws/broadcast-to! [uid] :admin/user-edit {:success (assoc data :user user)})
        (ws/broadcast-to! [uid] :admin/user-edit {:error "Not found"})))
    (ws/broadcast-to! [uid] :admin/user-edit {:error "Not allowed"})))

(defmethod ws/-msg-handler :admin/fetch-users
  [{{db :system/db user :user} :ring-req
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (let [users (->> (mc/find-maps db user-collection {$or [{:ismoderator true}
                                                            {:special true}
                                                            {:tournament-organizer true}
                                                            {:banned true}]}
                                   [:_id :username :ismoderator :special :tournament-organizer :banned])
                     (map #(update % :_id str)))]
      (ws/broadcast-to! [uid] :admin/fetch-users {:success users}))
    (ws/broadcast-to! [uid] :admin/fetch-users {:error "Not allowed"})))

(defn features-handler [{db :system/db}]
  (let [config (mc/find-one-as-map db "config" nil)
        features (:features config {})]
    (response 200 {:message "ok" :features features})))

(defn features-update-handler [{db :system/db
                                {version :version} :body}]
  (if-not (empty? version)
    (do
      (reset! frontend-version version)
      (mc/update db "config" {} {$set {:version version}})
      (response 200 {:message "ok" :version version}))
    (response 400 {:message "Missing version item"})))
