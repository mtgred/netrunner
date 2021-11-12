(ns web.admin
  (:require
   [game.main :as main]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.query :as mq]
   [monger.result :refer [acknowledged? updated-existing?]]
   [web.config :refer [frontend-version]]
   [web.lobby :refer [all-games]]
   [web.mongodb :refer [->object-id object-id]]
   [web.user :refer [active-user?]]
   [web.utils :refer [response]]
   [web.ws :as ws]))

(defn announce-create-handler [{{message :message} :body}]
  (if-not (empty? message)
    (do
      (doseq [{state :state} (vals @all-games)]
        (when state
          (main/handle-announcement state message)))
      (response 200 {:message "ok"}))
    (response 400 {:message "Missing announcement"})))

(defn news-create-handler [{db :system/db
                            {item :item} :body}]
  (if-not (empty? item)
    (do
      (mc/insert db "news" {:_id (->object-id) :item item :date (java.util.Date.)})
      (response 200 {:message "ok"}))
    (response 400 {:message "Missing news item"})))

(defn news-delete-handler [{db :system/db
                            {id :id} :params}]
  (try
    (if id
      (if (acknowledged? (mc/remove db "news" {:_id (object-id id)}))
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
           (:isadmin user)
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
           (:isadmin user))
    (let [users (->> (mc/find-maps db user-collection {$or [{:ismoderator true}
                                                            {:specials true}
                                                            {:tournament-organizer true}
                                                            {:banned true}]}
                                   [:_id :username :ismoderator :specials :tournament-organizer :banned])
                     (map #(update % :_id str)))]
      (println users)
      (ws/broadcast-to! [uid] :admin/fetch-users {:success users}))
    (ws/broadcast-to! [uid] :admin/fetch-users {:error "Not allowed"})))

(defn- find-user [db criteria]
  (let [data (mq/with-collection db user-collection
               (mq/find criteria)
               (mq/fields [:_id :username])
               (mq/sort (array-map :username 1)))]
    (response 200 data)))

(defn- update-user [db criteria field value]
  (if (updated-existing? (mc/update db "users" criteria {$set {field value}}))
    (response 200 {:message "ok"})
    (response 404 {:message "Unknown user"})))

(defn mods-handler [{db :system/db}]
  (find-user db {:ismoderator true}))

(defn mods-update-handler [{db :system/db
                            {username :username} :body}]
  (if-not (empty? username)
    (update-user db {:username username} :ismoderator true)
    (response 400 {:message "Missing username"})))

(defn mods-delete-handler [{db :system/db
                            {id :id} :params}]
  (if id
    (update-user db {:_id (object-id id)} :ismoderator false)
    (response 400 {:message "Missing id"})))

(defn tos-handler [{db :system/db}]
  (find-user db {:tournament-organizer true}))

(defn tos-update-handler [{db :system/db
                           {username :username} :body}]
  (if-not (empty? username)
    (update-user db {:username username} :tournament-organizer true)
    (response 400 {:message "Missing username"})))

(defn tos-delete-handler [{db :system/db
                           {id :id} :params}]
  (if id
    (update-user db {:_id (object-id id)} :tournament-organizer false)
    (response 400 {:message "Missing id"})))

(defn specials-handler [{db :system/db}]
  (find-user db {:special {$exists true}}))

(defn specials-update-handler [{db :system/db
                                {username :username} :body}]
  (if-not (empty? username)
    (update-user db {:username username} :special true)
    (response 400 {:message "Missing username"})))

(defn specials-delete-handler [{db :system/db
                                {id :id} :params}]
  (if id
    (if (updated-existing? (mc/update db "users" {:_id (object-id id)} {$unset {:special false}}))
      (response 200 {:message "Removed"})
      (response 404 {:message "Unknown user"}))
    (response 400 {:message "Missing id"})))

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
