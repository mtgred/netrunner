(ns web.admin
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [jinteki.utils :refer [superuser?]]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [monger.result :refer [acknowledged? updated-existing?]]
   [taoensso.carmine :as car :refer [wcar]]
   [web.app-state :as app-state]
   [web.mongodb :refer [->object-id]]
   [web.user :refer [active-user?]]
   [web.utils :refer [response]]
   [web.ws :as ws]))

(defn- last-ip-address
  "Legacy because I got it wrong the first time"
  [user]
  (or (:last-ip-address user) (:lastIpAddress user)))

(defmethod ws/-msg-handler :admin/announce
  admin--announce
  [{{:system/keys [ws] user :user} :ring-req
    {message :message} :?data
    reply-fn :?reply-fn}]
  (cond
    (not (superuser? user)) (reply-fn 403)
    (empty? message) (reply-fn 400)
    :else
    (do
      (doseq [uid (ws/connected-uids ws)]
        (ws/chsk-send! ws uid [:lobby/toast {:message message
                                             :type "warning"}]))
      (reply-fn 200))))

(defn news-create-handler [{:system/keys [db]
                            {item :item} :body}]
  (if-not (empty? item)
    (do
      (mc/insert db "news" {:_id (->object-id)
                            :item item
                            :date (inst/now)})
      (response 200 {:message "ok"}))
    (response 400 {:message "Missing news item"})))

(defn news-delete-handler
  [{:system/keys [db]
    {id :id} :path-params}]
  (try
    (if id
      (if (acknowledged? (mc/remove db "news" {:_id (->object-id id)}))
        (response 200 {:message "Deleted"})
        (response 403 {:message "Forbidden"}))
      (response 400 {:message "Missing new items id"}))
    (catch Exception _
      (response 409 {:message "Unknown news item id"}))))

(defn version-handler [{:system/keys [db]}]
  (let [config (mc/find-one-as-map db "config" nil)
        version (:version config "0.0")]
    (response 200 {:message "ok" :version version})))

(defn version-update-handler [{:system/keys [db redis]
                               {version :version} :body}]
  (if-not (empty? version)
    (do
      (wcar redis (car/set :frontend/version version))
      (mc/update db "config" {} {$set {:version version}})
      (response 200 {:message "ok" :version version}))
    (response 400 {:message "Missing version item"})))

(defn banned-message-handler [{:system/keys [db redis]}]
  (let [msg (or (wcar redis (car/get :config/banned-msg))
              (:banned-msg (mc/find-one-as-map db "config" nil))
              "Account is locked")]
    (response 200 {:message "ok" :banned msg})))

(defn banned-message-update-handler [{:system/keys [db redis]
                                      {banned :banned} :body}]
  (if (str/blank? banned)
    (response 400 {:message "Missing banned message item"})
    (do
      (wcar redis (car/set :config/banned-msg banned))
      (mc/update db "config" {} {$set {:banned-msg banned}})
      (response 200 {:message "ok" :banned banned}))))

(def user-collection "users")

(def user-type->field
  {:mods :ismoderator
   :specials :special
   :tos :tournament-organizer
   :banned :banned})

(defmethod ws/-msg-handler :admin/edit-user
  admin--edit-user
  [{{:system/keys [db ws] user :user} :ring-req
    {:keys [action user-type username] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:isadmin user)
               (and (:ismoderator user)
                    (or (= user-type :specials)
                        (= user-type :tos)
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
        (do
          (ws/broadcast-to! ws [uid] :admin/user-edit {:success (assoc data :user user)})
          (when (= user-type :banned)
            (when-let [connected-user ((:users @app-state/app-state) username)]
              (ws/broadcast-to! ws [(:uid connected-user)] :system/force-disconnect {}))))
        (ws/broadcast-to! ws [uid] :admin/user-edit {:error "Not found"})))
    (ws/broadcast-to! ws [uid] :admin/user-edit {:error "Not allowed"})))

(def ip-ban-collection "ip-bans")

(defmethod ws/-msg-handler :admin/look-up-ip
  admin--look-up-ip
  [{{:system/keys [db ws] user :user} :ring-req
    {:keys [username] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (if-let [res (mc/find-one-as-map db user-collection {:username username}
                                         {:username 1
                                          :lastIpAddress 1
                                          :last-ip-address 1
                                          :_id 0})]
      (ws/broadcast-to! ws [uid] :admin/look-up-ip {:success (-> res
                                                               (dissoc :lastIpAddress)
                                                               (assoc :last-ip-address (last-ip-address res)))})
      (ws/broadcast-to! ws [uid] :admin/look-up-ip {:error "Not found"}))
    (ws/broadcast-to! ws [uid] :admin/look-up-ip {:error "Not allowed"})))

(defmethod ws/-msg-handler :admin/fetch-ip-bans
  admin--fetch-ip-bans
  [{{:system/keys [db ws] user :user} :ring-req
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (let [ip-bans (mc/find-maps db ip-ban-collection {}
                                {:username 1
                                 :ip-address 1
                                 :_id 0})]
      (ws/broadcast-to! ws [uid] :admin/fetch-ip-bans {:success ip-bans}))
    (ws/broadcast-to! ws [uid] :admin/fetch-ip-bans {:error "Not allowed"})))

(defmethod ws/-msg-handler :admin/ip-ban-user
  admin--ip-ban-user
  [{{:system/keys [db ws] user :user} :ring-req
    {:keys [username] :as data} :?data
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (if-let [res (mc/find-one-as-map db user-collection {:username username}
                                         {:username 1
                                          :lastIpAddress 1
                                          :last-ip-address 1
                                          :_id 0})]
      (if-let [ip (last-ip-address res)]
        (do
          (prn "res: " res)
          (prn "ip: " ip)
          (mc/insert db ip-ban-collection {:username username :ip-address ip})
          (ws/broadcast-to! ws [uid] :admin/ip-ban-user {:success {:username username
                                                                :ip-address ip}}))
        (ws/broadcast-to! ws [uid] :admin/ip-ban-user {:error "Legacy user? No IP Address on record"}))
      (ws/broadcast-to! ws [uid] :admin/ip-ban-user {:error "Not found"}))
    (ws/broadcast-to! ws [uid] :admin/ip-ban-user {:error "Not allowed"})))

(defmethod ws/-msg-handler :admin/ip-unban-user
  admin--ip-unban-user
    [{{:system/keys [db ws] user :user} :ring-req
    {:keys [username] :as data} :?data
      uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (let [result (mc/remove db ip-ban-collection {:username username})
          n (.getN result)]
      (if (pos? n)
        (ws/broadcast-to! ws [uid] :admin/ip-unban-user {:success username})
        (ws/broadcast-to! ws [uid] :admin/ip-unban-user {:error "Not found"})))
    (ws/broadcast-to! ws [uid] :admin/ip-unban-user {:error "Not allowed"})))

(defmethod ws/-msg-handler :admin/fetch-users
  admin--fetch-users
  [{{:system/keys [db ws] user :user} :ring-req
    uid :uid}]
  (if (and (active-user? user)
           (or (:ismoderator user) (:isadmin user)))
    (let [users (->> (mc/find-maps db user-collection {$or [{:ismoderator true}
                                                            {:special {$exists true}}
                                                            {:tournament-organizer true}
                                                            {:banned true}]}
                                   [:_id :username :ismoderator :special :tournament-organizer :banned])
                     (map #(update % :_id str)))]
      (ws/broadcast-to! ws [uid] :admin/fetch-users {:success users}))
    (ws/broadcast-to! ws [uid] :admin/fetch-users {:error "Not allowed"})))

(defmethod ws/-msg-handler :admin/block-game-creation
  admin--block-game-creation
  [{{user :user} :ring-req
    block? :?data
    ?reply-fn :?reply-fn}]
  (when (and (active-user? user)
             (or (:ismoderator user) (:isadmin user)))
    (let [block? (boolean block?)]
      (swap! app-state/app-state assoc :block-game-creation block?)
      (when ?reply-fn (?reply-fn block?)))))
