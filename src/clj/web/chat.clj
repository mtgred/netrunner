(ns web.chat
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as s]
   [monger.collection :as mc]
   [monger.query :as q]
   [web.app-state :as app-state]
   [web.lobby :as lobby]
   [web.mongodb :refer [->object-id]]
   [web.user :refer [active-user? visible-to-user]]
   [web.utils :refer [response mongo-time-to-utc-string]]
   [web.ws :as ws]
   [taoensso.timbre :as timbre]))

(def msg-collection "messages")
(def log-collection "moderator_actions")

(defn- chat-max-length [chat-settings] (:max-length chat-settings 144))

(defn config-handler
  [{chat-settings :system/chat}]
  (response 200 {:max-length (chat-max-length chat-settings)}))

(defn- blocked-by-user
  [db username]
    (let [blocks
          (mc/find-one-as-map db :users
                              {:username username}
                              ["username" "options.blocked-users"])]
      [username blocks]))

(defn messages-handler
  [{db :system/db
    user :user
    {:keys [channel]} :path-params :as args}]
  (if user
    (let [messages (->> (q/with-collection
                          db msg-collection
                          (q/find {:channel channel})
                          (q/sort (array-map :date -1))
                          (q/limit 100))
                        reverse)
          messages (map #(update % :date mongo-time-to-utc-string) messages)
          senders (->> messages
                       (map :username)
                       (map #(blocked-by-user db %))
                       (into {}))
          visible-users (->> (for [username (keys senders)
                                   :when (or (= (:username user) username)
                                             (visible-to-user user {:username username} senders))]
                               username)
                             (into #{}))
          messages (filter #(contains? visible-users (:username %)) messages)]
      (response 200 messages))
    (response 200 [])))

(defn- within-rate-limit
  [db chat-settings username]
  (let [window (:rate-window chat-settings 60)
        start-date (inst/plus-seconds (inst/now) (- window))
        max-cnt (:rate-cnt chat-settings 10)
        msg-cnt (mc/count db msg-collection {:username username
                                             :date {"$gt" start-date}})]
    (< msg-cnt max-cnt)))

(defmethod ws/-msg-handler :chat/say
  chat--say
  [{{db :system/db
     chat-settings :system/chat
     user :user} :ring-req
    uid :uid
    {:keys [channel msg]} :?data
    id :id
    timestamp :timestamp}]
  (when (and (active-user? user)
             (not (s/blank? msg)))
    (let [len-valid (<= (count msg) (chat-max-length chat-settings))
          rate-valid (within-rate-limit db chat-settings (:username user))]
      (if (and len-valid rate-valid)
        (let [message {:emailhash (:emailhash user)
                       :username  (:username user)
                       :pronouns  (-> user :options :pronouns)
                       :msg       msg
                       :channel   channel
                       :date      (inst/now)}
              inserted (mc/insert-and-return db msg-collection message)
              inserted (update inserted :_id str)
              inserted (update inserted :date #(.toString %))
              connected-users (app-state/get-users)]
          (doseq [uid (ws/connected-uids)
                  :when (or (= (:username user) uid)
                            (visible-to-user user {:username uid} connected-users))]
            (ws/broadcast-to! [uid] :chat/message inserted)))
        (when uid
          (ws/broadcast-to! [uid] :chat/blocked {:reason (if len-valid :rate-exceeded :length-exceeded)})))))
  (lobby/log-delay! timestamp id))

(defmethod ws/-msg-handler :chat/delete-msg
  chat--delete-msg
  [{{db :system/db
     {:keys [username isadmin ismoderator] :as user} :user} :ring-req
    {:keys [msg]} :?data
    id :id
    timestamp :timestamp}]
  (when-let [id (:_id msg)]
    (when (or isadmin ismoderator)
      (timbre/info {:type :mod-action} (str username "deleted message" msg "\n"))
      (mc/remove-by-id db msg-collection (->object-id id))
      (mc/insert db log-collection
                 {:moderator username
                  :action :delete-message
                  :date (inst/now)
                  :msg msg})
      (doseq [uid (ws/connected-uids)]
        (ws/broadcast-to! [uid] :chat/delete-msg msg))))
  (lobby/log-delay! timestamp id))

(defmethod ws/-msg-handler :chat/delete-all
  chat--delete-all
  [{{db :system/db
     {:keys [username isadmin ismoderator]} :user :as user} :ring-req
    {:keys [sender]} :?data
    id :id
    timestamp :timestamp}]
  (when (and sender
             (or isadmin ismoderator))
    (timbre/info {:type :mod-action} (str username "deleted all messages from user" sender "\n"))
    (mc/remove db msg-collection {:username sender})
    (mc/insert db log-collection
               {:moderator username
                :action :delete-all-messages
                :date (inst/now)
                :sender sender})
    (doseq [uid (ws/connected-uids)]
      (ws/broadcast-to! [uid] :chat/delete-all {:username sender})))
  (lobby/log-delay! timestamp id))
