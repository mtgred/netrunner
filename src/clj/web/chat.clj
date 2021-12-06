(ns web.chat
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as s]
   [monger.collection :as mc]
   [monger.query :as q]
   [web.app-state :as app-state]
   [web.config :refer [server-config]]
   [web.mongodb :refer [->object-id]]
   [web.user :refer [active-user? visible-to-user]]
   [web.utils :refer [response]]
   [web.ws :as ws]))

(defonce chat-config (:chat server-config))
(def msg-collection "messages")
(def log-collection "moderator_actions")

(defn- chat-max-length [] (:max-length chat-config 144))

(defn config-handler [_]
  (response 200 {:max-length (chat-max-length)}))

(defn messages-handler
  [{db :system/db
    user :user
    {:keys [channel]} :params}]
  (if user
    (let [messages (->> (q/with-collection
                          db msg-collection
                          (q/find {:channel channel})
                          (q/sort (array-map :date -1))
                          (q/limit 100))
                        reverse)
          usernames (->> messages
                         (map :username)
                         (into #{}))
          connected-users (app-state/get-users)
          visible-users (->> (for [username usernames
                                   :when (or (= (:username user) username)
                                             (visible-to-user user {:username username} connected-users))]
                               username)
                             (into #{}))
          messages (filter #(contains? visible-users (:username %)) messages)]
      (response 200 messages))
    (response 200 [])))

(defn- within-rate-limit
  [db username]
  (let [window (:rate-window chat-config 60)
        start-date (inst/plus-seconds (inst/now) (- window))
        max-cnt (:rate-cnt chat-config 10)
        msg-cnt (mc/count db msg-collection {:username username :date {"$gt" start-date}})]
    (< msg-cnt max-cnt)))

(defmethod ws/-msg-handler :chat/say
  [{{db :system/db user :user} :ring-req
    uid :uid
    {:keys [channel msg]} :?data}]
  (when (and (active-user? user)
             (not (s/blank? msg)))
    (let [len-valid (<= (count msg) (chat-max-length))
          rate-valid (within-rate-limit db (:username user))]
      (if (and len-valid rate-valid)
        (let [message {:emailhash (:emailhash user)
                       :username  (:username user)
                       :pronouns  (-> user :options :pronouns)
                       :msg       msg
                       :channel   channel
                       :date      (inst/now)}
              inserted (mc/insert-and-return db msg-collection message)
              inserted (update inserted :_id str)
              connected-users (app-state/get-users)]
          (doseq [uid (keys connected-users)
                  :when (or (= (:username user) uid)
                            (visible-to-user user {:username uid} connected-users))]
            (ws/broadcast-to! [uid] :chat/message inserted)))
        (when uid
          (ws/broadcast-to! [uid] :chat/blocked {:reason (if len-valid :rate-exceeded :length-exceeded)}))))))

(defmethod ws/-msg-handler :chat/delete-msg
  [{{db :system/db
     {:keys [username isadmin ismoderator] :as user} :user} :ring-req
    {:keys [msg]} :?data}]
  (when-let [id (:_id msg)]
    (when (or isadmin ismoderator)
      (println username "deleted message" msg "\n")
      (mc/remove-by-id db msg-collection (->object-id id))
      (mc/insert db log-collection
                 {:moderator username
                  :action :delete-message
                  :date (inst/now)
                  :msg msg})
      (let [connected-users (app-state/get-users)]
        (doseq [uid (keys connected-users)
                :when (or (= (:username user) uid)
                          (visible-to-user user {:username uid} connected-users))]
          (ws/broadcast-to! [uid] :chat/delete-msg msg))))))


(defmethod ws/-msg-handler :chat/delete-all
  [{{db :system/db
     {:keys [username isadmin ismoderator]} :user :as user} :ring-req
    {:keys [sender]} :?data}]
  (when (and sender
             (or isadmin ismoderator))
    (println username "deleted all messages from user" sender "\n")
    (mc/remove db msg-collection {:username sender})
    (mc/insert db log-collection
               {:moderator username
                :action :delete-all-messages
                :date (inst/now)
                :sender sender})
    (let [connected-users (app-state/get-users)]
      (doseq [uid (keys connected-users)
              :when (or (= (:username user) uid)
                        (visible-to-user user {:username uid} connected-users))]
        (ws/broadcast-to! [uid] :chat/delete-all {:username sender})))))
