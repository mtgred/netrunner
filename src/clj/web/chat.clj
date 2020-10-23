(ns web.chat
  (:require [buddy.sign.jwt :as jwt]
            [clojure.string :as s]
            [clj-time.core :as t]
            [clj-time.coerce :as c]
            [monger.query :as q]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [web.db :refer [db]]
            [web.utils :refer [response]]
            [web.ws :as ws])
  (:import org.bson.types.ObjectId))

(defonce chat-config (:chat server-config))
(def msg-collection "messages")
(def log-collection "moderator_actions")

(defn- chat-max-length [] (:max-length chat-config 144))

(defn config-handler [req]
  (response 200 {:max-length (chat-max-length)}))

(defn messages-handler [{{:keys [channel]} :params}]
  (response 200 (reverse (q/with-collection db msg-collection
                                            (q/find {:channel channel})
                                            (q/sort (array-map :date -1))
                                            (q/limit 100)))))

(defn- within-rate-limit
  [username]
  (let [window (:rate-window chat-config 60)
        start-date (c/to-date (t/plus (t/now) (t/seconds (- window))))
        max-cnt (:rate-cnt chat-config 10)
        msg-cnt (mc/count db msg-collection {:username username :date {"$gt" start-date}})]
    (< msg-cnt max-cnt)))

(defn- insert-msg [{{{:keys [username emailhash]} :user} :ring-req
                    client-id :client-id
                    {:keys [:channel :msg]} :?data :as event}]
  (let [len-valid (<= (count msg) (chat-max-length))
        rate-valid (within-rate-limit username)]
    (when (and username
               emailhash
               (not (s/blank? msg)))
      (if (and len-valid rate-valid)
        (let [message {:emailhash emailhash
                       :username  username
                       :msg       msg
                       :channel   channel
                       :date      (java.util.Date.)}
              inserted (mc/insert-and-return db msg-collection message)]
          (update inserted :_id str))
        (do
          (when client-id
            (ws/broadcast-to! [client-id] :chat/blocked {:reason (if len-valid :rate-exceeded :length-exceeded)}))
          nil)))))

(defn broadcast-msg
  [arg]
  (when-let [msg (insert-msg arg)]
    (ws/broadcast! :chat/message msg)))

(defn- delete-msg [{{{:keys [username isadmin ismoderator]} :user} :ring-req
                    {:keys [msg]} :?data :as event}]
  (when-let [id (:_id msg)]
    (when (or isadmin ismoderator)
      (println username "deleted message" msg "\n")
      (mc/remove-by-id db msg-collection (ObjectId. id))
      (mc/insert db log-collection
                 {:moderator username
                  :action :delete-message
                  :date (java.util.Date.)
                  :msg msg})
      (ws/broadcast! :chat/delete-msg msg))))

(defn- delete-all-msg [{{{:keys [username isadmin ismoderator]} :user} :ring-req
                        {:keys [sender]} :?data :as event}]
  (when (and sender
             (or isadmin ismoderator))
      (println username "deleted all messages from user" sender "\n")
      (mc/remove db msg-collection {:username sender})
      (mc/insert db log-collection
                 {:moderator username
                  :action :delete-all-messages
                  :date (java.util.Date.)
                  :sender sender})
      (ws/broadcast! :chat/delete-all {:username sender})))

(ws/register-ws-handlers!
  :chat/say broadcast-msg
  :chat/delete-msg delete-msg
  :chat/delete-all delete-all-msg)
