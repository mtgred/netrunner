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

(defn messages-handler [{{:keys [channel]} :params}]
  (response 200 (reverse (q/with-collection db "messages"
                                            (q/find {:channel channel})
                                            (q/sort (array-map :date -1))
                                            (q/limit 100)))))

(defn- within-rate-limit
  [username]
  (let [window (:rate-window chat-config 60)
        start-date (c/to-date (t/plus (t/now) (t/seconds (- window))))
        max-cnt (:rate-cnt chat-config 10)
        msg-cnt (mc/count db "messages" {:username username :date {"$gt" start-date}})]
    (< msg-cnt max-cnt)))

(defn- insert-msg [{{{:keys [username emailhash]} :user} :ring-req
                    {:keys [:channel :msg]} :?data :as event}]
  (when (and username
             emailhash
             (not (s/blank? msg))
             (<= (count msg) (:max-length chat-config 144))
             (within-rate-limit username))
    (let [message {:emailhash emailhash
                   :username  username
                   :msg       msg
                   :channel   channel
                   :date      (java.util.Date.)}]
      (mc/insert db "messages" message)
      message)))

(defn broadcast-msg
  [arg]
  (when-let [msg (insert-msg arg)]
    (ws/broadcast! :chat/message msg)))

(ws/register-ws-handler!
  :chat/say broadcast-msg)

