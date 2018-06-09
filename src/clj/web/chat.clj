(ns web.chat
  (:require [buddy.sign.jwt :as jwt]
            [clojure.string :as s]
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

(defn- insert-msg [{{{:keys [username emailhash]} :user} :ring-req
                    {:keys [:channel :msg]} :?data :as event}]
  (when (and (not (s/blank? msg))
             (<= (count msg) (:max-length chat-config 144)))
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

