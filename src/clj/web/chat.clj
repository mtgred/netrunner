(ns web.chat
  (:require [buddy.sign.jwt :as jwt]
            [monger.query :refer :all]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.db :refer [db]]
            [web.utils :refer [response]]
            [web.ws :as ws])
  (:import org.bson.types.ObjectId))

(defn messages-handler [{{:keys [channel]} :params}]
  (response 200 (reverse (with-collection db "messages"
                                          (find {:channel channel})
                                          (sort (array-map :date -1))
                                          (limit 100)))))

(defn post-message [{{{:keys [username emailhash]} :user} :ring-req
                     {:keys [:channel :msg]} :?data :as event}]
  (let [message {:emailhash emailhash
                 :username  username
                 :msg       msg
                 :channel   channel
                 :date      (java.util.Date.)}]
    (mc/insert db "messages" message)
    message))


(ws/register-ws-handler!
  :chat/say
  #(ws/broadcast! :chat/message (post-message %)))

