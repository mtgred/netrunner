(ns web.angelarena
  (:require [clojure.string :refer [lower-case]]
            [web.ws :as ws]
            [web.utils :refer [response json-response]]
            [monger.collection :as mc]
            [monger.operators :refer :all]))

(defn fetch-run
  [{db :system/db
    {username :username} :user}]
  (if username
    (let [{:keys [angelarena-run]}
          (mc/find-one-as-map db "users" {:username username} ["angelarena-run"])]
      (json-response 200 (or angelarena-run {:corp nil :runner nil})))
    (response 401 {:message "Unauthorized"})))

(defmethod ws/-msg-handler :angelarena/start-run
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    client-id :client-id
    {:keys [side deckid]} :?data}]
  (try
    (if username
      (let [{:keys [angelarena-run]}
            (mc/find-one-as-map db "users" {:username username} ["angelarena-run"])
            angelarena-run (or angelarena-run {:corp nil :runner nil})
            side (keyword (lower-case side))]
        (when-not (side angelarena-run)
          (mc/update db "users"
                     {:username username}
                     {"$set" {:angelarena-run
                              (assoc angelarena-run side
                                     {:deckid deckid
                                      :wins 0
                                      :losses 0
                                      :run-started (java.util.Date.)})}}))))))

(defmethod ws/-msg-handler :angelarena/abandon-run
  [{{db :system/db
     {:keys [username]} :user} :ring-req
    client-id :client-id
    {:keys [side]} :?data}]
  (try
    (if username
      (let [{:keys [angelarena-run]}
            (mc/find-one-as-map db "users" {:username username} ["angelarena-run"])
            angelarena-run (or angelarena-run {:corp nil :runner nil})
            side (keyword (lower-case side))]
        (when (side angelarena-run)
          (mc/update db "users"
                     {:username username}
                     {"$set" {:angelarena-run
                              (assoc angelarena-run side nil)}}))))))
