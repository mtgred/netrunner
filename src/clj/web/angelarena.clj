(ns web.angelarena
  (:require [clojure.string :refer [lower-case]]
            [game.utils :refer [in-coll?]]
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
  (when username
    (try
      (let [{:keys [angelarena-run]} (mc/find-one-as-map db "users" {:username username} ["angelarena-run"])
            angelarena-run (or angelarena-run {:corp nil :runner nil})]
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
  (when username
    (try
      (let [{:keys [angelarena-run]} (mc/find-one-as-map db "users" {:username username} ["angelarena-run"])
            angelarena-run (or angelarena-run {:corp nil :runner nil})]
        (when (side angelarena-run)
          (mc/update db "users"
                     {:username username}
                     {"$set" {:angelarena-run
                              (assoc angelarena-run side nil)}}))))))

(defonce arena-queue (atom {:corp [] :runner []}))

(defmethod ws/-msg-handler :angelarena/queue
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id
    {:keys [side]} :?data}]
  (when (and username
             (empty? (filter #(= username (:username %)) (:corp @arena-queue)))
             (empty? (filter #(= username (:username %)) (:runner @arena-queue))))
    (try
      (let [other-side (if (= :corp side) :runner :corp)
            players-not-blocking-user (remove #(in-coll?
                                                 (get-in % [:options :blocked-users])
                                                 username)
                                              (other-side @arena-queue))]
        (println players-not-blocking-user)
        (println (remove #(in-coll?
                            (get-in user [:options :blocked-users])
                            (get-in % [:username]))
                         players-not-blocking-user))
        (swap! arena-queue update side conj user)
        (println "queue now:" @arena-queue)))))

(defmethod ws/-msg-handler :angelarena/dequeue
  [{{db :system/db
     {:keys [username] :as user} :user} :ring-req
    client-id :client-id}]
  (when username
    (try
      (swap! arena-queue update :corp (partial remove #(= username (:username %))))
      (swap! arena-queue update :runner (partial remove #(= username (:username %))))
      (println "queue now:" @arena-queue))))
