(ns web.lobby.db
  (:require
   [executor.core :as executor]
   [web.user :refer [active-user?]]))

(executor/reg-event-db
  :db/initialize
  (fn [_ _]
    {:lobbies {}
     :users {}}))

(defn chsk-uidport-open
  [db {uid :uid {user :user} :ring-req}]
  (when (active-user? user)
    (assoc-in db [:users uid] (assoc user :uid uid))))

(executor/reg-event-db
  :chsk/uidport-open
  [executor/unwrap]
  #'chsk-uidport-open)
