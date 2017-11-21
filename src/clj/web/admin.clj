(ns web.admin
  (:require [web.db :refer [db object-id]]
            [web.lobby :refer [all-games]]
            [game.main :as main]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [web.config :refer [frontend-version]]))

(defn wrap-version [handler]
  (fn [request]
    (handler (assoc request :version @frontend-version))))

(defn announcement-handler
  [{{:keys [message]} :params :as req}]
  (doseq [{state :state} (vals @all-games)]
    (when state
      (main/handle-announcement state message)))
  (response 200 {:message "ok"}))

(defn version-handler
  [{{:keys [version]} :params :as req}]
  (reset! frontend-version version)
  (mc/update db "config" {} {$set {:version version}})
  (response 200 {:message "ok" :version version}))
