(ns web.db
  (:require [monger.core :as mg]
            [web.config :refer [server-config]])

  (:import org.bson.types.ObjectId))

(defn connect[]
  (let [{:keys [address port connection-string name]} (:db server-config)
        connection (mg/connect-via-uri (or connection-string
                                           (str "mongodb://" address ":" port "/" name)))]
    (defonce conn (:conn connection))
    (defonce db (:db connection))))

(defn disconnect []
  (mg/disconnect conn))

(defn object-id [id]
  (if (string? id)
    (org.bson.types.ObjectId. id)
    id))
