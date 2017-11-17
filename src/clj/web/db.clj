(ns web.db
  (:require [aero.core :refer [read-config]]
            [monger.core :as mg]
            [jinteki.config :refer [server-config]])

  (:import org.bson.types.ObjectId))

(defn connect[]
  (let [{:keys [address port connection-string name]} (:db server-config)
        connection (mg/connect-via-uri (or connection-string
                                           (str "mongodb://" address ":" port "/" name)))]
    (defonce conn (:conn connection))
    (defonce db (:db connection))))

(defn object-id [id]
  (org.bson.types.ObjectId. id))