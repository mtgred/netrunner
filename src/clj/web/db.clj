(ns web.db
  (:require [monger.collection]
            [monger.core :as mg]
            [monger.cursor]
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

(defn- create-collation
  [locale strength]
  ;; This feels clumsy but there seems to be no support yet in Monger for doing
  ;; this without falling back to the Java API.
  (-> (com.mongodb.client.model.Collation/builder)
      (.locale locale)
      (.collationStrength (com.mongodb.client.model.CollationStrength/fromInt strength))
      (.build)))

(defn find-one-as-map-case-insensitive
  "Returns a single object converted to Map from this collection matching the query.

  Like `monger.collection/find-one-as-map`, but case-insensitive."
  [db coll query]
  (-> (monger.collection/find db coll query)
      (.setCollation (create-collation "en" 2))
      (.limit 1)
      (monger.cursor/format-as :map)
      (first)))

(defn find-maps-case-insensitive
  "Queries for objects in this collection, returning clojure Seq of Maps.

  Like `monger.collection/find-maps`, but case-insensitive."
  [db coll query]
  (-> (monger.collection/find db coll query)
      (.setCollation (create-collation "en" 2))
      (monger.cursor/format-as :map)))
