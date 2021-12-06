(ns web.mongodb
  (:require
   [cljc.java-time.local-date :as ld]
   [cljc.java-time.local-date-time :as ldt]
   [cljc.java-time.zone-id :as zid]
   [cljc.java-time.zone-offset :as zoff]
   [cljc.java-time.zoned-date-time :as zdt]
   [monger.collection]
   [monger.conversion :refer [ConvertFromDBObject ConvertToDBObject
                              from-db-object to-db-object]]
   [monger.cursor])
  (:import
   (java.time
     Instant
     LocalDate
     LocalDateTime
     ZonedDateTime)
   java.util.Date
   org.bson.types.ObjectId))

(extend-protocol ConvertToDBObject
  Instant
  (to-db-object [^Instant input]
    (to-db-object (Date/from input)))
  LocalDate
  (to-db-object [^LocalDate input]
    (-> input
        (ld/at-start-of-day (zid/from zoff/utc))
        (zdt/to-instant)
        (Date/from)
        (to-db-object)))
  LocalDateTime
  (to-db-object [^LocalDateTime input]
    (-> input
        (ldt/to-instant (zid/from zoff/utc))
        (Date/from)
        (to-db-object)))
  ZonedDateTime
  (to-db-object [^ZonedDateTime input]
    (-> input
        (zdt/to-instant)
        (Date/from)
        (to-db-object))))

(extend-protocol ConvertFromDBObject
  java.util.Date
  (from-db-object [^java.util.Date input keywordize]
    (ldt/of-instant (.toInstant input) (zid/from zoff/utc))))

(defn ->object-id
  ([] (ObjectId.))
  ([id]
   (if (string? id)
     (ObjectId. id)
     id)))

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
