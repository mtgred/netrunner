(ns web.analytics
  (:require
   [clojure.set :as set]
   [monger.collection :as mc]
   [clojure.java.io :as io]
   [taoensso.encore :as enc]
   [clojure.core.async :refer [<! go timeout]]
   [cljc.java-time.temporal.chrono-unit :as chrono]
   [cljc.java-time.duration :as duration]
   [cljc.java-time.instant :as inst]))

(defonce daily-analytics
  (atom {}))

(def log-analytics-frequency (enc/ms :hours 24))

(defn- write-analytic [k v]
  (when v
    (let [date-str (str (java.time.LocalDate/now))
          filename (str (name k) "-" date-str ".edn")
          dir (io/file (System/getProperty "user.home") "jinteki" "analytics")
          file (io/file dir filename)]
      (.mkdirs dir)
      (spit file (pr-str v)))))

(defn- anonymize-engagement
  [data]
  (update-in data [:users] (fn [users] (if users (count users) 0))))

(defonce write-analytics
  (go (while true
        (<! (timeout log-analytics-frequency))
        (let [analytics @daily-analytics]
          (swap! daily-analytics assoc :engagement nil)
          (write-analytic :smogon (:smogon analytics))
          (write-analytic :engagement (anonymize-engagement (:engagement analytics)))))))

(defn- merge-analytics
  [a b]
  (cond
    (and (number? a) (number? b))
    (+ a b)
    (and (map? a) (map? b))
    (merge-with merge-analytics a b)
    (and (vector? a) (vector? b))
    (mapv merge-analytics a b)
    (and (set? a) (set? b))
    (set/union a b)
    :else (or a b)))

(defn- update-analytic
  [key data]
  (swap! daily-analytics update key (fn [old-data]
                                      (if-not old-data
                                        data
                                        (merge-analytics old-data data)))))

(defn update-analytics
  [key data]
  (case key
    :smogon (update-analytic :smogon data)
    :engagement (update-analytic :engagement data)))
