(ns tasks.user-stats
  "Export information about users"
  (:require
    [monger.collection :as mc]
    [monger.operators :refer :all]
    [clj-time.format :as f]
    [clj-time.local :as l]
    [clj-time.core :as t]
    [clj-time.coerce :as c]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]
    [tasks.setup :refer [connect disconnect]]))

(defn- write-file
  [ ks headers filename data]
  (io/make-parents filename)
  (with-open [writer (io/writer filename)]
    (let [out-data (map #(map % ks) data)]
      (csv/write-csv writer [headers])
      (csv/write-csv writer out-data))))

(defn- all-users-fn [db cdate]
  (mc/aggregate db "users"
                [{$match {:registrationDate {$gte cdate}}},
                 {$group {:_id { $dateToString {:format "%Y%m%d" :date "$registrationDate"}},
                          :count {$sum 1}}}
                 {$sort {:_id 1}}]))

(defn- all-backgrounds-fn [db cdate]
  (mc/aggregate db "users"
                [{$match {:registrationDate {$gte cdate}}},
                 {$group {:_id "$options.background"
                          :count {$sum 1}}}]))

(defn- aggregate-shell
  [filename start-date-str userfn ks headers]
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
    (try
      (let [start-date (f/parse (f/formatters :year-month-day) start-date-str)
            cdate (c/to-date start-date)
            results (userfn db cdate)
            len (count results)]
        (write-file ks headers filename results)
        (println "Wrote" len "entries"))
      (catch Exception e (.printStackTrace e))
      (finally (disconnect system)))))

(defn all-users
  "Return user registrations from a specified date (in YYYY-MM-DD format as a string)"
  ([filename]
   (all-users filename (l/format-local-time (t/minus (l/local-now) (t/months 3)) :year-month-day)))
  ([filename start-date-str]
   (aggregate-shell filename start-date-str all-users-fn [:_id :count] ["date" "count"])))

(defn all-backgrounds
  "Return game screen backgrounds from a specified date (in YYYY-MM-DD format as a string)"
  ([filename]
   (all-backgrounds filename (l/format-local-time (t/minus (l/local-now) (t/months 3)) :year-month-day)))
  ([filename start-date-str]
   (aggregate-shell filename start-date-str all-backgrounds-fn [:_id :count] ["background" "count"])))
