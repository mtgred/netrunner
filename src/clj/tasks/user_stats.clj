(ns tasks.user-stats
  "Export information about users"
  (:require
   [cljc.java-time.format.date-time-formatter :as formatter]
   [cljc.java-time.local-date :as ld]
   [cljc.java-time.local-date-time :as ldt]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [monger.collection :as mc]
   [monger.operators :refer :all]
   [tasks.setup :refer [connect disconnect]]))

(def ymd-formatter (formatter/of-pattern "yyyy-MM-dd"))

(defn- write-file
  [headers filename data]
  (io/make-parents filename)
  (with-open [writer (io/writer filename)]
    (let [out-data (map (juxt :_id :count) data)]
      (csv/write-csv writer [headers])
      (csv/write-csv writer out-data))))

(defn- all-users-fn [db date]
  (mc/aggregate db "users"
                [{$match {:registrationDate {$gte date}}},
                 {$group {:_id {$dateToString {:format "%Y-%m-%d"
                                               :date "$registrationDate"}},
                          :count {$sum 1}}}
                 {$sort {:_id 1}}]))

(defn- all-backgrounds-fn [db date]
  (mc/aggregate db "users"
                [{$match {:registrationDate {$gte date}}},
                 {$group {:_id "$options.background"
                          :count {$sum 1}}}]))

(defn- aggregate-shell
  [filename start-date-str userfn headers]
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
    (try
      (let [start-date (ld/parse start-date-str ymd-formatter)
            results (userfn db start-date)
            len (count results)]
        (write-file headers filename results)
        (println "Wrote" len "entries"))
      (catch Exception e (.printStackTrace e))
      (finally (disconnect system)))))

(defn months-ago [n]
  (ldt/format (ldt/minus-months (ldt/now) n) ymd-formatter))

(defn all-users
  "Return user registrations from a specified date (in YYYY-MM-DD format as a string)"
  ([filename] (all-users filename (months-ago 3)))
  ([filename start-date-str]
   (aggregate-shell filename start-date-str all-users-fn ["date" "count"])))

(defn all-backgrounds
  "Return game screen backgrounds from a specified date (in YYYY-MM-DD format as a string)"
  ([filename] (all-backgrounds filename (months-ago 3)))
  ([filename start-date-str]
   (aggregate-shell filename start-date-str all-backgrounds-fn ["background" "count"])))
