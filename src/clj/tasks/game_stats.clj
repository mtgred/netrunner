(ns tasks.game-stats
  "Export information about finished games"
  (:require
   [cljc.java-time.format.date-time-formatter :as formatter]
   [cljc.java-time.local-date :as ld]
   [cljc.java-time.local-date-time :as ldt]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [monger.operators :refer :all]
   [monger.query :as mq]
   [tasks.setup :refer [connect disconnect]]))

(def ymd-formatter (formatter/of-pattern "yyyy-MM-dd"))

(defn- convert-date [d]
  (when d
    (ldt/format d ymd-formatter)))

(defn- write-to-file
  [filename data]
  (io/make-parents filename)
  (with-open [writer (io/writer filename)]
    (let [out-data (->> data
                        (map #(assoc % :runner (:identity (:runner %))))
                        (map #(assoc % :corp (:identity (:corp %))))
                        (map #(update % :start-date convert-date))
                        (map #(update % :end-date convert-date))
                        (map (juxt :start-date :end-date :room :format :winner :reason :turn :corp :runner)))]
      (csv/write-csv writer [["start" "end" "room" "format" "winner" "reason" "turn" "corp" "runner"]])
      (csv/write-csv writer out-data))))

(defn months-ago [n]
  (ldt/format (ldt/minus-months (ldt/now) n) ymd-formatter))

(def coll "game-logs")

(defn all-games
  "Return game info from a specified date (in YYYY-MM-DD format as a string)"
  ([filename] (all-games filename (months-ago 3)))
  ([filename start-date-str]
   (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
     (try
       (let [start-date (ld/parse start-date-str ymd-formatter)
             games (mq/with-collection db (str coll)
                     (mq/find {:start-date {$gte start-date}})
                     (mq/fields {:_id 0 :start-date 1 :end-date 1 :room 1 :format 1 :winner 1
                                 :reason 1 :turn 1 :corp.identity 1 :runner.identity 1})
                     (mq/sort (array-map :start-date -1)))
             len (count games)]
         (write-to-file filename games)
         (println "Wrote" len "entries"))
       (catch Exception e (.printStackTrace e))
       (finally (disconnect system))))))
