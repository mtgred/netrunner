(ns tasks.game-stats
  "Export information about finished games"
  (:require
    [tasks.setup :refer [connect disconnect]]
    [monger.query :as mq]
    [monger.operators :refer :all]
    [clj-time.format :as f]
    [clj-time.local :as l]
    [clj-time.core :as t]
    [clj-time.coerce :as c]
    [clojure.data.csv :as csv]
    [clojure.java.io :as io]))

(defn- convert-date [d]
  (let [cd (c/from-date d)]
    (f/unparse (f/formatters :basic-date-time-no-ms) cd)))

(defn- write-to-file
  [filename data]
  (io/make-parents filename)
  (with-open [writer (io/writer filename)]
    (let [out-data (->> data
                        (map #(assoc % :runner (:identity (:runner %))))
                        (map #(assoc % :corp (:identity (:corp %))))
                        (map #(update % :start-date convert-date))
                        (map #(update % :end-date convert-date))
                        (map #(map % [:start-date :end-date :room :format :winner :reason :turn :corp :runner])))]
      (csv/write-csv writer [["start" "end" "room" "format" "winner" "reason" "turn" "corp" "runner"]])
      (csv/write-csv writer out-data))))

(defn all-games
  "Return game info from a specified date (in YYYY-MM-DD format as a string)"
  ([filename] (all-games filename (l/format-local-time (t/minus (l/local-now) (t/months 3)) :year-month-day)))
  ([filename start-date-str]
   (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
     (try
       (let [start-date (f/parse (f/formatters :year-month-day) start-date-str)
             games (mq/with-collection db "game-logs"
                     (mq/find {:start-date {$gte (c/to-date start-date)}})
                     (mq/fields {:_id 0 :start-date 1 :end-date 1 :room 1 :format 1 :winner 1
                                 :reason 1 :turn 1 :corp.identity 1 :runner.identity 1})
                     (mq/sort (array-map :start-date -1)))
             len (count games)]
         (write-to-file filename games)
         (println "Wrote" len "entries"))
       (catch Exception e (.printStackTrace e))
       (finally (disconnect system))))))
