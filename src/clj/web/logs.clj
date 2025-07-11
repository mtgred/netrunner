(ns web.logs
  (:require
   [taoensso.timbre :as timbre]
   [clojure.java.io :as io]))

(defn appender
  [file min-level print?]
  {:enabled? true
   :async? true
   :min-level min-level
   :output-fn timbre/default-output-fn
   :fn (fn [{:keys [output_]}]
         (when print? (println @output_))
         (spit file (str @output_ "\n") :append true))})

(def log-appender (appender "logs/clojure.log" :info nil))
(def exceptions-appender (appender "logs/exceptions.log" :error true))
(def mod-action-appender (appender "logs/mod-actions.log" :info nil))

(defn timbre-init!
  []
  (io/make-parents "logs/clojure.log")
  (println "initialized timbre")
  ;; todo - back up the logs files or something like that
  ;; maybe we can actually just have an indexed html that points to different log files?
  ;; that would actually be sick as hell
  (timbre/merge-config!
    {:appenders {:default log-appender
                 :exceptions exceptions-appender
                 :mod-action mod-action-appender}}))
