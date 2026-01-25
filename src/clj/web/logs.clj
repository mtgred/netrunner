(ns web.logs
  (:require
   [taoensso.timbre :as timbre]
   [clojure.java.io :as io]))

(def home (System/getProperty "user.home"))

(def default-appender
  {:enabled? true
   :async? true
   :min-level :info
   :output-fn timbre/default-output-fn
   :fn (fn [{:keys [level output_ context]}]
         (when-not (or (= level :error) (= level :fatal) (= (:type context) :moderator))
           (spit (str home "/logs/jinteki-clojure.log") (str @output_ "\n") :append true)))})

(def exceptions-appender
  {:enabled? true
   :async? true
   :min-level :error
   :output-fn timbre/default-output-fn
   :fn (fn [{:keys [output_]}]
         (println @output_)
         (spit (str home "/logs/jinteki-exceptions.log") (str @output_ "\n") :append true))})

(def mod-action-appender
  {:enabled? true
   :async? true
   :min-level :info
   :output-fn timbre/default-output-fn
   :fn (fn [{:keys [output_ context]}]
         (when (= (:type context) :moderator)
           (spit (str home "/logs/jinteki-mod-actions.log") (str @output_ "\n") :append true)))})

(defn timbre-init!
  []
  (io/make-parents "logs/clojure.log")
  ;; clojure.log and exceptions.log can be cleaned up between each run
  (when-let [f (io/file (str home "/logs/jinteki-clojure.log"))] (when (.exists f) (.delete f)))
  (when-let [f (io/file (str home "/logs/jinteki-exceptions.log"))] (when (.exists f) (.delete f)))
  ;; todo - back up the logs files or something like that
  ;; maybe we can actually just have an indexed html that points to different log files?
  ;; that would actually be sick as hell
  (timbre/merge-config!
    {:appenders {:default default-appender
                 :println {:enabled? false}
                 :error exceptions-appender
                 :mod-action mod-action-appender}}))
