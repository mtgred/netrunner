(ns web.logs
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [taoensso.timbre :as timbre]
   [taoensso.timbre.appenders.core :as tac]
   [taoensso.trove :as trove]
   [taoensso.trove.timbre :as trove-timbre]))

(set! *warn-on-reflection* true)

(defn- redact-uid-middleware
  "Timbre middelware to remove UIDs from Sente log lines"
  [{:as data :keys [vargs]}]
  (let [filter-uid (fn [arg] (if (string? arg)
                               (str/replace arg #"u_.*/c_" "u_[REDACTED]/c_")
                               arg))]
    (assoc data :vargs (mapv filter-uid vargs))))

;; Sente logs via trove, set it to use timbre.
(trove/set-log-fn! (trove-timbre/get-log-fn))

(defn- extract-opts-middleware [{:as data :keys [vargs]}]
  (if (map? (first vargs))
    (update data :context merge (first vargs))
    data))

(defn- filtered-spit-appender [appender-args pred]
  (-> (tac/spit-appender {:fname (str (:log-path appender-args))})
    (merge appender-args)
    (update :fn (fn [f] (fn [data] (when (pred data) (f data)))))))

(defn timbre-init!
  [{{:keys [default mod-action telemetry println]} :appenders :as config}]
  (let [println-args (merge {:enabled? false} println)
        default-args (assoc default :log-path (or (:log-path default) (io/file "logs" "jinteki-clojure.log")))
        mod-action-args (assoc mod-action :log-path (or (:log-path mod-action) (io/file "logs" "jinteki-mod-actions.log")))
        telemetry-args (assoc telemetry :log-path (or (:log-path telemetry) (io/file "logs" "jinteki-telemetry.log")))]
    (io/make-parents (:log-path default-args))
    (doseq [f [(:log-path default-args) (:log-path telemetry-args)]]
      (spit f ""))
    ;; todo - back up the logs files or something like that
    ;; maybe we can actually just have an indexed html that points to different log files?
    ;; that would actually be sick as hell
    (timbre/merge-config!
      (-> config
        (assoc :middleware [#'redact-uid-middleware #'extract-opts-middleware])
        (assoc :appenders
          {:println (merge (tac/println-appender) println-args)
           :default (filtered-spit-appender default-args #(not (#{:mod-action :telemetry} (:type (:context %)))))
           :mod-action (filtered-spit-appender mod-action-args #(= :mod-action (:type (:context %))))
           :telemetry (filtered-spit-appender telemetry-args #(= :telemetry (:type (:context %))))})))))
