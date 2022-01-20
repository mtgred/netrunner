(ns executor.loggers
  "shamelessly stolen from/inspired by re-frame: https://github.com/day8/re-frame
  licensed under MIT
  source: https://github.com/day8/re-frame/blob/master/src/re_frame/loggers.cljc"
  (:require
   [clojure.set :refer [difference]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]))

(defn log [level & args]
  (log/log level (str/join " " args)))

(def ^:private loggers
  "Holds the current set of logging functions.
  By default, re-frame uses the functions provided by js/console.
  Use `set-loggers!` to change these defaults"
  (atom {:log      (partial log :info)
         :warn     (partial log :warn)
         :error    (partial log :error)
         :debug    (partial log :debug)
         :group    (partial log :info)
         :groupEnd  #()}))

(defn console
  [level & args]
  (assert (contains? @loggers level)
          (str "re-frame: log called with unknown level: " level))
  (apply (level @loggers) args))


(defn set-loggers!
  [new-loggers]
  (assert (empty? (difference (-> new-loggers keys set) (-> @loggers keys set)))
          (str "Unknown keys in new-loggers: " (difference (-> new-loggers keys set) (-> @loggers keys set))))
  (swap! loggers merge new-loggers))

(defn get-loggers
  "Get the current logging functions used by re-frame."
  []
  @loggers)
