(ns dev.nr
  (:require
    [nr.main :as main]
    [devtools.core :as devtools]))

(defn ^:dev/after-load on-reload []
  (main/mount))

(enable-console-print!)

(devtools/install!)

(main/init!)
