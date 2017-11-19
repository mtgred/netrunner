(ns jinteki.config
  (:require [aero.core :refer [read-config]]))

(defonce frontend-version (atom "0.1.0"))

(let [dev-config (read-config "dev.edn")
      master-config (read-config "config.edn")]
  (defonce server-config (merge dev-config master-config)))

