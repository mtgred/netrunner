(ns web.config
  (:require [aero.core :refer [read-config]]
            [clojure.java.io :as io]))

(defonce frontend-version (atom "0.1.0"))

(defonce server-mode (atom "prod"))

(let [dev-config (read-config "dev.edn")
      master-filename "config.edn"
      master-config (when (.exists (io/file master-filename))
                      (read-config master-filename))]
  (defonce server-config (merge dev-config master-config)))

