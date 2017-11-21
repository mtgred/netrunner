(ns web.config
  (:require [aero.core :refer [read-config]]
            [clojure.java.io :as io]))

(defonce frontend-version (atom "0.1.0"))

(let [dev-config (read-config "dev.edn")
      master-config (when (.exists (io/file "config.edn"))
                      (read-config "config.edn"))]
  (defonce server-config (merge dev-config master-config)))

