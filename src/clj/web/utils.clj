(ns web.utils
  (:require [ring.util.response :as resp]
            [monger.collection :as mc]
            [web.db :refer [db]]))

(defn tick
  "Call f with args every ms. First call will be after ms"
  [callback ms]
  (future
    (while true
      (do (Thread/sleep ms)
          (callback)))))

(defn response [status-code msg]
  (resp/status (resp/response msg) status-code))
