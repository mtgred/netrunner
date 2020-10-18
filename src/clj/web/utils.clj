(ns web.utils
  (:require [ring.util.response :as resp]
            [monger.collection :as mc]
            [web.db :refer [db]])
  (:import java.security.MessageDigest))

(defn tick
  "Call f with args every ms. First call will be after ms"
  [callback ms]
  (future
    (while true
      (do (Thread/sleep ms)
          (callback)))))

(defn response [status-code msg]
  (resp/status (resp/response msg) status-code))

(defn md5
  "Taken from here: https://gist.github.com/jizhang/4325757"
  [^String s]
  (->> (.getBytes s)
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))
