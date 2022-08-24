(ns web.utils
  (:require [ring.util.response :as resp]
            [cljc.java-time.local-date-time :as ldt]
            [cljc.java-time.zone-offset :as zone])
  (:import java.security.MessageDigest))

(defn tick
  "Call f with args every ms. First call will be after ms. Timeout is applied to each call and defaults to 5 minutes. Calls which timeout or error 3 times in a row will be cancelled."
  ([callback ms] (tick callback ms 300000))
  ([callback ms timeout]
  (future
    (let [failures (atom 0)]
      (while (<= @failures 3)
        (let [_ (Thread/sleep ms)
              thread (future (try (callback) (catch Exception e (println "Caught an error in tick") (.printStackTrace e) ::failed)))
              result (deref thread timeout ::failed)]
             (if (= result ::failed)
                 (do (future-cancel thread) (swap! failures inc))
                 (reset! failures 0))))
      (println "Tick task failed.")))))

(defn response [status-code msg]
  (resp/status (resp/response msg) status-code))

(defn html-response [status-code msg]
  (resp/status (resp/content-type (resp/response msg) "text/html") status-code))

(defn json-response [status-code msg]
  (resp/status (resp/content-type (resp/response msg) "application/json") status-code))

(defn md5
  "Taken from here: https://gist.github.com/jizhang/4325757"
  [^String s]
  (->> (.getBytes s)
       (.digest (MessageDigest/getInstance "MD5"))
       (BigInteger. 1)
       (format "%032x")))

(defn average
  [numbers]
  (if (empty? numbers)
    0
    (/ (reduce + numbers) (count numbers))))

(defn mongo-time-to-utc-string
  [s]
  (if s
    (.toString (ldt/to-instant s zone/utc))
    ""))
