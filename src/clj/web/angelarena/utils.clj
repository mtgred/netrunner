(ns web.angelarena.utils
  (:require [clojure.string :refer [lower-case capitalize]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clj-time.core :as t]))

(defonce supported-formats [:standard :startup])

(defn get-runs
  [db username]
  (try
    (let [{:keys [angelarena-runs]}
          (mc/find-one-as-map db "users" {:username username} ["angelarena-runs"])]
      (merge (into (hash-map)
                   (map (fn [form] [form {:corp nil :runner nil}])
                        supported-formats))
             angelarena-runs))
    (catch Exception e
      (println "Caught exception searching for run: " (.getMessage e)))))
