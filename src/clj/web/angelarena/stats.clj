(ns web.angelarena.stats
  (:require [clojure.string :refer [lower-case]]
            [web.angelarena.utils :refer [get-runs]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.operators :refer :all]))

(defn- enter-winner
  [db player {:keys [gameid format ending-players state] :as game}]
  (try
    (let [username (get-in player [:user :username])
          other-player (first (filter #(not= username (get-in % [:user :username])) ending-players))
          runs (get-runs db username)
          form (keyword (lower-case format))
          side (keyword (lower-case (:side player)))]
      (mc/update db "users"
                 {:username username}
                 {"$set" {:angelarena-runs
                          (update-in runs [form side :games]
                                     (partial map #(if (= (str gameid) (:game-id %))
                                                     {:game-id (str gameid)
                                                      :winner (:winner @state)
                                                      :opponent {:username (get-in other-player [:user :username])
                                                                 :identity (get-in @state [(keyword (lower-case (:side other-player))) :identity :title])}}
                                                     %)))}}))
    (catch Exception e
      (println "Caught exception entering winner: " (.getMessage e)))))

(defn game-finished
  [db {:keys [gameid ending-players original-players state format state] :as game}]
  (doall
    (for [player original-players]
      (let [username (get-in player [:user :username])
            end-player (first (filter #(= username (get-in % [:user :username])) ending-players))
            form (keyword (lower-case format))
            side (keyword (lower-case (:side player)))]
        (enter-winner db player game)
        (ws/broadcast-to! [(:ws-id end-player)] :angelarena/run-update {})))))
