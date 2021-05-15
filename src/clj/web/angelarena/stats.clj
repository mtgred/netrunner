(ns web.angelarena.stats
  (:require [clojure.string :refer [lower-case]]
            [web.angelarena.runs :refer [finish-run]]
            [web.angelarena.utils :refer [get-runs get-losses get-deck-from-id]]
            [web.mongodb :refer [object-id]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.operators :refer :all]))

(defonce max-losses 3)

(defn- enter-winner
  [db player {:keys [gameid format ending-players state] :as game}]
  (try
    (let [username (get-in player [:user :username])
          other-player (first (filter #(not= username (get-in % [:user :username])) ending-players))
          runs (get-runs db username)
          form (keyword (lower-case format))
          side (keyword (lower-case (:side player)))
          new-runs (update-in runs [form side :games]
                                     (partial map #(if (= (str gameid) (:game-id %))
                                                     {:game-id (str gameid)
                                                      :winner (:winner @state)
                                                      :opponent {:username (get-in other-player [:user :username])
                                                                 :identity (get-in @state [(keyword (lower-case (:side other-player))) :identity :title])}}
                                                     %)))]
      (mc/update db "users"
                 {:username username}
                 {"$set" {:angelarena-runs new-runs}})
      new-runs)
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
        (when-let [runs (enter-winner db player game)]
          (when-let [deck (get-deck-from-id db username (get-in runs [form side :deck-id]))]
            (if (<= max-losses (get-losses (get-in runs [form side])))
              (let [run-id (finish-run db username runs deck)]
                (ws/broadcast-to! [(:ws-id end-player)] :angelarena/run-update {:finished-run run-id}))
              (ws/broadcast-to! [(:ws-id end-player)] :angelarena/run-update {}))))))))

(defn add-new-game
  [db player other-player game-id]
  (try
    (let [username (get-in player [:user :username])
          runs (get-runs db username)
          side (keyword (lower-case (:side player)))
          form (:format player)
          other-username (get-in other-player [:user :username])
          other-identity (get-in other-player [:deck :identity :title])]
      (mc/update db "users"
                 {:username username}
                 {"$set" {:angelarena-runs
                          (update-in runs [form side :games] conj {:game-id game-id
                                                                   :winner nil
                                                                   :opponent {:username other-username
                                                                              :identity other-identity}})}}))
    (catch Exception e
      (println "Caught exception adding new game to Angel Arena history: " (.getMessage e)))))
