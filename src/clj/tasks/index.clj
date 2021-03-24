(ns tasks.index
  "Database index tasks"
  (:require [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.db]))

(def ^:const indexes
  (let [case-insensitive-index-opts {:collation {:locale "en" :strength (int 2)}}]
    [["cards" (array-map :code 1)]
     ["cards" (array-map :previous-versions 1)]
     ["cards" (array-map :type 1)]
     ["decks" (array-map :username 1)]
     ["game-logs" (array-map :gameid 1)]
     ["game-logs" (array-map "corp.player.username" 1 :start-date -1)]
     ["game-logs" (array-map "runner.player.username" 1 :start-date -1)]
     ["messages" (array-map :channel 1 :date -1)]
     ["messages" (array-map :username 1 :date -1)]
     ["news" (array-map :date -1)]
     ["users" (array-map :username 1)]
     ["users" (array-map :username 1)
      (assoc case-insensitive-index-opts :name "username_ci_1")]
     ["users" (array-map :email 1)]
     ["users" (array-map :email 1)
      (assoc case-insensitive-index-opts :name "email_ci_1")]
     ["users" (array-map :isadmin 1)]
     ["users" (array-map :ismoderator 1)]
     ["users" (array-map :special 1)]
     ["users" (array-map :resetPasswordToken 1)]]))

(defn create-indexes
  "Create indexes for queries in our codebase.

  `create-indexes` can safely be executed multiple times, as long as the
  existing indexes don't conflict with the ones created here."
  ([] (create-indexes true))
  ([standalone]
   (when standalone (webdb/connect))
   (try
     (do
       (doseq [index-args indexes]
         (apply mc/create-index db index-args))
       (println "Indexes successfully created."))
     (catch Exception e (do
                          (println "Create indexes failed" (.getMessage e))
                          (.printStackTrace e)))
     (finally (when standalone (webdb/disconnect))))))

(defn drop-indexes
  "Drop all indexes except the index on the `_id` field."
  []
  (webdb/connect)
  (try
    (do
      (doseq [coll (monger.db/get-collection-names db)]
        (do
          (mc/drop-indexes db coll)
          (println "Dropped indexes on" coll)))
      (println "\nIndexes successfully dropped."))
    (catch Exception e (do
                         (println "Drop indexes failed" (.getMessage e))
                         (.printStackTrace e)))
    (finally (webdb/disconnect))))
