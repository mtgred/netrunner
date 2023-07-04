(ns tasks.index
  "Database index tasks"
  (:require
    [tasks.setup :refer [connect disconnect]]
    [monger.collection :as mc]
    [monger.db]))

(def ^:const indexes
  (let [case-insensitive-index-opts {:collation {:locale "en" :strength (int 2)}}]
    [["api-keys" (array-map :api-key 1)]
     ["cards" (array-map :code 1)]
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
     ["users" (array-map :username 1) (assoc case-insensitive-index-opts :name "username_ci_1")]
     ["users" (array-map :email 1)]
     ["users" (array-map :email 1) (assoc case-insensitive-index-opts :name "email_ci_1")]
     ["users" (array-map :isadmin 1)]
     ["users" (array-map :ismoderator 1)]
     ["users" (array-map :special 1)]
     ["users" (array-map :resetPasswordToken 1)]]))

(defn create-indexes
  "Create indexes for queries in our codebase.
  `create-indexes` can safely be executed multiple times, as long as the
  existing indexes don't conflict with the ones created here."
  ([] (create-indexes nil))
  ([db?]
   (let [{{:keys [db]} :mongodb/connection :as system}
         (if db? {:mongodb/connection {:db db?}} (connect))]
     (try
       (doseq [index-args indexes]
         (apply mc/create-index db index-args))
       (println "Indexes successfully created.")
       (catch Exception e
         (do
           (println "Create indexes failed" (.getMessage e))
           (.printStackTrace e)))
       (finally (when-not db? (disconnect system)))))))

(defn drop-indexes
  "Drop all indexes except the index on the `_id` field."
  []
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
    (try
      (doseq [coll (monger.db/get-collection-names db)]
        (mc/drop-indexes db coll)
        (println "Dropped indexes on" coll))
      (println "\nIndexes successfully dropped.")
      (catch Exception e
        (do
          (println "Drop indexes failed" (.getMessage e))
          (.printStackTrace e)))
      (finally (disconnect system)))))
