(ns tasks.db
  "Database maintenance tasks"
  (:require [web.db :refer [db object-id] :as webdb]
            [web.decks :refer [update-deck prepare-deck-for-db]]
            [web.core :refer [load-data]]
            [monger.collection :as mc]
            [monger.db]
            [monger.operators :refer :all]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status]]))

(defn- get-deck-status
  [deck]
  (if (or (nil? (:identity deck)) (empty? (:identity deck)))
    (throw (Exception. "Nil/Empty identity"))
    (let [updated-deck (update-deck deck)]
      (calculate-deck-status updated-deck))))

(defn update-all-decks
  "Run after fetching the data to update all decks"
  [& args]
  (webdb/connect)
  (load-data)
  (let [cnt (atom 0)]
    (doseq [deck (mc/find-maps db "decks" nil)]
      (let [deck-id (:_id deck)]
        (swap! cnt inc)
        (when (zero? (mod @cnt 1000)) (do (print ".") (flush)))
        (try
          (let [status (get-deck-status deck)]
            (mc/update db "decks"
                       {:_id (object-id deck-id)}
                       {"$set" {"status" status}}))
          (catch Exception e (do (println "Something got hecked" (.getMessage e))
                                 (println "Deck id:" deck-id))))))
    (newline)
    (println "Updated" @cnt "decks"))
    (webdb/disconnect))

(defn- get-all-users
  "Get all users in the database. Takes a list of fields."
  [fields]
  (mc/find-maps db "users" {} fields))

(defn- delete-user
  "Delete a user by Mongo document id"
  [id]
  (mc/remove-by-id db "users" id))

(defn delete-duplicate-users
  "Delete entries in the users table that share a username. Leave the first registered entry found in the collection."
  [& args]
  (webdb/connect)
  (try
    (let [dry-run (some #{"--dry-run"} args)
          users (get-all-users [:email :username :registrationDate :lastConnection])
          grouped (vals (group-by :username users))
          duplicates (filter #(> (count %) 1) grouped)]
      (when dry-run
        (println "DRY RUN: not deleting accounts"))
      (println "Found" (count users) "user accounts.")
      (println "Found" (count duplicates) "duplicated usernames.")
      (doseq [d duplicates]
        (let [[f & r] (sort-by :registrationDate d)]
          (println "Found username:" (:username f))
          (println "\tKeeping:" (:email f) "," (:registrationDate f))
          (if dry-run
            (println "\tWould delete:")
            (println "\tDeleting:"))
          (doseq [del r]
            (println "\t\t" (:email del) "," (:registrationDate del))
            (when (not dry-run)
              (delete-user (:_id del)))))))
    (catch Exception e (do
                         (println "Delete duplicate users failed" (.getMessage e))
                         (.printStackTrace e)))
    (finally (webdb/disconnect))))

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
  []
  (webdb/connect)
  (try
    (do
      (doseq [index-args indexes]
        (apply mc/create-index db index-args))
      (println "Indexes successfully created."))
    (catch Exception e (do
                         (println "Create indexes failed" (.getMessage e))
                         (.printStackTrace e)))
    (finally (webdb/disconnect))))

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
