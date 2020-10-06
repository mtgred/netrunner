(ns tasks.db
  "Database maintenance tasks"
  (:require [web.db :refer [db object-id] :as webdb]
            [web.decks :refer [update-deck prepare-deck-for-db]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status]]))

(defn update-all-decks
  "Run after fetching the data to update all decks"
  [& args]
  (webdb/connect)
  (try
    (doseq [deck (mc/find-maps db "decks" nil)
            :when (:identity deck)
            :let [deck-id (:_id deck)
                  status (calculate-deck-status (update-deck deck))]]
      (mc/update db "decks"
                 {:_id (object-id deck-id)}
                 {"$set" {"status" status}}))
    (catch Exception e (do (println "Something got hecked" (.getMessage e))
                           (.printStackTrace e)))
    (finally (webdb/disconnect))))

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
