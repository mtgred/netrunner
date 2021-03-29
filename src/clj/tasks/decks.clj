(ns tasks.decks
  "Deck modification tasks"
  (:require [web.db :refer [db object-id]:as webdb]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]))

(defn- get-all-usernames
  "Get all usernames in the database."
  []
  (map :username (mc/find-maps db "users" {} [:username])))

(defn- get-deck [deck-id]
  "Get a deck by _id"
  (dissoc (mc/find-one-as-map db "decks" {:_id (object-id deck-id)}) :_id))

(defn add-for-all-users
  "Add the specified deck id for all users"
  [deck-id]
  (webdb/connect)
  (try
    (let [deck (get-deck deck-id)]
      (if-not deck
        (println "ERROR: Unknown deck-id" deck-id)
        (let [usernames (get-all-usernames)
              new-decks (map #(assoc deck :username %) usernames)
              result (mc/insert-batch db "decks" new-decks)]
          (if (acknowledged? result)
            (println (count new-decks) "decks added")
            (println "ERROR ADDING DECKS")))))
    (finally (webdb/disconnect))))
