(ns tasks.decks
  "Deck modification tasks"
  (:require
    [monger.collection :as mc]
    [monger.result :refer [acknowledged?]]
    [tasks.setup :refer [connect disconnect]]
    [web.mongodb :refer [object-id]]))

(defn- get-all-usernames
  "Get all usernames in the database."
  [db]
  (map :username (mc/find-maps db "users" {} [:username])))

(defn- get-deck
  "Get a deck by _id"
  [db deck-id]
  (dissoc (mc/find-one-as-map db "decks" {:_id (object-id deck-id)}) :_id))

(defn add-for-all-users
  "Add the specified deck id for all users"
  [deck-id]
  (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
    (try
      (let [deck (get-deck db deck-id)]
        (if-not deck
          (println "ERROR: Unknown deck-id" deck-id)
          (let [usernames (distinct (get-all-usernames db))
                new-decks (map #(assoc deck :username %) usernames)
                result (mc/insert-batch db "decks" new-decks)]
            (if (acknowledged? result)
              (println (count new-decks) "decks added")
              (println "ERROR ADDING DECKS")))))
      (finally (disconnect system)))))
