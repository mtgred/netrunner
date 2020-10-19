(ns web.decks
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [clojure.string :refer [split split-lines join escape lower-case] :as s]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [crypto.password.pbkdf2 :as pbkdf2]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status]]))


(defn decks-handler [req]
  (if-let [user (:user req)]
    (response 200 (mc/find-maps db "decks" {:username (:username user)}))
    (response 200 (mc/find-maps db "decks" {:username "__demo__"}))))

(defn update-card
  [card]
  (update card :card @all-cards))

(defn update-deck
  [deck]
  (-> deck
      (update :cards #(map update-card %))
      (update :identity #(@all-cards (:title %)))))

(defn prepare-deck-for-db
  [deck username status deck-hash]
  (-> deck
      (update :cards (fn [cards] (mapv #(select-keys % [:qty :card :id :art]) cards)))
      (assoc :username username
             :status status
             :hash deck-hash)))

(defn hash-deck
  [deck]
  (let [check-deck (-> deck
                       (update :cards #(map update-card %))
                       (update :identity #(get @all-cards (:title %))))
        id (-> deck :identity :title)
        sorted-cards (sort-by #(:code (:card %)) (:cards check-deck))
        decklist (s/join (for [entry sorted-cards]
                           (str (:qty entry) (:code (:card entry)))))
        deckstr (str id decklist)
        salt (byte-array (map byte (:name deck)))]
    (last (s/split (pbkdf2/encrypt deckstr 100000 "HMAC-SHA1" salt) #"\$"))))

(defn decks-create-handler [{{username :username} :user
                             deck                 :body}]
  (if (and username deck)
    (let [updated-deck (update-deck deck)
          status (calculate-deck-status updated-deck)
          deck-hash (hash-deck updated-deck)
          deck (prepare-deck-for-db deck username status deck-hash)]
      (response 200 (mc/insert-and-return db "decks" deck)))
    (response 401 {:message "Unauthorized"})))

(defn decks-save-handler [{{username :username} :user
                           deck                 :body}]
  (if (and username deck)
    (let [updated-deck (update-deck deck)
          status (calculate-deck-status updated-deck)
          deck-hash (hash-deck updated-deck)
          deck (prepare-deck-for-db deck username status deck-hash)]
      (if-let [deck-id (:_id deck)]
        (if (:identity deck)
          (do (mc/update db "decks"
                         {:_id (object-id deck-id) :username username}
                         {"$set" (dissoc deck :_id)})
            (response 200 {:message "OK" :_id (object-id deck-id)}))
          (response 409 {:message "Deck is missing identity"}))
        (response 409 {:message "Deck is missing _id"})))
    (response 401 {:message "Unauthorized"})))

(defn hash-existing-decks []
  (let [decks (mc/find-maps db "decks")]
    (->> decks
         (remove :hash)
         (map #(assoc % :hash (hash-deck %)))
         (map #(let [deck-id (:_id %)
                     username (:username %)]
                 (when deck-id
                   (mc/update db "decks"
                              {:_id (object-id deck-id) :username username}
                              {"$set" (dissoc % :_id)}))))
         (filter acknowledged?)
         count)))

(defn decks-delete-handler [{{username :username} :user
                             {id :id}             :params}]
  (if (and username id)
    (if (acknowledged? (mc/remove db "decks" {:_id (object-id id) :username username}))
      (response 200 {:message "Deleted"})
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))
