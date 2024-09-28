(ns web.decks
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [crypto.password.pbkdf2 :as pbkdf2]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :refer [slugify]]
   [jinteki.validator :refer [calculate-deck-status]]
   [monger.collection :as mc]
   [monger.result :refer [acknowledged?]]
   [web.mongodb :refer [->object-id ->object-id]]
   [web.nrdb :as nrdb]
   [web.utils :refer [response mongo-time-to-utc-string]]
   [web.ws :as ws]))

(defn decks-handler
  [{db :system/db
    {:keys [username]} :user}]
  (let [uname (or username "__demo__")
        decks (mc/find-maps db "decks" {:username uname})
        decks (map #(update % :date (fn [x] (if (string? x) x (mongo-time-to-utc-string x)))) decks)]
    (response 200 decks)))

(defn update-card
  [card]
  (update card :card @all-cards))

(defn update-deck
  [deck]
  (-> deck
      (update :cards #(map update-card %))
      (update :identity #(@all-cards (:title %)))))

(defn prepare-deck-for-db
  [deck username status]
  (-> deck
      (update :cards (fn [cards] (mapv #(select-keys % [:qty :card :id :art]) cards)))
      (assoc :username username
             :status status)))

(defn- deck-locked?
  [db deck-id]
  (let [deck (mc/find-one-as-map db "decks" {:_id (->object-id deck-id)})]
    (or (:locked deck)
        false)))

(defn make-salt
  [deck-name]
  (let [salt (byte-array (map byte (slugify deck-name)))]
    (if (empty? salt) (byte-array (map byte "default-salt")) salt)))

(defn decks-create-handler
  [{db :system/db
    {username :username} :user
    deck                 :body}]
  (if (and username deck)
    (let [updated-deck (update-deck deck)
          status (calculate-deck-status updated-deck)
          deck (prepare-deck-for-db deck username status)]
      (response 200 (mc/insert-and-return db "decks" deck)))
    (response 401 {:message "Unauthorized"})))

(defn decks-save-handler
  [{db :system/db
    {username :username} :user
    deck                 :body}]
  (if (and username deck)
    (let [updated-deck (update-deck deck)
          status (calculate-deck-status updated-deck)
          deck (prepare-deck-for-db deck username status)]
      (if-let [deck-id (:_id deck)]
        (if-not (deck-locked? db deck-id)
          (if (:identity deck)
            (do (mc/update db "decks"
                           {:_id (->object-id deck-id) :username username}
                           {"$set" (dissoc deck :_id)})
                (response 200 {:message "OK" :_id (->object-id deck-id)}))
            (response 409 {:message "Deck is missing identity"}))
          (response 403 {:message "Deck is locked"}))
        (response 409 {:message "Deck is missing _id"})))
    (response 401 {:message "Unauthorized"})))

(defn decks-delete-handler
  [{db :system/db
    {username :username} :user
    {id :id}             :path-params}]
  (try
    (if (and username id)
      (if-not (deck-locked? db id)
        (if (acknowledged? (mc/remove db "decks" {:_id (->object-id id) :username username}))
          (response 200 {:message "Deleted"})
          (response 403 {:message "Forbidden"}))
        (response 403 {:message "Locked"}))
      (response 401 {:message "Unauthorized"}))
    (catch Exception _
      ;; Deleting a deck that was never saved throws an exception
      (response 409 {:message "Unknown deck id"}))))

(defmethod ws/-msg-handler :decks/import
  decks--import
  [{{db :system/db
     {username :username} :user} :ring-req
    uid :uid
    {:keys [input]} :?data
    id :id
    timestamp :timestamp}]
  (try
    (let [deck (nrdb/download-public-decklist db input)]
      (if (every? #(contains? deck %) [:name :identity :cards])
        (let [db-deck (assoc deck
                             :_id (->object-id)
                             :date (inst/now)
                             :format "standard")
              updated-deck (update-deck db-deck)
              status (calculate-deck-status updated-deck)
              deck (prepare-deck-for-db db-deck username status)]
          (mc/insert db "decks" deck)
          (ws/broadcast-to! [uid] :decks/import-success "Imported"))
        (ws/broadcast-to! [uid] :decks/import-failure "Failed to parse imported deck.")))
    (catch Exception _
      (ws/broadcast-to! [uid] :decks/import-failure "Failed to import deck.")))
  (lobby/log-delay! timestamp id))
