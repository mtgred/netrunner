(ns web.data
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]))

(defn cards-handler [req]
  (let [r (map #(dissoc % :_id) (mc/find-maps db "cards" nil))]
    (response 200 r)))

(defn alt-arts-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "altarts" nil))))

(defn decks-handler [req]
  (if-let [user (:user req)]
    (response 200 (mc/find-maps db "decks" {:username (:username user)}))
    (response 200 (mc/find-maps db "decks" {:username "__demo__"}))))

(defn decks-create-handler [{{username :username} :user
                             deck                 :body}]
  (if (and username deck)
    (let [deck (-> deck
                   (update-in [:cards] (fn [cards] (mapv #(select-keys % [:qty :card :id :art]) cards)))
                   (assoc :username username))]
      (response 200 (mc/insert-and-return db "decks" deck)))
    (response 401 {:message "Unauthorized"})))

(defn decks-save-handler [{{username :username} :user
                           deck                 :body}]
  (if (and username deck)
    (let [deck (-> deck
                   (update-in [:cards] (fn [cards] (mapv #(select-keys % [:qty :card :id :art]) cards)))
                   (assoc :username username))]
      (if-let [deck-id (:_id deck)]
        (do (mc/update db "decks" {:_id (object-id deck-id)} {"$set" (dissoc deck :_id)})
            (response 200 {:message "OK"}))
        (response 409 {:message "Deck is missing _id"})))
    (response 401 {:message "Unauthorized"})))


(defn decks-delete-handler [{{username :username} :user
                             {id :id}             :params}]
  (if (and username id)
    (if (acknowledged? (mc/remove db "decks" {:_id (object-id id) :username username}))
      (response 200 {:message "Deleted"})
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))

(defn sets-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "sets" nil))))

(defn mwl-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "mwl" nil))))

(defn cycles-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "cycles" nil))))

(defn donors-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "donators" nil))))
