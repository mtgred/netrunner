(ns web.decks
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [jinteki.cards :refer [all-cards]]
            [jinteki.decks :as decks]))


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
    (let [update-card (fn [c] (update-in c [:card] @all-cards))
          check-deck (-> deck
                         (update-in [:cards] #(map update-card %))
                         (update-in [:identity] #(@all-cards (:title %))))
          deck (-> deck
                   (update-in [:cards] (fn [cards] (mapv #(select-keys % [:qty :card :id :art]) cards)))
                   (assoc :username username))
          status (decks/calculate-deck-status check-deck)
          deck (assoc deck :status status)]
      (if-let [deck-id (:_id deck)]
        (if (:identity deck)
          (do (mc/update db "decks"
                         {:_id (object-id deck-id) :username username}
                         {"$set" (dissoc deck :_id)})
            (response 200 {:message "OK"}))
          (response 409 {:message "Deck is missing identity"}))
        (response 409 {:message "Deck is missing _id"})))
    (response 401 {:message "Unauthorized"})))

(defn decks-delete-handler [{{username :username} :user
                             {id :id}             :params}]
  (if (and username id)
    (if (acknowledged? (mc/remove db "decks" {:_id (object-id id) :username username}))
      (response 200 {:message "Deleted"})
      (response 403 {:message "Forbidden"}))
    (response 401 {:message "Unauthorized"})))

