(ns web.deck-api
  (:require [monger.collection :as mc]
            [clojure.string :as s]
            [web.utils :refer [response]]
            [web.db :refer [db]]
            [web.tokens :as tokens])
  (:import org.bson.types.ObjectId))

(defn- validate-request [{headers :headers}]
  (if-let [auth (get headers "authorization")]
    (let [[bearer, token] (s/split (s/trim auth) #"\s+" 2)]
      (if (= "Bearer" bearer)
        (if-let [entry (mc/find-one-as-map db "api_keys" {:token token})]
          (let [[is-valid data] (tokens/verify-api-token token (:public-key entry))]
            (case is-valid
              :valid (response 200 data)
              (response 403 {:message data})))
          (response 403 {:message "Unknown Token"}))
        (response 400 {:message "No Bearer Token"})))
    (response 400 {:message "No Authorization Header"})))

(defn- get-username
  [checked-response]
  (if-let [emailhash (get-in checked-response [:body :emailhash] nil)]
    (:username (mc/find-one-as-map db "users" {:emailhash emailhash}))
    nil))

(defn- send-decks
  [username]
  (let [decks (mc/find-maps db "decks" {:username username} ["_id" "name" "date" "format"])]
    (response 200 {:decks decks})))

(defn- send-deck
  [username deck_id]
  (try
    (if-let [deck (mc/find-one-as-map db "decks"
                                      {:_id (ObjectId. deck_id) :username username}
                                      ["_id" "name" "date" "format" "cards" "identity"])]
      (response 200 {:deck 
                     (update deck :identity #(select-keys % [:title :side]))})
      (response 404 {:message "Unknown deck"}))
    (catch Exception e (response 404 {:message "Invalid deck id"}))))

(defn decks-list-handler
  [req]
  (let [checked-response (validate-request req)]
    (if (not= 200 (:status checked-response))
      checked-response
      (if-let [username (get-username checked-response)]
        (send-decks username)
        (response 403 {:message "Unknown user"})))))

(defn deck-list-handler
  [req]
  (let [checked-response (validate-request req)]
    (if (not= 200 (:status checked-response))
      checked-response
      (if-let [username (get-username checked-response)]
        (let [deck_id (get-in req [:params :deck_id] nil)]
          (send-deck username deck_id))
        (response 403 {:message "Unknown user"})))))
