(ns web.game-api
  (:require [web.mongodb :refer [object-id]]
            [web.decks :as decks]
            [web.lobby :as lobby]
            [web.utils :refer [response]]
            [monger.collection :as mc]))

(defn- make-link [host path] (str host path))

(defn- make-card-details [host card]
  (-> card
      (dissoc :_id :images :quantity :format :rotated :normalizedtitle :previous-versions)
      (assoc :image (make-link host (get-in card [:images :en :default :stock])))))

(defn- make-card-info [host card]
  {:qty (:qty card)
   :title (:title (:card card))
   :details (make-card-details host (:card card))})

(defn- get-deck-id [username game]
  (if (:started game)
    (let [state @(:state game)
          side (if (= username (get-in state [:runner :user :username])) :runner :corp)]
      (get-in state [side :deck-id]))
    (:_id (:deck (first (filter #(= username (get-in % [:user :username])) (:players game)))))))

(defn- get-deck [db username game]
  (if-let [deck-id (get-deck-id username game)]
    (decks/update-deck (mc/find-one-as-map db "decks" {:_id (object-id deck-id) :username username}))
    nil))

(defn deck-handler [{db :system/db
                     scheme :scheme
                     headers :headers}]
  (if-let [api-key (get headers "x-jnet-api")]
    (let [api-uuid (try
                     (java.util.UUID/fromString api-key)
                     (catch Exception e nil))
          api-record (mc/find-one-as-map db "api-keys" {:api-key api-uuid} ["username"])
          username (:username api-record)]
      (if username
        (let [game (lobby/game-for-username username)
              allow-access (:api-access game)]
          (if (and game allow-access)
            (if-let [deck (get-deck db username game)]
              (let [host (str (name scheme) "://" (get headers "host"))]
                (response 200 {:name (:name deck)
                               :identity {:title (get-in deck [:identity :title])
                                          :details (make-card-details host (:identity deck))}
                               :cards (map #(make-card-info host %) (:cards deck))}))
              (response 204 {:message "No deck selected"}))
            (response 403 {:message "No game for key or API Access not enabled"})))
        (response 404 {:message "Unknown X-JNet-API key"})))
    (response 400 {:message "No X-JNet-API header specified"})))
