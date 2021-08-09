(ns web.angel-arena.utils
  (:require [clojure.string :refer [lower-case capitalize]]
            [jinteki.cards :refer [all-cards]]
            [jinteki.validator :refer [calculate-deck-status]]
            [web.mongodb :refer [object-id]]
            [web.ws :as ws]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [clj-time.core :as t]))

(defonce supported-formats [:standard :startup])

(defn get-runs
  [db username]
  (try
    (let [{:keys [angel-arena-runs]}
          (mc/find-one-as-map db "users" {:username username} ["angel-arena-runs"])]
      (merge (into (hash-map)
                   (map (fn [form] [form {:corp nil :runner nil}])
                        supported-formats))
             angel-arena-runs))
    (catch Exception e
      (println "Caught exception searching for run: " (.getMessage e)))))

(defn get-deck-from-id
  [db username deck-id]
  (try
    (let [map-card (fn [c] (update-in c [:card] @all-cards))
          unknown-card (fn [c] (nil? (:card c)))]
      (as-> (mc/find-one-as-map db "decks" {:_id (object-id deck-id) :username username}) d
        (update-in d [:cards] #(mapv map-card %))
        (update-in d [:cards] #(vec (remove unknown-card %)))
        (update-in d [:identity] #(@all-cards (:title %)))
        (assoc d :status (calculate-deck-status d))))
    (catch Exception e
      (println "Caught exception searching for a deck from deck-id: " (.getMessage e)))))

(defn get-current-deck
  [db username form side]
  (let [runs (get-runs db username)
        deck-id (get-in runs [form side :deck-id])]
    (get-deck-from-id db username deck-id)))

(defn get-wins
  [{:keys [games side] :as run-info}]
  (count (filter #(= (name side) (:winner %)) games)))

(defn get-losses
  [{:keys [games] :as run-info}]
  (- (count (remove #(nil? (:winner %)) games))
     (get-wins run-info)))
