(ns web.nrdb
  (:require [cheshire.core :as json]
            [clojure.string :refer [split includes?]]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [org.httpkit.client :as http]))

(def nrdb-decklist-url "https://netrunnerdb.com/api/2.0/public/decklist/")
(def nrdb-readable-url "https://netrunnerdb.com/en/decklist/")

(defn- take-numbers [coll v]
  (if (re-matches #"^\d+$" v)
    (conj coll v)
    coll))

(defn- parse-input
  "Want to handle an NRDB URL or just a deck id number"
  [input]
  (if (includes? input "/")
    (let [chunks (split input #"/")]
      (first (reduce take-numbers `() chunks)))
    (re-find #"\d+$" input)))

(defn- lookup-card [db id]
  (if-let [c (mc/find-one-as-map db "cards" {:code id})]
    c
    (mc/find-one-as-map db "cards" {:previous-versions {$elemMatch {:code id}}})))

(defn- reduce-card [db]
  (fn [m k v]
    (let [card (lookup-card db (name k))]
      (if card
        (if (= "Identity" (:type card))
          (assoc m :identity {:title (:title card) :side (:side card)})
          (update m :cards #(conj % {:card (:title card) :qty v})))
        m))))

(defn- parse-cards
  "Returns a map with the identity and the cards in a deck separated"
  [db cards]
  (reduce-kv (reduce-card db) {:identity nil :cards []} cards))

(defn- parse-nrdb-deck [db deck]
  (merge {:name (:name deck)
          :notes (str "Imported from " nrdb-readable-url (:id deck))}
         (parse-cards db (:cards deck))))

(defn- parse-response [db body]
  (let [parsed (json/parse-string body true)]
    (cond
      (not (:success parsed)) (throw (Exception. "Query failed."))
      (not= 1 (:total parsed)) (throw (Exception. "Query did not return one element."))
      (contains? parsed :data) (parse-nrdb-deck db (first (:data parsed)))
      :else (throw (Exception. "Query does not have a data field")))))

(defn download-public-decklist
  [db input]
  (let [deck-id (parse-input input)
        url (str nrdb-decklist-url deck-id)
        data (http/get url)
        {:keys [status body error]} @data]
    (cond
      error (throw (Exception. (str "Failed to download deck " error)))
      (= 200 status) (parse-response db body)
      :else (throw (Exception. (str "Failed to download deck, status " status))))))
