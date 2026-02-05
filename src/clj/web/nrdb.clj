(ns web.nrdb
  (:require [cheshire.core :as json]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [org.httpkit.client :as http]
            [clojure.string :as str]
            [taoensso.timbre :as timbre]))

(def nrdb-decklist-url "https://netrunnerdb.com/api/2.0/public/")
(def nrdb-readable-url "https://netrunnerdb.com/en/decklist/")

(def private-endpoint "deck/")
(def public-endpoint "decklist/")

(defn- parse-input
  "Want to handle an NRDB URL or just a deck/decklist id number
   returns: [:public/:private/:unknown, id]"
  [input]
  (let [id (if (str/includes? input "/")
             (let [frame (second (str/split input #"decklist/|deck/"))]
               (first (str/split frame #"/")))
             input)
        endpoint (cond
                   (str/includes? input "/decklist/") :public
                   (str/includes? input "/deck/") :private
                   :else :unknown)]
    [endpoint id]))

(defn- lookup-card [db id]
  (or (mc/find-one-as-map db "cards" {:code id})
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
  "returns a map with the identity and the cards in a deck separated"
  [db cards]
  (reduce-kv (reduce-card db) {:identity nil :cards []} cards))

(defn- parse-nrdb-deck [db deck]
  (merge {:name (:name deck)
          :notes (str "imported from " nrdb-readable-url (:id deck))}
         (parse-cards db (:cards deck))))

(defn- parse-response [endpoint db body]
  (let [parsed (json/parse-string body true)]
    (cond
      (not (:success parsed)) (timbre/info (Exception. "NRDB Query did not return success using endpoint: " endpoint))
      (not= 1 (:total parsed)) (timbre/info (Exception. "NRDB Query did not return one element"))
      (contains? parsed :data) (parse-nrdb-deck db (first (:data parsed)))
      :else (timbre/info (Exception. "NRDB Query does not have a data field")))))

(defn try-download-public-decklist
  "Try to download a public decklist given a specific endpoint. If the endpoint is unknown, try public first, then private"
  [db deck-id endpoint]
  (let [chosen-endpoint (case endpoint
                          :public public-endpoint
                          :private private-endpoint
                          :unknown public-endpoint)
        url (str nrdb-decklist-url chosen-endpoint deck-id)
        data (http/get url)
        {:keys [status body error]} @data]
    (cond
      error (timbre/info (Exception. (str "Failed to download deck " deck-id ": " error)))
      (= 200 status)
      (let [maybe-parsed (parse-response endpoint db body)]
        (cond
          maybe-parsed maybe-parsed
          (= endpoint :unknown) (try-download-public-decklist db deck-id :private)
          :else nil))
      :else (do (timbre/info (Exception. (str "Failed to download deck " deck-id " using endpoint " endpoint ", status: " error)))
                (when (= endpoint :unknown)
                  (try-download-public-decklist db deck-id :private))))))

(defn download-public-decklist
  [db input]
  (let [[endpoint deck-id] (parse-input input)]
    (when deck-id (try-download-public-decklist db deck-id endpoint))))
