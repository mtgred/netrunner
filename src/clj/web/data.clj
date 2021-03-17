(ns web.data
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [monger.query :as mq]
            [web.config :refer [server-config]]
            [game.core.initializing :refer [card-implemented]]
            [clojure.edn :as edn]))

(defn news-handler [req]
  (let [data (mq/with-collection db "news"
               (mq/find {})
               (mq/fields [:_id :item :date])
               (mq/sort (array-map :date -1)))]
    (response 200 data)))

(defn- cards-version-impl []
  (:cards-version (mc/find-one-as-map db "config" nil)))

(def cards-version (memoize cards-version-impl))

(defn cards-version-handler [req]
  (response 200 {:version (int (cards-version))}))

(defn- enriched-cards-impl []
  (let [cards (mc/find-maps db "cards")]
    (->> cards
         (map #(assoc % :implementation (card-implemented %)))
         (map #(dissoc % :_id)))))

(def enriched-cards (memoize enriched-cards-impl))

(defn cards-handler [req]
  (response 200 (enriched-cards)))

(defn alt-arts-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "altarts"))))

(defn sets-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "sets"))))

(defn mwl-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "mwls"))))

(defn cycles-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "cycles"))))

(defn donors-handler [req]
  (response 200 (->> (mc/find-maps db "donators")
                     (map #(let [amount (:amount %)]
                             (assoc % :amount (if (string? amount)
                                                (edn/read-string amount)
                                                amount))))
                     (sort-by :amount >)
                     (map #(let [username (:username %)]
                             (if (empty? username)
                               (:name %)
                               username))))))
