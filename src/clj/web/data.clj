(ns web.data
  (:require [web.utils :refer [response mongo-time-to-utc-string]]
            [monger.collection :as mc]
            [monger.query :as mq]
            [game.core.initializing :refer [card-implemented]]
            [clojure.edn :as edn]))

(defn news-handler [{db :system/db}]
  (let [data (mq/with-collection db (.getCollection db "news")
               (mq/find {})
               (mq/fields [:_id :item :date])
               (mq/sort (array-map :date -1)))
        data (mapv #(update % :date mongo-time-to-utc-string) data)]
    (response 200 data)))

(defn- cards-version [db]
  (:cards-version (mc/find-one-as-map db "config" nil)))

(defn cards-version-handler [{db :system/db}]
  (response 200 {:version (int (cards-version db))}))

(defn- enriched-cards [db]
  (let [cards (mc/find-maps db "cards")]
    (mapv #(-> % (assoc :implementation (card-implemented %)) (dissoc :_id)) cards)))

(defn cards-handler [{db :system/db}]
  (response 200 (enriched-cards db)))

(defn- validate-lang
  [lang]
  (contains? #{"de" "es" "fr" "it" "ja" "ko" "pl" "zh"} lang))

(defn lang-handler [{db :system/db {lang :lang} :path-params}]
  (if (validate-lang lang)
    (response 200 (mapv #(dissoc % :_id) (mc/find-maps db (str "cards-" lang))))
    (response 200 {})))

(defn alt-arts-handler [{db :system/db}]
  (response 200 (mapv #(dissoc % :_id) (mc/find-maps db "altarts"))))

(defn sets-handler [{db :system/db}]
  (response 200 (mapv #(dissoc % :_id) (mc/find-maps db "sets"))))

(defn mwl-handler [{db :system/db}]
  (response 200 (mapv #(dissoc % :_id) (mc/find-maps db "mwls"))))

(defn cycles-handler [{db :system/db}]
  (response 200 (mapv #(dissoc % :_id) (mc/find-maps db "cycles"))))

(defn donors-handler [{db :system/db}]
  (response 200 (->> (mc/find-maps db "donators")
                     (mapv #(let [amount (:amount %)]
                              (assoc % :amount (if (string? amount)
                                                 (edn/read-string amount)
                                                 amount))))
                     (sort-by :amount >)
                     (mapv #(let [username (:username %)]
                              (if (empty? username)
                                (:name %)
                                username))))))
