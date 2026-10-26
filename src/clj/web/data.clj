(ns web.data
  (:require
   [clojure.edn :as edn]
   [game.core.initializing :refer [card-implemented]]
   [jinteki.i18n :as i18n]
   [monger.collection :as mc]
   [monger.query :as mq]
   [web.utils :refer [cached-response mongo-time-to-utc-string response]]
   [web.versions :refer [cards-version frontend-version]]))

(defn news-handler [{db :system/db}]
  (let [data (mq/with-collection db (.getCollection db "news")
               (mq/find {})
               (mq/fields [:_id :item :date])
               (mq/sort (array-map :date -1)))
        data (mapv #(update % :date mongo-time-to-utc-string) data)]
    (response 200 data)))

(defn- enriched-cards [db]
  (let [cards (mc/find-maps db "cards")]
    (mapv #(-> % (assoc :implementation (card-implemented %)) (dissoc :_id)) cards)))

(defn cards-handler [{db :system/db :as request}]
  (cached-response request
                   (str @frontend-version "-" @cards-version)
                   #(enriched-cards db)))

(defn- validate-lang
  [lang]
  (contains? #{"de" "es" "fr" "it" "ja" "ko" "pl" "zh-simp" "zh-trad"} lang))

(defn card-lang-handler [{db :system/db {lang :lang} :path-params :as request}]
  (if (validate-lang lang)
    (let [coll (case lang
                 "zh-simp" "cards-zh-hans"
                 "zh-trad" "cards-zh-hant"
                 ; else
                 (str "cards-" lang))]
      (cached-response request
                       (str @frontend-version "-" @cards-version "-" lang)
                       #(mapv (fn [card] (dissoc card :_id)) (mc/find-maps db coll))))
    (response 200 {})))

(defn lang-handler [{{lang :lang} :path-params}]
  (if-let [content (i18n/get-content lang)]
    (response 200 (pr-str content))
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
