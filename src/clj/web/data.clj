(ns web.data
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]
            [clojure.edn :as edn]))

(defn cards-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "cards"))))

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

(defn cards-version-handler [req]
  (response 200 {:version (int (:cards-version (mc/find-one-as-map db "config" nil)))}))
