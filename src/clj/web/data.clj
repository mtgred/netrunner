(ns web.data
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]))

(defn cards-handler [req]
  (let [r (map #(dissoc % :_id) (mc/find-maps db "cards"))]
    (response 200 r)))

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
                     (sort-by :amount >)
                     (map #(let [username (:username %)]
                             (if (empty? username)
                               (:name %)
                               username))))))

(defn cards-version-handler [req]
  (response 200 {:version (int (:cards-version (mc/find-one-as-map db "config" nil)))}))
