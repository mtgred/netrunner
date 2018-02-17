(ns web.data
  (:require [web.db :refer [db object-id]]
            [web.utils :refer [response]]
            [monger.collection :as mc]
            [monger.result :refer [acknowledged?]]
            [web.config :refer [server-config]]))

(defn cards-handler [req]
  (let [r (map #(dissoc % :_id) (mc/find-maps db "cards" nil))]
    (response 200 r)))

(defn alt-arts-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "altarts" nil))))

(defn sets-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "sets" nil))))

(defn mwl-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "mwl" nil))))

(defn cycles-handler [req]
  (response 200 (map #(dissoc % :_id) (mc/find-maps db "cycles" nil))))

(defn donors-handler [req]
  (response 200 (map #(or (:username %) (:name %)) (mc/find-maps db "donators" nil))))

(defn cards-version-handler [req]
  (response 200 {:version (int (:cards-version (mc/find-one-as-map db "config" nil)))}))