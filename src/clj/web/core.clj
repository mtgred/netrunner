(ns web.core
  (:require [web.api :refer [app]]
            [monger.collection :as mc]
            [jinteki.cards :refer [all-cards]]
            [web.config :refer [frontend-version server-config]]
            [web.ws :as ws]
            [web.db :refer [db]]
            [web.chat :as chat]
            [web.lobby :as lobby]
            [web.game :as game]
            [web.stats :as stats]
            [jinteki.nav :as nav])
  (:gen-class :main true))

(defonce server (atom nil))


(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  (let [port (or (-> server-config :web :port) 4141)]
    (web.db/connect)
    (let [cards (mc/find-maps db "cards" nil)]
      (reset! all-cards (into {} (map (juxt :title identity) cards))))

    (if-let [config (mc/find-one-as-map db "config" nil)]
      (reset! frontend-version (:version config))
      (do (mc/create db "config" nil)
          (mc/insert db "config" {:version "0.1.0"})))


    (web.utils/tick lobby/send-lobby 1000)

    (reset! server (org.httpkit.server/run-server app {:port port}))
    (println "Jinteki server running on port" port)
    (println "Frontend version " @frontend-version))

  (ws/start-ws-router!))