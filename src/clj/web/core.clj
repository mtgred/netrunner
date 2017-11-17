(ns web.core
  (:require [web.api :refer [app]]
            [immutant.web]
            [monger.collection :as mc]
            [jinteki.cards :refer [all-cards]]
            [web.ws :as ws]
            [web.db :refer [db]]
            [web.chat :as chat]
            [web.lobby :as lobby]
            [web.game :as game]
            [web.stats :as stats]))

(defonce server (atom nil))

(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  (immutant.util/set-log-level! :WARN)
  (let [port (or (first args) 4141)]
    (web.db/connect)
    (let [cards (mc/find-maps db "cards" nil)]
      (reset! all-cards (into {} (map (juxt :title identity) cards))))

    (web.utils/tick lobby/send-lobby 1000)

    (reset! server (immutant.web/run app {:port port}))
    (println "Jinteki server running on port" port))
  (ws/start-ws-router!))