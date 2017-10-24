(ns web.core
  (:require [web.api :refer [app]]
            [immutant.web]
            [web.ws :as ws]
            [web.chat :as chat]
            [web.lobby :as lobby]
            [web.game :as game]))


(defonce server (atom nil))

(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  (immutant.util/set-log-level! :WARN)
  (let [port (or (first args) 4141)]
    (web.db/connect)
    (web.utils/tick lobby/send-lobby 1000)
    (reset! server (immutant.web/run app {:port port}))
    (println "Jinteki server running on port" port))
  (ws/start-ws-router!))