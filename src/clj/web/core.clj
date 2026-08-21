(ns web.core
  (:require
   [monger.collection :as mc]
   [taoensso.timbre :as timbre]
   [web.system :refer [start stop]])
  (:gen-class))

(defn -main [& _args]
  (when-let [system (start)]
    (let [port (:port (:web/server system))
          server-mode (:server/mode system)
          db (:db (:mongodb/connection system))
          config (mc/find-one-as-map db "config" nil)
          frontend-version (:version config)]
      (timbre/info (str "Jinteki server running in " server-mode " mode on port " port))
      (timbre/info (str "Frontend version " frontend-version))
      (.addShutdownHook (Runtime/getRuntime) (Thread. (fn []
                                                        (shutdown-agents)
                                                        (stop system)))))))
