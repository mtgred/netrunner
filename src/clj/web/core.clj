(ns web.core
  (:require
   [taoensso.timbre :as timbre]
   [web.system :refer [start stop]])
  (:gen-class))

(defn -main [& _args]
  (when-let [system (start)]
    (let [port (:port (:web/server system))
          server-mode (:server/mode system)
          frontend-version (:frontend/version system)]
      (timbre/info "Jinteki server running in" server-mode "mode on port" port)
      (timbre/info "Frontend version" frontend-version)
      (.addShutdownHook (Runtime/getRuntime) (Thread. (fn []
                                                        (shutdown-agents)
                                                        (stop system)))))))
