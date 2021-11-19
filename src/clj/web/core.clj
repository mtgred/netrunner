(ns web.core
  (:require
    [web.config :refer [frontend-version server-mode]]
    [web.system :refer [start stop]])
  (:gen-class :main true))

(defn -main [& _args]
  (let [system (start)
        port (get-in system [:web/server :port])]
    (reset! server-mode "prod")
    (println "Jinteki server running in" @server-mode "mode on port" port)
    (println "Frontend version " @frontend-version)

    (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop system)))))
