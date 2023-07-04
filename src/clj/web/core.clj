(ns web.core
  (:require
   [monger.collection :as mc]
   [web.system :refer [start stop]])
  (:gen-class :main true))

(defn -main [& _args]
  (let [system (start)
        port (-> system :web/server :port)
        server-mode (:server/mode system)
        db (-> system :mongodb/connection :db)
        config (mc/find-one-as-map db "config" nil)
        frontend-version (:version config)]
    (println "Jinteki server running in" server-mode "mode on port" port)
    (println "Frontend version" frontend-version)

    (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop system)))))
