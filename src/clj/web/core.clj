(ns web.core
  (:require
    [monger.collection :as mc]
    [tasks.index :refer [create-indexes]]
    [tasks.nrdb :refer [fetch-data]]
    [web.angel-arena :as angel-arena]
    [web.config :refer [frontend-version server-mode]]
    [web.lobby :as lobby]
    [web.system :refer [start stop]]
    [web.utils :refer [tick]])
  (:gen-class :main true))

(defn -main [& args]
  (when (#{"dev" "prod"} (first args))
    (reset! server-mode (first args)))

  (let [system (start)
        db (get-in system [:mongodb/connection :db])
        port (get-in system [:web/base :port])]

    ;; TODO (noah): This should be in the system, but the fetch and index stuff
    ;; create cyclic dependencies, so I gotta research better ways to do that first
    (if-let [config (mc/find-one-as-map db "config" nil)]
      (reset! frontend-version (:version config))
      (do (mc/create db "config" nil)
          (mc/insert db "config" {:version "0.1.0" :cards-version 0})
          (fetch-data {:db db
                       :card-images true
                       :db-connection true})
          (create-indexes db)))

    ;; Clear inactive lobbies after 30 minutes
    (tick #(lobby/clear-inactive-lobbies db 1800) 1000)
    (tick #(angel-arena/check-for-inactivity db) 1000)
    (tick #(lobby/reset-send-lobby) 1000)

    (println "Jinteki server running in" @server-mode "mode on port" port)
    (println "Frontend version " @frontend-version)

    (.addShutdownHook (Runtime/getRuntime) (Thread. #(stop system)))))
