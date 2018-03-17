(ns web.core
  (:require [web.api :refer [app]]
            [monger.collection :as mc]
            [jinteki.cards :as cards]
            [web.config :refer [frontend-version server-config server-mode]]
            [web.ws :as ws]
            [web.db :refer [db]]
            [web.chat :as chat]
            [web.lobby :as lobby]
            [web.game :as game]
            [web.stats :as stats]
            [jinteki.nav :as nav]
            [clj-time.format :as f])
  (:gen-class :main true))

(defonce server (atom nil))


(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  (let [port (or (-> server-config :web :port) 4141)]
    (web.db/connect)
    (let [cards (mc/find-maps db "cards" nil)
          sets (mc/find-maps db "sets" nil)
          cycles (mc/find-maps db "cycles" nil)
          mwl (mc/find-maps db "mwl" nil)
          latest_mwl (->> mwl
                       (map (fn [e] (update e :date_start #(f/parse (f/formatters :date) %))))
                       (sort-by :date_start)
                       (last))]
      (reset! cards/all-cards (into {} (map (juxt :title identity)
                                            (sort-by (complement :rotated) cards))))
      (reset! cards/sets sets)
      (reset! cards/cycles cycles)
      (reset! cards/mwl latest_mwl))

    (when (#{"dev" "prod"} (first args))
      (reset! server-mode (first args)))

    (if-let [config (mc/find-one-as-map db "config" nil)]
      (reset! frontend-version (:version config))
      (do (mc/create db "config" nil)
          (mc/insert db "config" {:version "0.1.0" :cards-version 0})))

    ;; Clear inactive lobbies after 15 minutes
    (web.utils/tick #(lobby/clear-inactive-lobbies 900) 1000)
    (web.utils/tick lobby/send-lobby 1000)

    (reset! server (org.httpkit.server/run-server app {:port port}))
    (println "Jinteki server running in" @server-mode "mode on port" port)
    (println "Frontend version " @frontend-version))

  (ws/start-ws-router!))
