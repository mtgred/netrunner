(ns web.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [web.api :refer [app]]
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
            [clj-time.format :as f]
            [game.core :as core]
            [game.quotes :as quotes]
            [hawk.core :as hawk])
  (:gen-class :main true))

(defonce server (atom nil))


(defn stop-server! []
  (when-not (nil? @server)
    (@server :timeout 100)
    (reset! server nil)))

(defn load-card-data
  [e]
  (let [path (-> e :file str io/file)]
    (when (and (.isFile path)
               (string/ends-with? path ".edn"))
      (->> (slurp path)
           edn/read-string
           ((juxt :title identity))
           (swap! cards/all-cards merge)))))

(defn -main [& args]
  (let [port (or (-> server-config :web :port) 4141)]
    (web.db/connect)
    (let [sets (mc/find-maps db "sets" nil)
          cycles (mc/find-maps db "cycles" nil)
          mwl (mc/find-maps db "mwls" nil)
          latest_mwl (->> mwl
                       (map (fn [e] (update e :date_start #(f/parse (f/formatters :date) %))))
                       (sort-by :date_start)
                       (last))]
      (core/reset-card-data)
      (core/reset-card-defs)
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
    (println "Frontend version " @frontend-version)

    ;; Set up the watch on quotes files, and load them once.
    (hawk/watch! [{:paths ["data/cards"]
                   :filter hawk/file?
                   :handler (fn [ctx e]
                              (load-card-data e))}
                  {:paths [quotes/quotes-corp-filename
                           quotes/quotes-runner-filename]
                   :handler (fn [ctx e]
                              (when (= :create (:kind e))
                                (quotes/load-quotes!)))}])
    (quotes/load-quotes!))

  (ws/start-ws-router!))
