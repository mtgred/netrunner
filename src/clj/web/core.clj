(ns web.core
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clj-time.format :as f]
            [hawk.core :as hawk]
            [monger.collection :as mc]
            [game.core :as core]
            [game.quotes :as quotes]
            [jinteki.cards :as cards]
            [jinteki.nav :as nav]
            [tasks.nrdb :refer [replace-collection update-config]]
            [tasks.altart :refer [add-art]]
            [web.api :refer [app]]
            [web.chat :as chat]
            [web.config :refer [frontend-version server-config server-mode]]
            [web.db :refer [db]]
            [web.game :as game]
            [web.lobby :as lobby]
            [web.stats :as stats]
            [web.ws :as ws]
            [game.cards.agendas]
            [game.cards.assets]
            [game.cards.basic]
            [game.cards.events]
            [game.cards.hardware]
            [game.cards.ice]
            [game.cards.identities]
            [game.cards.operations]
            [game.cards.programs]
            [game.cards.resources]
            [game.cards.upgrades]
            )
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
          all-cards (into {} (map (juxt :title identity) cards))
          sets (mc/find-maps db "sets" nil)
          cycles (mc/find-maps db "cycles" nil)
          mwl (mc/find-maps db "mwls" nil)
          latest-mwl (->> mwl
                          (filter #(= "standard" (:format %)))
                          (map (fn [e] (update e :date-start #(f/parse (f/formatters :date) %))))
                          (sort-by :date-start)
                          last)
          ;; Gotta turn the card names back to strings
          latest-mwl (assoc latest-mwl
                            :cards
                            (reduce-kv (fn [m k v]
                                         (assoc m (name k) v))
                                       {}
                                       (:cards latest-mwl)))
          ]
      (reset! cards/all-cards all-cards)
      (reset! cards/sets sets)
      (reset! cards/cycles cycles)
      (reset! cards/mwl latest-mwl))

    (when (#{"dev" "prod"} (first args))
      (reset! server-mode (first args)))

    (if-let [config (mc/find-one-as-map db "config" nil)]
      (reset! frontend-version (:version config))
      (do (mc/create db "config" nil)
          (mc/insert db "config" {:version "0.1.0" :cards-version 0})))

    ;; Clear inactive lobbies after 30 minutes
    (web.utils/tick #(lobby/clear-inactive-lobbies 1800) 1000)
    (web.utils/tick lobby/send-lobby 1000)

    (reset! server (org.httpkit.server/run-server app {:port port}))
    (println "Jinteki server running in" @server-mode "mode on port" port)
    (println "Frontend version " @frontend-version)

    ;; Set up the watch on quotes files, and load them once.
    (hawk/watch! [{:paths [quotes/quotes-corp-filename
                           quotes/quotes-runner-filename]
                   :handler (fn [ctx e]
                              (when (= :create (:kind e))
                                (quotes/load-quotes!)))}])
    (quotes/load-quotes!))

  (ws/start-ws-router!))
