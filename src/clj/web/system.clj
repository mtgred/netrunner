(ns web.system
  (:require
   [aero.core :as aero]
   [cljc.java-time.local-date :as ld]
   [clojure.java.io :as io]
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
   [game.quotes :refer [load-quotes!]]
   [integrant.core :as ig]
   [jinteki.cards :as cards]
   [medley.core :refer [deep-merge]]
   [monger.collection :as mc]
   [monger.core :as mg]
   [org.httpkit.server :refer [run-server server-stop!]]
   [taoensso.sente :as sente]
   [time-literals.data-readers]
   [time-literals.read-write :as read-write]
   [web.angel-arena :as angel-arena]
   [web.api :refer [make-app make-dev-app]]
   [web.app-state :as app-state]
   [web.game]
   [web.lobby :as lobby]
   [web.telemetry]
   [web.utils :refer [tick]]
   [web.versions :refer [banned-msg frontend-version]]
   [web.ws :refer [ch-chsk event-msg-handler]]))

(read-write/print-time-literals-clj!)

(defmethod aero/reader 'ig/ref
  [_ _ value]
  (ig/ref value))

(defn server-config []
  (let [dev-file (io/resource "dev.edn")
        dev-config (when dev-file (aero/read-config dev-file))
        prod-file (io/resource "prod.edn")
        master-config (when prod-file (aero/read-config prod-file))]
    (deep-merge dev-config master-config)))

(defmethod ig/init-key :server/mode [_ mode]
  mode)

(defmethod ig/init-key :mongodb/connection [_ opts]
  (let [{:keys [address port name connection-string]} opts
        connection (or connection-string (str "mongodb://" address ":" port "/" name))]
    (mg/connect-via-uri connection)))

(defmethod ig/halt-key! :mongodb/connection [_ {:keys [conn]}]
  (mg/disconnect conn))

(defmethod ig/init-key :web/app [_ opts]
  (if (:server-mode opts)
    (make-app opts)
    (make-dev-app opts)))

(defmethod ig/init-key :web/app-state [_ _]
  (reset! app-state/app-state
          {:lobbies {}
           :lobby-updates {}
           :users {}})
  (reset! angel-arena/arena-queue []))

(defmethod ig/init-key :web/server [_ {:keys [app port]}]
  (run-server app {:port port
                   :legacy-return-value? false}))

(defmethod ig/halt-key! :web/server [_ server]
  (when server
    (server-stop! server nil)))

(defmethod ig/init-key :web/auth [_ settings]
  settings)

(defmethod ig/init-key :web/lobby [_ {:keys [interval mongo time-inactive]}]
  (let [db (:db mongo)]
    [(tick #(lobby/clear-inactive-lobbies db time-inactive) interval)
     (tick #(angel-arena/check-for-inactivity db) interval)]))

(defmethod ig/halt-key! :web/lobby [_ futures]
  (run! future-cancel futures))

(defmethod ig/init-key :web/chat [_ settings]
  settings)

(defmethod ig/init-key :web/email [_ settings]
  settings)

(defmethod ig/init-key :web/banned-msg [_ {initial :initial
                                           {:keys [db]} :mongo}]
  (if-let [config (mc/find-one-as-map db "config" nil)]
    (do (reset! banned-msg (:banned-msg config))
        config)
    (do (doto db
          (mc/create "config" nil)
          (mc/insert-and-return "config" {:banned-msg initial}))
        (reset! banned-msg initial))))

(defmethod ig/init-key :frontend/version [_ {initial :initial
                                             {:keys [db]} :mongo}]
  (if-let [config (mc/find-one-as-map db "config" nil)]
    (do (reset! frontend-version (:version config))
        config)
    (do (doto db
          (mc/create "config" nil)
          (mc/insert-and-return "config" {:version initial
                                          :cards-version 0}))
        (reset! frontend-version initial))))

(defmethod ig/init-key :sente/router [_ _opts]
  (sente/start-server-chsk-router!
    ch-chsk
    event-msg-handler))

(defmethod ig/halt-key! :sente/router [_ stop-fn]
  (when (fn? stop-fn)
    (stop-fn)))

(defmethod ig/init-key :game/quotes [_ _opts]
  (load-quotes!))

(defn- format-card-key->string
  [fmt]
  (assoc fmt :cards
         (reduce-kv
           (fn [m k v]
             (assoc m (name k) v))
           {} (:cards fmt))))

(defmethod ig/init-key :jinteki/cards [_ {{:keys [db]} :mongo}]
  (let [cards (mc/find-maps db "cards" nil)
        stripped-cards (mapv #(update % :_id str) cards)
        all-cards (into {} (map (juxt :title identity)) stripped-cards)
        sets (mc/find-maps db "sets" nil)
        cycles (mc/find-maps db "cycles" nil)
        mwl (mc/find-maps db "mwls" nil)
        latest-mwl (->> mwl
                        (map (fn [e] (update e :date-start ld/parse)))
                        (group-by #(keyword (:format %)))
                        (mapv (fn [[k, v]] [k (->> v
                                                  (sort-by :date-start)
                                                  (last)
                                                  (format-card-key->string))]))
                        (into {}))]
    (reset! cards/all-cards all-cards)
    (reset! cards/sets sets)
    (reset! cards/cycles cycles)
    (reset! cards/mwl latest-mwl)
    {:all-cards all-cards
     :sets sets
     :cycles cycles
     :mwl latest-mwl}))

(defmethod ig/halt-key! :jinteki/cards [_ _opts]
  (reset! cards/all-cards nil)
  (reset! cards/sets nil)
  (reset! cards/cycles nil)
  (reset! cards/mwl nil))

(defn start
  [& [{:keys [only]}]]
  (let [config (server-config)]
    (if only
      (ig/init config only)
      (ig/init config))))

(defn stop [system & [{:keys [only]}]]
  (when system
    (if only
      (ig/halt! system only)
      (ig/halt! system)))
  nil)

(comment
  (def system (start))
  (stop system)
  )
