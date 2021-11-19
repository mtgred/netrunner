(ns web.system
  (:require
    [web.config :refer [frontend-version server-mode]]
    [clj-time.format :as f]
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
    [monger.collection :as mc]
    [monger.core :as mg]
    [org.httpkit.server :refer [run-server server-stop!]]
    [taoensso.sente :as sente]
    [web.angel-arena :as angel-arena]
    [web.api :refer [make-app]]
    [web.lobby :as lobby]
    [web.new-lobby]
    [web.utils :refer [tick]]
    [web.ws :refer [ch-chsk event-msg-handler]]))

(defn build-config []
  {:mongodb/connection {:address "localhost"
                        :port 27017
                        :name "netrunner"}
   :web/app (ig/ref :mongodb/connection)
   :web/server {:port 1042
                :app (ig/ref :web/app)}
   :web/lobby {:interval 1000
               :mongo (ig/ref :mongodb/connection)
               :time-inactive 1800}
   :frontend-version (ig/ref :mongodb/connection)
   :server-mode "dev"
   :sente/router nil
   :game/quotes nil
   :jinteki/cards (ig/ref :mongodb/connection)})

(defmethod ig/init-key :mongodb/connection [_ opts]
  (let [{:keys [address port name]} opts]
    (mg/connect-via-uri (str "mongodb://" address ":" port "/" name))))

(defmethod ig/halt-key! :mongodb/connection [_ {:keys [conn]}]
  (mg/disconnect conn))

(defmethod ig/init-key :web/app [_ opts]
  (make-app opts))

(defmethod ig/init-key :web/server [_ {:keys [app port]}]
  (run-server app {:port port
                   :legacy-return-value? false}))

(defmethod ig/halt-key! :web/server [_ server]
  (when server
    (server-stop! server nil)))

(defmethod ig/init-key :web/lobby [_ {:keys [interval mongo time-inactive]}]
  (let [db (:db mongo)]
    [(tick #(lobby/clear-inactive-lobbies db time-inactive) interval)
     (tick #(angel-arena/check-for-inactivity db) interval)
     (tick #(lobby/reset-send-lobby) interval)]))

(defmethod ig/halt-key! :web/lobby [_ futures]
  (run! future-cancel futures))

(defmethod ig/init-key :frontend-version [_ {:keys [db]}]
  (if-let [config (mc/find-one-as-map db "config" nil)]
    (reset! frontend-version (:version config))
    (doto db
      (mc/create "config" nil)
      (mc/insert "config" {:version @frontend-version
                           :cards-version 0}))))

(defmethod ig/init-key :server-mode [_ mode]
  (reset! server-mode mode))

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

(defmethod ig/init-key :jinteki/cards [_ {:keys [db]}]
  (let [cards (mc/find-maps db "cards" nil)
        stripped-cards (map #(update % :_id str) cards)
        all-cards (into {} (map (juxt :title identity) stripped-cards))
        sets (mc/find-maps db "sets" nil)
        cycles (mc/find-maps db "cycles" nil)
        mwl (mc/find-maps db "mwls" nil)
        latest-mwl (->> mwl
                        (map (fn [e] (update e :date-start #(f/parse (f/formatters :date) %))))
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
  (let [config (build-config)]
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
