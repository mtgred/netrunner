(ns web.system
  (:require
   [aero.core :as aero]
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
   [medley.core :refer [deep-merge dissoc-in]]
   [monger.collection :as mc]
   [org.httpkit.server :refer [run-server server-stop!]]
   [taoensso.carmine :as car :refer [wcar]]
   [time-literals.data-readers]
   [time-literals.read-write :as read-write]
   [web.api :refer [make-app make-dev-app]]
   [web.app-state :as app-state]
   [web.game]
   [web.telemetry]) 
  (:import
   [clojure.lang ExceptionInfo]
   [org.httpkit.server HttpServer]))

;; side-effecting requires for ig/init-key and ig/halt-key! method definitions
(require 'jinteki.cards 'jinteki.i18n 'web.lobby 'web.logs 'web.mongodb 'web.ws)

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

(defmethod ig/init-key :redis/connection [_ {:keys [pool spec] :as opts}]
  {:pool (car/connection-pool pool)
   :spec spec})

(defmethod ig/halt-key! :redis/connection [_ {:keys [pool]}]
  (java.io.Closeable/.close pool))

(defmethod ig/init-key :web/app [_ opts]
  (if (:server-mode opts)
    (make-app opts)
    (make-dev-app opts)))

(defmethod ig/init-key :web/app-state [_ _]
  (reset! app-state/app-state app-state/base-app-state))

(defmethod ig/halt-key! :web/app-state [_ _]
  (reset! app-state/app-state app-state/base-app-state))

(defmethod ig/init-key :web/server [_ {:keys [app port] :as opts}]
  (let [^HttpServer s (run-server app {:port port
                                       :legacy-return-value? false})]
    {:server s
     :port (.getPort s)}))

(defmethod ig/halt-key! :web/server [_ {server :server}]
  (when server
    (server-stop! server nil)))

(defmethod ig/init-key :web/auth [_ settings]
  settings)

(defmethod ig/init-key :web/chat [_ settings]
  settings)

(defmethod ig/init-key :web/email [_ settings]
  settings)

(defmethod ig/init-key :web/banned-msg [_ {:keys [initial redis]
                                           {:keys [db]} :mongo}]
  (if-let [msg (:banned-msg (mc/find-one-as-map db "config" nil))]
    (do (wcar redis (car/set :config/banned-msg msg))
      msg)
    (do (mc/insert-and-return db "config" {:banned-msg initial})
      (wcar redis (car/set :config/banned-msg initial))
      initial)))

(defmethod ig/init-key :frontend/version [_ {:keys [initial redis]
                                             {:keys [db]} :mongo}]
  (if-let [version (:version (mc/find-one-as-map db "config" nil))]
    (do (wcar redis (car/set :config/version version))
      version)
    (do (mc/insert-and-return db "config" {:version initial})
      (wcar redis (car/set :config/version initial))
      initial)))

(defmethod ig/init-key :game/quotes [_ _]
  (load-quotes!))

(defmethod ig/init-key :jinteki/cards-version [_ {:keys [initial redis]
                                                  {:keys [db]} :mongo}]
  (if-let [version (:version (mc/find-one-as-map db "config" nil))]
    (do (wcar redis (car/set :config/cards-version version))
      version)
    (do (mc/insert-and-return db "config" {:version initial})
      (wcar redis (car/set :config/cards-version initial))
      initial)))

(defn stop [system & {:keys [only]}]
  (when system
    (if only
      (ig/halt! system only)
      (ig/halt! system)))
  nil)

(defn start
  [& {:keys [only]}]
  (let [config (server-config)]
    (try (if only
           (ig/init config only)
           (ig/init config))
         (catch ExceptionInfo ex
           (prn (ex-info (ex-message ex) (dissoc-in (ex-data ex) [:system :jinteki/cards]) ex))
           (stop (:system (ex-data ex)))
           nil))))

(comment
  (let [redis (start {:only [:redis/connection]})]
    (prn (wcar redis
          (car/hset :foo 1 "amazing")
          (car/hset :foo 2 "wonderful")
          (car/parse-map (car/hgetall :foo))
          ))
    (stop redis))
  (def system (start {:only [:redis/connection]}))
  (wcar (:redis/connection system)
    (car/set :hello/world 1)
    (car/get :hello/world)
    )
  (stop system)
  )
