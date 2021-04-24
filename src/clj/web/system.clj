(ns web.system
  (:require
    ;; external
    [clj-time.format :as f]
    [integrant.core :as ig]
    [monger.collection :as mc]
    [monger.core :as mg]
    [org.httpkit.server :refer [run-server server-stop!]]
    [taoensso.sente :as sente]
    ;; internal
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
    [jinteki.cards :as cards]
    [web.api :refer [make-app]]
    [web.ws :refer [ch-chsk event-msg-handler]]))

(defn build-config []
  {:mongodb/base {:address "localhost"
                  :port 27017
                  :name "netrunner"}
   :mongodb/connection {:base (ig/ref :mongodb/base)}
   :web/base {:port 1042}
   :web/app {:db (ig/ref :mongodb/connection)}
   :web/server {:base (ig/ref :web/base)
                :app (ig/ref :web/app)}
   :sente/router nil
   :game/quotes nil
   :jinteki/cards {:db (ig/ref :mongodb/connection)}})

(defmethod ig/init-key :mongodb/base [_ opts] opts)

(defmethod ig/init-key :mongodb/connection [_ {:keys [base]}]
  (let [{:keys [address port name]} base]
    (mg/connect-via-uri (str "mongodb://" address ":" port "/" name))))

(defmethod ig/halt-key! :mongodb/connection [_ {:keys [conn]}]
  (mg/disconnect conn))

(defmethod ig/init-key :web/base [_ opts] opts)

(defmethod ig/init-key :web/app [_ {:keys [db]}]
  (make-app db))

(defmethod ig/init-key :web/server [_ {:keys [app base]}]
  (run-server app {:port (:port base)
                   :legacy-return-value? false}))

(defmethod ig/halt-key! :web/server [_ server]
  (when server
    (server-stop! server nil)))

(defmethod ig/init-key :sente/router [_ opts]
  (sente/start-server-chsk-router!
    ch-chsk
    event-msg-handler))

(defmethod ig/halt-key! :sente/router [_ stop-fn]
  (when (fn? stop-fn)
    (stop-fn)))

(defmethod ig/init-key :game/quotes [_ opts]
  (load-quotes!))

(defmethod ig/init-key :jinteki/cards [_ {{:keys [db]} :db}]
  (let [cards (mc/find-maps db "cards" nil)
        stripped-cards (map #(update % :_id str) cards)
        all-cards (into {} (map (juxt :title identity) stripped-cards))
        sets (mc/find-maps db "sets" nil)
        cycles (mc/find-maps db "cycles" nil)
        mwl (mc/find-maps db "mwls" nil)
        latest-mwl (->> mwl
                        (filter #(= "standard" (:format %)))
                        (map (fn [e] (update e :date-start #(f/parse (f/formatters :date) %))))
                        (sort-by :date-start)
                        (last))
        ;; Gotta turn the card names back to strings
        latest-mwl (assoc latest-mwl
                          :cards (reduce-kv
                                   (fn [m k v] (assoc m (name k) v))
                                   {}
                                   (:cards latest-mwl)))]
    (reset! cards/all-cards all-cards)
    (reset! cards/sets sets)
    (reset! cards/cycles cycles)
    (reset! cards/mwl latest-mwl)
    {:all-cards all-cards
     :sets sets
     :cycles cycles
     :mwl latest-mwl}))

(defmethod ig/halt-key! :jinteki/cards [_ opts]
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
  ,)
