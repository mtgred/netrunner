(ns web.dev
  (:require
   [integrant.repl :as ig-repl]
   [potemkin :refer [import-vars]]
   [web.core]
   [web.system :as system]
   [web.logs :refer [timbre-init!]]
   [tasks.nrdb :as nrdb]))

(ig-repl/set-prep! (fn [] (system/server-config)))
(timbre-init!)

(import-vars
  [integrant.repl
   go
   halt
   reset])

(defn restart [] (halt) (go))

(defn fetch-cards []
  (nrdb/fetch-data {:db true})
  (system/stop :jinteki/cards)
  (system/start :jinteki/cards))
