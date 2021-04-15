(ns tasks.setup
  (:require [web.system :refer [start stop]]))

(defn connect []
  (start {:only [:mongodb/connection :jinteki/cards]}))

(defn disconnect [mongodb]
  (stop mongodb))
