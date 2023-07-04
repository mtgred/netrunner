(ns tasks.setup
  (:require [web.system :refer [start stop]]))

(def ^:private task-system-keys {:only [:mongodb/connection :jinteki/cards]})

(defn connect []
  (start task-system-keys))

(defn disconnect [system]
  (stop system task-system-keys))
