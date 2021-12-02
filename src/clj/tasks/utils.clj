(ns tasks.utils
  "utilities for the tasks"
  (:require [monger.collection :as mc]))

(defn type->dir
  [card]
  (case (:type card)
    "Agenda" "agendas"
    "Asset" "assets"
    "Event" "events"
    "Fake-Identity" "identities"
    "Hardware" "hardware"
    "ICE" "ice"
    "Identity" "identities"
    "Operation" "operations"
    "Program" "programs"
    "Resource" "resources"
    "Upgrade" "upgrades"))

(defn replace-collection
  [db col data]
  (mc/remove db col)
  (mc/insert-batch db col data))
