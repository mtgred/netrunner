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

(defn deep-merge [v & vs]
  ;; Pulled from https://gist.github.com/danielpcox/c70a8aa2c36766200a95#gistcomment-2313926
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      v)))

(defn replace-collection
  [db col data]
  (mc/remove db col)
  (mc/insert-batch db col data))
