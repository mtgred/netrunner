(ns tasks.utils
  "utilities for the tasks"
  (:require [clojure.string :as string]))

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
    "Program" (if (and (:subtype card)
                       (> (.indexOf (:subtype card) "Icebreaker") -1))
                "icebreakers"
                "programs")
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

(defn slugify
  "As defined here: https://you.tools/slugify/"
  ([s] (slugify s "-"))
  ([s sep]
   (if (nil? s) ""
     (as-> s s
       (java.text.Normalizer/normalize s java.text.Normalizer$Form/NFD)
       (string/replace s #"[\P{ASCII}]+" "")
       (string/lower-case s)
       (string/trim s)
       (string/split s #"[\p{Space}\p{Punct}]+")
       (filter seq s)
       (string/join sep s)))))
