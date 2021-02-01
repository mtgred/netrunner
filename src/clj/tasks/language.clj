(ns tasks.language
  "Alternate card language import tasks"
  (:require [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [tasks.nrdb :refer [update-config]]
            [clojure.string :as string]
            [clojure.java.io :as io]))

(def ^:const img-directory ["resources" "public" "img" "cards"])
(def ^:const card-collection "cards")

(defn find-language-files
  "Find files of the form code[LANG].png in the card image directory"
  []
  (->> img-directory
    (apply io/file)
    .list
    (map #(string/split % #"[\[\]]"))
    (filter #(= 3 (count %)))
    (group-by first)))

(defn remove-old-languages
  "Remove any languages attached to cards in the db"
  []
  (println "Removing old languages")
  (mc/update db card-collection {} {$unset {:languages 1}} {:multi true}))

(defn add-to-card
  "Add additional languages to a card"
  [[code languages-vec]]
  (let [languages (map second languages-vec)]
    (println "Adding" code ":" languages)
    (mc/update db card-collection {:code code} {$set {:languages languages}})))

(defn add-language
  "Add additional language card images to the database"
  ([] (add-language true))
  ([standalone?]
   (when standalone?
     (webdb/connect))
   (try
     (let [language-files (find-language-files)]
       (println (count language-files) "cards with additional languages found")
       (remove-old-languages)
       (doall (map add-to-card language-files))
       (when standalone?
         (update-config)))
     (catch Exception e (do
                          (println "Additional language import failed:" (.getMessage e))
                          (.printStackTrace e)))
     (finally (when standalone? (webdb/disconnect))))))
