(ns tasks.altart
  "Alternative and Promo card art import tasks"
  (:require [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [tasks.nrdb :refer [replace-collection]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint] :as pprint]
            [cheshire.core :as json]))

(def ^:const alt-art-sets "data/promo.json")
(def ^:const img-directory ["resources" "public" "img" "cards"])

(def ^:const alt-collection "clj_altarts")

(defn read-alt-sets
  "Read in the alt art set information"
  []
  (-> alt-art-sets
    slurp
    (json/parse-string true)))

(defn find-alt-files
  "Find files of the form code-set.png in the card image directory"
  [suffixes]
  (->> img-directory
    (apply io/file)
    .list
    (map #(string/split % #"[-\.]"))
    (filter #(= 3 (count %)))
    (filter (fn [x] (some #(= (second x) %) suffixes)))
    (group-by second)))

(defn add-cards
  "Add alt cards to the alt sets map"
  [alt-sets alt-cards]
  (reduce (fn [acc v]
            (let [suffix (:version v)
                  cards (->> suffix
                          alt-cards
                          (map first)
                          sort)]
            (conj acc (assoc v :cards cards))))
             nil
             alt-sets))

(defn add_art
  "Add alt art card images to the database"
  []
  (webdb/connect)
  (try
    (let [alt-sets (read-alt-sets)
          alt-files (find-alt-files (map :version alt-sets))
          alt-sets-cards (add-cards alt-sets alt-files)]
      (replace-collection alt-collection alt-sets-cards)
      (println (count alt-sets-cards) "alt art sets imported")
      )
    (catch Exception e (do
                         (println "Alt art import failed:" (.getMessage e))
                         (.printStackTrace e)))
    (finally (webdb/disconnect))))

