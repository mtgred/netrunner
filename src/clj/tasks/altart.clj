(ns tasks.altart
  "Alternative and Promo card art import tasks"
  (:require [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [tasks.nrdb :refer [replace-collection tables update-config tables]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [cheshire.core :as json]))

(def ^:const alt-art-sets "data/promo.json")
(def ^:const img-directory ["resources" "public" "img" "cards"])

(def ^:const alt-collection "altarts")
(def ^:const card-collection (:collection (:card tables)))

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
             nil alt-sets))

(defn remove-old-alt-art
  "Remove any alt art attached to cards in the db"
  []
  (println "Removing old alt arts")
  (mc/update db card-collection {} {$unset {:alt_art 1}} {:multi true}))

(defn add-alt-art
  "Add single alt art set to cards"
  [{:keys [version name cards] :as alt-set}]
  (let [k (keyword (str "alt_art." version))
        cnt (reduce (fn [acc code]
                      (mc/update db card-collection {:code code} {$set {k (str code "-" version)}})
                      (mc/update db card-collection {:replaces code} {$set {k (str code "-" version)}})
                      (inc acc))
                    0 cards)]
    (println "Added" cnt "alt art cards to set" name)))

(defn add-art
  "Add alt art card images to the database"
  ([] (add-art true))
  ([standalone?]
   (when standalone?
     (webdb/connect))
   (try
     (let [alt-sets (read-alt-sets)
           alt-files (find-alt-files (map :version alt-sets))
           alt-sets-cards (add-cards alt-sets alt-files)]
       (replace-collection alt-collection alt-sets-cards)
       (println (count alt-sets-cards) "alt art sets imported")
       (remove-old-alt-art)
       (doall (map add-alt-art alt-sets-cards))
       (when standalone?
         (update-config (:config tables))))
     (catch Exception e (do
                          (println "Alt art import failed:" (.getMessage e))
                          (.printStackTrace e)))
     (finally (when standalone? (webdb/disconnect))))))