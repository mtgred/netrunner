(ns tasks.images
  "Tasks to import alt art, alternate language, and high-res card images"
  (:require [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer :all]
            [tasks.utils :refer [replace-collection]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def ^:const alt-art-sets "data/promos.edn")
(def ^:const img-directory ["resources" "public" "img" "cards"])

(def ^:const alt-collection "altarts")
(def ^:const card-collection "cards")

(defn find-dirs
  "List directories in the argument directory"
  [d]
  (->> d
    .listFiles
    (filter #(.isDirectory %))))

(defn find-files
  "List files in the argument directory"
  [d]
  (->> d
    .listFiles
    (remove #(.isDirectory %))))

(defn read-alt-sets
  "Read in the alt art set information"
  []
  (-> alt-art-sets
    slurp
    edn/read-string))

(defn remove-old-images
  "Remove images attached to cards in the db"
  []
  (println "Removing old images from db cards")
  (mc/update db card-collection {} {$unset {:faces 1}} {:multi true})
  (mc/update db card-collection {} {$unset {:images 1}} {:multi true}))

(defn- add-flip-card-image
  [lang resolution art-set filename]
  (let [code-face (first (string/split filename #"\."))
        code-face-split (string/split code-face #"-")
        code (first code-face-split)
        face (second code-face-split)
        k (string/join "." ["faces" face "images" (name lang) (name resolution) (name art-set)])
        prev-k-root (if (= :stock art-set) code (name art-set))
        prev-k (string/join "." ["faces" face "images" (name lang) (name resolution) prev-k-root])
        path (string/join "/" ["/img/cards" (name lang) (name resolution) (name art-set) filename])]
    (mc/update db card-collection {:code code} {$set {k path}})
    (mc/update db card-collection {:previous-versions code} {$set {prev-k path}})))

(defn- add-card-image
  "Add an image to a card in the db"
  ([lang resolution f] (add-card-image lang resolution :stock f))
  ([lang resolution art-set f]
   (let [filename (.getName f)]
     (if (string/includes? filename "-")
       (add-flip-card-image lang resolution art-set filename)
       (let [code (first (string/split filename #"\."))
             k (string/join "." ["images" (name lang) (name resolution) (name art-set)])
             prev-k-root (if (= :stock art-set) code (name art-set))
             prev-k (string/join "." ["images" (name lang) (name resolution) prev-k-root])
             path (string/join "/" ["/img/cards" (name lang) (name resolution) (name art-set) filename])]
         (mc/update db card-collection {:code code} {$set {k path}})
         (mc/update db card-collection {:previous-versions code} {$set {prev-k path}}))))))

(defn- add-alt-images
  "All all images in the specified alt directory"
  [lang resolution alt-dir]
  (let [alt (keyword (.getName alt-dir))
        images (find-files alt-dir)]
    (doall (map #(add-card-image lang resolution alt %) images))
    (println "Added" (count images) "images to" lang resolution alt)))

(defn- add-resolution-images
  "Add all images in the specified resolution directory"
  [lang res-dir]
  (let [resolution (keyword (.getName res-dir))
        alts (find-dirs res-dir)
        images (find-files res-dir)]
    (doall (map #(add-alt-images lang resolution %) alts))))

(defn- add-language-images
  "Add all images in the specified language directory"
  [lang-dir]
  (let [lang (keyword (.getName lang-dir))
        resolutions (find-dirs lang-dir)]
    (doall (map #(add-resolution-images lang %) resolutions))))

(defn add-images
  "Add alt art, alternate language, and high-res card images to the database"
  []
  (try
    (let [alt-sets (read-alt-sets)
          card-dir (apply io/file img-directory)
          langs (find-dirs card-dir)]
      (replace-collection alt-collection alt-sets)
      (remove-old-images)
      (doall (map add-language-images langs)))
    (catch Exception e (do
                         (println "Image import failed:" (.getMessage e))
                         (.printStackTrace e)))))
