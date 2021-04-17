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
(def ^:const overrides-img-directory ["resources" "public" "img" "cards" "overrides"])

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
  [base-path lang resolution art-set filename]
  (let [code-face (first (string/split filename #"\."))
        code-face-split (string/split code-face #"-")
        code (first code-face-split)
        face (second code-face-split)
        k (string/join "." ["faces" face "images" (name lang) (name resolution) (name art-set)])
        prev-k-root (if (= :stock art-set) code (name art-set))
        prev-k (string/join "." ["faces" face "images" (name lang) (name resolution) prev-k-root])
        path (string/join "/" [base-path (name lang) (name resolution) (name art-set) filename])]
    (mc/update db card-collection {:code code} {$set {k path}})
    (mc/update db card-collection {:previous-versions {$elemMatch {:code code}}} {$set {prev-k path}})))

(defn- add-card-image
  "Add an image to a card in the db"
  ([base-path lang resolution f] (add-card-image base-path lang resolution :stock f))
  ([base-path lang resolution art-set f]
   (let [filename (.getName f)]
     (if (string/includes? filename "-")
       (add-flip-card-image base-path lang resolution art-set filename)
       (let [code (first (string/split filename #"\."))
             k (string/join "." ["images" (name lang) (name resolution) (name art-set)])
             prev-k-root (if (= :stock art-set) code (name art-set))
             prev-k (string/join "." ["images" (name lang) (name resolution) prev-k-root])
             path (string/join "/" [base-path (name lang) (name resolution) (name art-set) filename])]
         (mc/update db card-collection {:code code} {$set {k path}})
         (mc/update db card-collection {:previous-versions {$elemMatch {:code code}}} {$set {prev-k path}}))))))

(defn- add-alt-images
  "All all images in the specified alt directory"
  [base-path lang resolution alt-dir]
  (let [alt (keyword (.getName alt-dir))
        images (find-files alt-dir)]
    (doall (map #(add-card-image base-path lang resolution alt %) images))
    (println "Added" (count images) "images to" lang resolution alt)))

(defn- add-resolution-images
  "Add all images in the specified resolution directory"
  [base-path lang res-dir]
  (let [resolution (keyword (.getName res-dir))
        alts (find-dirs res-dir)
        images (find-files res-dir)]
    (doall (map #(add-alt-images base-path lang resolution %) alts))))

(defn- add-language-images
  "Add all images in the specified language directory"
  [base-path lang-dir]
  (let [lang (keyword (.getName lang-dir))
        resolutions (find-dirs lang-dir)]
    (doall (map #(add-resolution-images base-path lang %) resolutions))))

(defn add-images
  "Add alt art, alternate language, and high-res card images to the database"
  []
  (try
    (let [alt-sets (read-alt-sets)
          card-dir (apply io/file img-directory)
          langs (remove #(= "overrides" (.getName %)) (find-dirs card-dir))
          overrides-dir (apply io/file overrides-img-directory)]
      (replace-collection alt-collection alt-sets)
      (remove-old-images)
      (doall (map (partial add-language-images "/img/cards") langs))
      (println "Adding override images...")
      (doall (for [o (find-dirs overrides-dir)]
               (let [overrides-langs (find-dirs o)]
                 (doall (map (partial add-language-images (str "/img/cards/overrides/" (.getName o))) overrides-langs))))))
    (catch Exception e (do
                         (println "Image import failed:" (.getMessage e))
                         (.printStackTrace e)))))
