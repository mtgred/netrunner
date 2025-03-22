(ns tasks.images
  "Tasks to import alt art, alternate language, and high-res card images"
  (:require
    [monger.collection :as mc]
    [monger.operators :refer :all]
    [tasks.setup :refer [connect disconnect]]
    [tasks.utils :refer [replace-collection]]
    [clojure.string :as str]
    [clojure.java.io :as io]
    [clojure.edn :as edn]))

(def ^:const alt-art-sets "data/promos.edn")
(def ^:const img-directory ["resources" "public" "img" "cards"])
(def ^:const overrides-img-directory ["resources" "public" "img" "cards" "overrides"])

(def ^:const alt-collection "altarts")
(def ^:const card-collection "cards")

(defn find-dirs
  "List directories in the argument directory"
  [dirs]
  (->> dirs
       (.listFiles)
       (filter #(.isDirectory %))))

(defn find-files
  "List files in the argument directory"
  [dirs]
  (->> dirs
       (.listFiles)
       (remove #(.isDirectory %))))

(defn read-alt-sets
  "Read in the alt art set information"
  []
  (-> alt-art-sets
      (slurp)
      (edn/read-string)))

(defn remove-old-images
  "Remove images attached to cards in the db"
  [db]
  (println "Removing old images from db cards")
  (mc/update db card-collection {} {$unset {:faces 1}} {:multi true})
  (mc/update db card-collection {} {$unset {:images 1}} {:multi true}))
;; note: this should select a period, perhaps preceeded by an alphabetic string,
;; so long as it either has front,back,or some numbers behind it
;; the excess dots are because the lookbehind needs to be fixed width
;; but this ensures we don't split on "front.", and instead split on "." for multi-faced cards
(def ^:cost image-select-regex #"(?<=(.tank|house|ewery|front|.back|....[0123456789]))[a-zA-Z]*\.")

(defn- add-flip-card-image
  [db base-path lang resolution art-set filename]
  (let [code-face (first (str/split filename image-select-regex))
        code-face-split (str/split code-face #"-")
        code (first code-face-split)
        face (second code-face-split)
        k (str/join "." ["faces" face "images" (name lang) (name resolution) (name art-set)])
        prev-k-root (if (= :stock art-set) code (name art-set))
        prev-k (str/join "." ["faces" face "images" (name lang) (name resolution) prev-k-root])
        path (str/join "/" [base-path (name lang) (name resolution) (name art-set) filename])]
    (mc/update db card-collection {:code code} {$set {k path}})
    (mc/update db card-collection {:previous-versions {$elemMatch {:code code}}} {$set {prev-k path}})))

(def ^:const cards-to-skip #{"08012" "09001" "26066" "26120"})

(defn- add-card-image
  "Add an image to a card in the db"
  ([db base-path lang resolution f] (add-card-image db base-path lang resolution :stock f))
  ([db base-path lang resolution art-set f]
   (let [filename (.getName f)]
     (if (str/includes? filename "-")
       (add-flip-card-image db base-path lang resolution art-set filename)
       (let [code (first (str/split filename image-select-regex))
             k (str/join "." ["images" (name lang) (name resolution) (name art-set)])
             prev-k-root (if (= :stock art-set) code (name art-set))
             prev-k (str/join "." ["images" (name lang) (name resolution) prev-k-root])
             path (str/join "/" [base-path (name lang) (name resolution) (name art-set) filename])]
         (when-not (some #(= % code) cards-to-skip)
           (mc/update db card-collection {:code code} {$addToSet {k path}})
           (mc/update db card-collection {:previous-versions {$elemMatch {:code code}}} {$addToSet {prev-k path}})))))))

(defn- add-alt-images
  "All all images in the specified alt directory"
  [db base-path lang resolution alt-dir]
  (let [alt (keyword (.getName alt-dir))
        images (find-files alt-dir)]
    (run! #(add-card-image db base-path lang resolution alt %) images)
    (println "Added" (count images) "images to" lang resolution alt)))

(defn- add-resolution-images
  "Add all images in the specified resolution directory"
  [db base-path lang res-dir]
  (let [resolution (keyword (.getName res-dir))
        alts (find-dirs res-dir)
        images (find-files res-dir)]
    (run! #(add-alt-images db base-path lang resolution %) alts)))

(defn- add-language-images
  "Add all images in the specified language directory"
  [db base-path lang-dir]
  (let [lang (keyword (.getName lang-dir))
        resolutions (find-dirs lang-dir)]
    (run! #(add-resolution-images db base-path lang %) resolutions)))

(defn add-images
  "Add alt art, alternate language, and high-res card images to the database"
  ([] (add-images nil))
  ([db?]
   (let [{{:keys [db]} :mongodb/connection :as system}
         (if db? {:mongodb/connection {:db db?}} (connect))]
     (try
       (let [alt-sets (read-alt-sets)
             card-dir (apply io/file img-directory)
             langs (remove #(= "overrides" (.getName %)) (find-dirs card-dir))
             overrides-dir (apply io/file overrides-img-directory)]
         (replace-collection db alt-collection alt-sets)
         (remove-old-images db)
         (run! (partial add-language-images db "/img/cards") langs)
         (println "Adding override images...")
         (doseq [o (find-dirs overrides-dir)
                 :let [overrides-langs (find-dirs o)]]
           (run! (partial add-language-images db (str "/img/cards/overrides/" (.getName o))) overrides-langs)))
       (catch Exception e
         (do (println "Image import failed:" (.getMessage e))
             (.printStackTrace e)))
       (finally (when (not db?) (disconnect system)))))))
