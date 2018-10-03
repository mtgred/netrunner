(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require [org.httpkit.client :as http]
            [web.db :refer [db] :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer [$exists $inc $currentDate]]
            [throttler.core :refer [throttle-fn]]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.pprint :as pprint]))

(def ^:const base-url "https://raw.githubusercontent.com/NoahTheDuke/netrunner-cards-edn/master/edn/raw_data.edn")
(def ^:const cgdb-image-url "https://www.cardgamedb.com/forums/uploads/an/")
(def ^:const nrdb-image-url "https://netrunnerdb.com/card_image/")

(defn download-edn-data
  [localpath]
  (if localpath
    ((comp edn/read-string slurp) (str localpath "edn/raw_data.edn"))
    (let [{:keys [status body error] :as resp} @(http/get base-url)]
      (cond
        error (throw (Exception. (str "Failed to download file " error)))
        (= 200 status) (edn/read-string body)
        :else (throw (Exception. (str "Failed to download file, status " status)))))))

(defn replace-collection
  "Remove existing collection and insert new data"
  [collection data]
  (mc/remove db collection)
  (mc/insert-batch db collection data))

(defn- card-image-file
  "Returns the path to a card's image as a File"
  [card]
  (io/file "resources" "public" "img" "cards" (str (:code card) ".png")))

(defn- download-card-image
  "Download a single card image from NRDB"
  [card]
  (println "Downloading: " (:title card) "\t\t(" (:image_url card) ")")
  (http/get (:image_url card) {:as :byte-array :timeout 120000}
            (fn [{:keys [status body error]}]
              (case status
                404 (println "No image for card" (:code card) (:title card))
                200 (with-open [w (io/output-stream (.getPath (card-image-file card)))]
                      (.write w body))
                (println "Error downloading art for card" (:code card) error)))))

(def download-card-image-throttled
  (throttle-fn download-card-image 5 :second))

(defn download-card-images
  "Download card images (if necessary) from NRDB"
  [cards]
  (let [img-dir (io/file "resources" "public" "img" "cards")]
    (io/make-parents img-dir)
    (let [missing-cards (remove #(.exists (card-image-file %)) cards)
          missing (count missing-cards)]
      (when (pos? missing)
        (println "Downloading art for" missing "cards...")
        (let [futures (doall (map download-card-image-throttled missing-cards))]
          (doseq [resp futures]
            ; wait for all the GETs to complete
            (:status @resp)))
        (println "Finished downloading card art")))))

(defn fetch-data
  "Find the NRDB card edn files and import them."
  [localpath download-images]
  (let [data (download-edn-data localpath)
        cards (:cards data)
        zp-settings {:style :community
                     :map {:comma? false
                           :force-nl? true}
                     :width 1000}]

    (println "Writing cards")
    (doseq [card cards
            :let [path (:normalizedtitle card)
                  filename (str "data/cards/" path ".edn")]]
      (io/make-parents filename)
      (with-open [w (clojure.java.io/writer filename)]
        (binding [*out* w
                  pprint/*print-right-margin* 1000]
          (pprint/write (into (sorted-map) card)))))
    (replace-collection "cards" cards)

    (doseq [[k v] data
            :when (not= "cards" (name k))
            :let [collection (name k)
                  filename (str "data/" collection ".edn")]]
      (println (str "Writing " filename))
      (io/make-parents filename)
      (spit filename (zp/zprint-str v zp-settings))
      (replace-collection collection v))
    (when download-images
      (download-card-images cards))
    data))

(defn update-config
  "Store import meta info in the db"
  []
  (mc/update db "config"
             {:cards-version {$exists true}}
             {$inc {:cards-version 1}
              $currentDate {:last-updated true}}
             {:upsert true}))
