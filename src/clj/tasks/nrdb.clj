(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require [org.httpkit.client :as http]
            [web.db :as webdb]
            [monger.collection :as mc]
            [monger.operators :refer [$exists $inc $currentDate]]
            [throttler.core :refer [throttle-fn]]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def ^:const base-url "https://raw.githubusercontent.com/NoahTheDuke/netrunner-cards-edn/master/edn/raw_data.edn")
(def ^:const cgdb-image-url "https://www.cardgamedb.com/forums/uploads/an/")
(def ^:const nrdb-image-url "https://netrunnerdb.com/card_image/")

(defn download-edn-data
  [localpath]
  (if localpath
    ((comp edn/read-string slurp) (str localpath "/edn/raw_data.edn"))
    (let [{:keys [status body error] :as resp} @(http/get base-url)]
      (cond
        error (throw (Exception. (str "Failed to download file " error)))
        (= 200 status) (edn/read-string body)
        :else (throw (Exception. (str "Failed to download file, status " status)))))))

(defn write-to-file
  [filename data]
  (io/make-parents filename)
  (spit filename data))

(defn replace-collection
  [col data]
  (mc/remove webdb/db col)
  (mc/insert-batch webdb/db col data))

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
          total (count cards)
          missing (count missing-cards)]
      (if (pos? missing)
        (do
          (println "Have art for" (str (- total missing) "/" total) "cards. Downloading" missing "missing images...")
          (let [futures (doall (map download-card-image-throttled missing-cards))]
            (doseq [resp futures]
              ; wait for all the GETs to complete
              (:status @resp)))
          (println "Finished downloading card art"))
        (println "All" total "card images exist, skipping download")))))

(defn update-config
  "Store import meta info in the db"
  []
  (mc/update webdb/db "config"
             {:cards-version {$exists true}}
             {$inc {:cards-version 1}
              $currentDate {:last-updated true}}
             {:upsert true}))

(defn fetch-data
  [{:keys [card-images db local]}]
  (let [edn (download-edn-data local)]
    (doseq [[k data] edn
            :let [filename (str "data/" (name k) ".edn")]]
      (write-to-file filename data)
      (println (str "Wrote data/" filename ".edn to disk")))
    (when db
      (webdb/connect)
      (try
        (doseq [[k data] edn
                :let [col (name k)]]
          (replace-collection col data)
          (println (str "Imported " col " into database")))
        (update-config)
        (finally (webdb/disconnect))))
    (println (count (:cycles edn)) "cycles imported")
    (println (count (:sets edn)) "sets imported")
    (println (count (:mwls edn)) "MWL versions imported")
    (println (count (:cards edn)) "cards imported")
    (when card-images
      (download-card-images (:cards edn)))))
