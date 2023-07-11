(ns tasks.nrdb
  "NetrunnerDB import tasks"
  (:require
    ;; external
    [org.httpkit.client :as http]
    [org.httpkit.sni-client :as sni-client]
    [monger.collection :as mc]
    [monger.operators :refer [$exists $inc $currentDate]]
    [throttler.core :refer [throttle-fn]]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    ;; internal
    [tasks.utils :refer [replace-collection]]
    [tasks.images :refer [add-images]]
    [tasks.index :refer [create-indexes]]
    [tasks.setup :refer [connect disconnect]]))

(def ^:const edn-base-url "https://raw.githubusercontent.com/NoahTheDuke/netrunner-data/master/edn/raw_data.edn")
(def ^:const jnet-image-url-base "https://static.nrdbassets.com/v1/")
(def ^:const supported-img-langs #{"en" "ja"})

(defn download-edn-data
  [localpath]
  (if localpath
    (-> (str localpath "/edn/raw_data.edn")
        (slurp)
        (edn/read-string))
    (let [{:keys [status body error]} @(http/get edn-base-url)]
      (cond
        error (throw (Exception. (str "Failed to download file " error)))
        (= 200 status) (edn/read-string body)
        :else (throw (Exception. (str "Failed to download file, status " status)))))))

(defn write-to-file
  [filename data]
  (io/make-parents filename)
  (spit filename data))

(defn- card-image-file
  "Returns the path to a card's image as a File"
  [code lang]
  (io/file "resources" "public" "img" "cards" (or lang "en") "default" "stock" (str code ".png")))

(defn- jnet-image-url
  [code lang]
  (let [lang-path (if (and lang (not= lang "en")) (str lang "/"))]
    (str jnet-image-url-base lang-path "large/" code ".jpg")))

(defn- download-card-image
  "Download a single card image from NRDB"
  [lang {:keys [code title]}]
  (binding [org.httpkit.client/*default-client* sni-client/default-client]
    (let [url (jnet-image-url code lang)]
      (println "Downloading: " title "\t\t(" url ")")
      (http/get url {:as :byte-array :timeout 120000}
                (fn [{:keys [status body error]}]
                  (case status
                    404 (println "No image for card" code title)
                    200 (let [card-path (.getPath (card-image-file code lang))]
                          (io/make-parents card-path)
                          (with-open [w (io/output-stream card-path)]
                            (.write w body)))
                    (println "Error downloading art for card" code (or lang "default") error)))))))

(def download-card-image-throttled
  (throttle-fn download-card-image 5 :second))

(defn- expand-card
  "Make a card stub for all previous versions specified in a card."
  [acc card]
  (reduce #(conj %1 {:title (:title card) :code (:code %2)}) acc (:previous-versions card)))

(defn- generate-previous-card-stubs
  "The cards database only has the latest version of a card. Create stubs for previous versions of a card."
  [cards]
  (let [c (filter #(contains? % :previous-versions) cards)]
    (reduce expand-card `() c)))

;; these are cards with multiple faces, so we can't download them directly
(def ^:const cards-to-skip #{"08012" "09001" "26066" "26120"})
;; starting code for NSG cards in other languages since NRDB doesn't host non-English FFG cards
(def ^:const translated-img-code-start 26001)

(defn download-card-images
  "Download card images (if necessary) from NRDB"
  [cards lang]
  (let [img-dir (io/file "resources" "public" "img" "cards" lang "default" "stock")]
    (io/make-parents img-dir)
    (let [previous-cards (generate-previous-card-stubs cards)
          total-cards (concat cards previous-cards)
          total-cards (remove #(get cards-to-skip (:code %)) total-cards)
          total-cards (remove #(and (not= lang "en") (>= translated-img-code-start (Integer/parseInt (:code %)))) total-cards)
          missing-cards (remove #(.exists (card-image-file (:code %) lang)) total-cards)
          total (count total-cards)
          missing (count missing-cards)]
      (if (pos? missing)
        (do
          (println "Have art for" (str (- total missing) "/" total) "cards for" lang "lang. Downloading" missing "missing images...")
          (let [futures (doall (map (partial download-card-image-throttled lang) missing-cards))]
            (doseq [resp futures]
              ; wait for all the GETs to complete
              (:status @resp)))
          (println "Finished downloading card art"))
        (println "All" total "card images exist for" lang "lang, skipping download")))))

(defn- update-config
  "Store import meta info in the db"
  [db]
  (mc/update db "config"
             {:cards-version {$exists true}}
             {$inc {:cards-version 1}
              $currentDate {:last-updated true}}
             {:upsert true}))

(defn fetch-data
  [{:keys [card-images db local]}]
  (let [edn (dissoc (download-edn-data local) :promos)]
    (doseq [[k data] edn
            :let [filename (str "data/" (name k) ".edn")]]
      (write-to-file filename data)
      (println (str "Wrote " filename " to disk")))
    (when db
      (let [{{:keys [db]} :mongodb/connection :as system} (connect)]
        (try
          (doseq [[k data] edn
                  :let [col (name k)]]
            (replace-collection db col data)
            (println (str "Imported " col " into database")))
          (update-config db)
          (when card-images
            (doall (map (partial download-card-images (:cards edn)) supported-img-langs)))
          (add-images db)
          (create-indexes db)
          (finally (disconnect system)))))
    (println (count (:cycles edn)) "cycles imported")
    (println (count (:sets edn)) "sets imported")
    (println (count (:mwls edn)) "MWL versions imported")
    (println (count (:cards edn)) "cards imported")))
