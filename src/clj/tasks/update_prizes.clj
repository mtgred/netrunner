(ns tasks.update-prizes
  "import prize data"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [org.httpkit.client :as http]
   [clojure.tools.cli :refer [parse-opts]]))

(def ^:const edn-base-url
  "https://raw.githubusercontent.com/NBKelly/netrunner-prizes/main/")
(def ^:const base-card-back-url
  "https://raw.githubusercontent.com/NBKelly/netrunner-prizes/main/img/card-backs/")
(def ^:const base-card-back-path
  "resources/public/img/card-backs/")

(defn download-prize-data
  [localpath listing]
  (if localpath
    (-> (str localpath listing)
        (slurp)
        (edn/read-string))
    (let [{:keys [status body error]} @(http/get (str edn-base-url listing))]
      (cond
        error (throw (Exception. (str "Failed to download file " error)))
        (= 200 status) (edn/read-string body)
        :else (throw (Exception. (str "Failed to download file, status " status)))))))

(defn write-to-file
  [filename data]
  (io/make-parents filename)
  (spit filename data))

(defn fetch-prizes [{:keys [card-images local]}]
  (if-let [card-back-data (download-prize-data local "/data/card-backs.edn")]
    (do (write-to-file "data/card-backs.edn" card-back-data)
        (when card-images
          (doseq [[_ {title :name side :side file :file}] card-back-data]
            ;;(binding [org.httpkit.client/*default-client* sni-client/default-client]
            (let [ext (str (name side) "/" file ".png")
                  url (str base-card-back-url ext)]
              (println "Downloading: " title "\t\t(" url ")")
              (let [{:keys [status body error]} @(http/get url {:as :byte-array :timeout 120000})]
                (case status
                  404 (println "No image for card-back: " title "\t\t(" url ")")
                  200 (let [path (str base-card-back-path ext)
                            f (io/file (str base-card-back-path ext))
                            card-path (.getPath f)]
                        (io/make-parents card-path)
                        (with-open [w (io/output-stream card-path)]
                          (.write w body)))
                  (println "Error downloading art for card-back: " title error))))))
        (println "done!"))
    (println "Unable to fetch card-back prize data")))

(defn usage
  [options-summary]
  (->> [""
        "Usage: lein update-prizes [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(def cli-options
  [["-l" "--local PATH" "Path to fetch card edn from"
    :validate [#(.exists (io/file (str % "/data/card-backs.edn")))
               "Could not find local data file"]]
   ["-i" "--card-images" "Fetch card images from the prizes repo (default)"
    :id :card-images
    :default true]
   ["-j" "--no-card-images" "Do not fetch card images from the prizes repo"
    :id :card-images
    :parse-fn not]])

(defn exit [status msg]
  (binding [*out* *err*]
    (println msg))
  (System/exit status))

(defn command
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or errors
            (not-empty arguments))
      (exit 1 (str/join \newline (conj errors (usage summary))))
      (fetch-prizes options))))
