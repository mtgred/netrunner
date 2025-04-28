(ns tasks.update-alt-arts
  "import alt-art cards from a private repo"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.data.json :as json]
   [org.httpkit.client :as http]
   [com.climate.claypoole :as cp]
   [clojure.tools.cli :refer [parse-opts]])
  (:import [java.security MessageDigest]
           [java.io FileInputStream]))

(def ^:const owner "NBKelly")
(def ^:const repo "netrunner-alt-art-collection")
(def ^:const branch "master")

(defn sha1-checksum [file-path]
  (let [digest (MessageDigest/getInstance "SHA-1")
        file (java.io.File. file-path)
        byte-count (.length (io/file file-path))
        blob-header (str "blob " byte-count "\0")]
    ;; note that github uses a blob header for it's sha sums for files
    ;; so we need to manually insert that
    ;; the header is: 'blob <size-in-bytes>\0'
    (.update digest (.getBytes blob-header))
    (with-open [in-stream (FileInputStream. file)]
      (let [buffer (byte-array 1024)]
        (loop [n (.read in-stream buffer)]
          (when (pos? n)
            (.update digest buffer 0 n)
            (recur (.read in-stream buffer))))))
    (format "%040x" (BigInteger. 1 (.digest digest)))))

(defn list-content [token key high]
  (let [url (str "https://api.github.com/repos/"
                 owner "/" repo "/contents/images/"
                 (if high "high" "default") "/" key
                 "?ref=" branch)
        _ (println (str "fetching cards: " key " (" (if high "high-res)" "standard-res)")))
        headers {"Authorization" (str "Bearer " token)}
        {:keys [status body error]} @(http/get url {:headers headers})]
    (if (= 200 status)
      (json/read-str body :key-fn keyword)
      (println "Error fetching files for url:" url "\n  -- " status body))))

(defn write-to-file
  [filename data]
  (io/make-parents filename)
  (with-open [in-stream data
              out-stream (io/output-stream filename)]
    (io/copy in-stream out-stream)))

(defn download-github-file [token filepath output-path]
  (let [url (str "https://api.github.com/repos/"
                 owner "/" repo "/contents/" filepath
                 "?ref=" branch)
        headers {"Authorization" (str "Bearer " token)
                 "Accept" "application/vnd.github.v3.raw"}
        {:keys [status body error]} @(http/get url {:headers headers :as :stream})]
    (cond
      (= status 200)
      (do
        (write-to-file output-path body)
        (println "Downloaded " output-path)
        true)

      (= status 404)
      (println "Error: File not found (404)")
      error
      (println "Error: Exception during HTTP request" error)
      :else
      (println "Error: HTTP" status))))

(defn local-image-path [key high name]
  (str "resources/public/img/cards/en/" (if high "high" "default") "/" key "/" name))

(defn fetch-image [token key high data force]
  (let [local-path (local-image-path key high (:name data))]
    (if (and (not force) (.exists (io/file local-path)))
      (let [local-sha (sha1-checksum local-path)
            remote-sha (:sha data)]
        (when-not (= (str local-sha) (str remote-sha))
          (download-github-file token (:path data) local-path)))
      (download-github-file token (:path data) local-path))))

(defn- get-promo-paths
  []
  (->> "data/promos.edn" slurp edn/read-string (mapv :version)))

(defn update-promos
  [{:keys [token force]}]
  (println "updating promo cards...")
  (let [token (str/trim (slurp token))]
    (if (download-github-file token "promos.edn" "data/promos.edn")
      (doseq [path (get-promo-paths)]
        ;; get standard res images
        (when-let [content (list-content token path nil)]
          (cp/pmap 3 #(fetch-image token path nil % force) content))
        (when-let [content (list-content token path :high-res)]
          (cp/pmap 3 #(fetch-image token path :high-res % force) content)))
      (println "Unable to read promo data from remote source - is your key accurate?"))))

(defn usage
  [options-summary]
  (->> [""
        "Usage: lein update-prizes [options]"
        ""
        "Options:"
        options-summary]
       (str/join \newline)))

(def cli-options
  [["-f" "--force" "Force refetch all files"
    :id :force]
   ["-t" "--token PATH" "Path to fetch token from"
    :id :token
    :validate [#(.exists (io/file %))
               "Could not find token file"]]])

(defn exit [status msg]
  (binding [*out* *err*]
    (println msg))
  (System/exit status))

(defn command
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or errors
            (not-empty arguments)
            (nil? (:token options)))
      (exit 1 (str/join \newline (conj errors (usage summary))))
      (update-promos options))))
