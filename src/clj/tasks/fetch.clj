(ns tasks.fetch
  "NetrunnerDB import tasks"
  (:require [clojure.java.io :as io]
            [tasks.nrdb :refer [fetch-data]]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]))

(defn usage
  [options-summary]
  (->> ["Usage: lein fetch [options]"
        ""
        "Options:"
        options-summary]
       (string/join \newline)))

(def cli-options
  [["-l" "--local PATH" "Path to fetch card edn from"
    :validate [#(-> %
                    (str "/edn/raw_data.edn")
                    io/file
                    .exists)
               "Could not find local data file"]]
   ["-d" "--db" "Load card data into the database (default)"
    :id :db
    :default true]
   ["-n" "--no-db" "Do not load edn data into the database"
    :id :db
    :parse-fn not]
   ["-i" "--card-images" "Fetch card images from NetrunnerDB (default)"
    :id :card-images
    :default true]
   ["-j" "--no-card-images" "Do not fetch card images from NetrunnerDB"
    :id :card-images
    :parse-fn not]
   ])

(defn exit [status msg]
  (binding [*out* *err*]
    (println msg))
  (System/exit status))

(defn command
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (if (or errors
            arguments)
      (exit 1 (string/join \newline (conj errors "" (usage summary))))
      (fetch-data options))))
