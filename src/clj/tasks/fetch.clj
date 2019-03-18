(ns tasks.fetch
  "NetrunnerDB import tasks"
  (:require [web.db :refer [db] :as webdb]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [tasks.nrdb :refer :all]
            [tasks.altart :refer [add-art]]))

(defn fetch-with-db
  "Import data from NRDB. Assumes the database is already open. See `fetch` for arguments."
  [& args]
  (zp/set-options!
    {:style :community
     :map {:comma? true
           :force-nl? true}
     :width 1000})

  (let [localpath (first (remove #(string/starts-with? % "--") args))
        download-images (not (some #{"--no-card-images"} args))
        data (fetch-data localpath download-images)]
    (println (count (:cycles data)) "cycles imported")
    (println (count (:sets data)) "sets imported")
    (println (count (:mwls data)) "MWL versions imported")
    (println (count (:cards data)) "cards imported")
    (add-art false)
    (update-config)))

(defn fetch
  "Import data from NetrunnerDB.
  Can accept `--local <path>` to use the `netrunner-card-json` project locally,
  otherwise pulls data from NRDB.
  Specifying `--no-card-images` will not attempt to download images for cards."
  [& args]

  (webdb/connect)
  (try
    (apply fetch-with-db args)
    (catch Exception e (do
                         (println "Import data failed:" (.getMessage e))
                         (.printStackTrace e)))
    (finally (webdb/disconnect))))

