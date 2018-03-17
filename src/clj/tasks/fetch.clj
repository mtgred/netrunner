(ns tasks.fetch
  "NetrunnerDB import tasks"
  (:require [web.db :refer [db] :as webdb]
            [tasks.nrdb :refer :all]
            [tasks.altart :refer [add-art]]))

(defn fetch
  "Import data from NetrunnerDB"
  [& args]
  (webdb/connect)
  (try
    (let [cycles (fetch-data (:cycle tables))
          mwls (fetch-data (:mwl tables))
          sets (fetch-data (:set tables) (partial add-set-fields cycles))
          cards (fetch-cards (:card tables) sets (not (some #{"--no-card-images"} args)))]
      (println (count cycles) "cycles imported")
      (println (count sets) "sets imported")
      (println (count mwls) "MWL versions imported")
      (println (count cards) "cards imported")
      (add-art false)
      (update-config (:config tables)))
    (catch Exception e (do
                         (println "Import data failed:" (.getMessage e))
                         (.printStackTrace e)))
    (finally (webdb/disconnect))))