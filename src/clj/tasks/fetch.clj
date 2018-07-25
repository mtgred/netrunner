(ns tasks.fetch
  "NetrunnerDB import tasks"
  (:require [web.db :refer [db] :as webdb]
            [game.utils :refer [pluralize]]
            [clojure.string :as string]
            [tasks.nrdb :refer :all]
            [clojure.java.io :as io]
            [jinteki.cards :refer [all-cards]]
            [tasks.altart :refer [add-art]]))

(defn fetch
  "Import data from NetrunnerDB.
  Can accept `--local <path>` to use the `netrunner-card-json` project locally,
  otherwise pulls data from NRDB.
  Specifying `--no-card-images` will not attempt to download images for cards."
  [& args]
  (webdb/connect)
  (try
    (let [use-local (some #{"--local"} args)
          localpath (first (remove #(string/starts-with? % "--") args))
          download-fn (if use-local
                        (partial read-local-data localpath)
                        download-nrdb-data)
          cycles (fetch-data download-fn (:cycle tables))
          mwls (fetch-data download-fn (:mwl tables))
          sets (fetch-data download-fn (:set tables) (partial add-set-fields cycles))
          card-download-fn (if use-local
                             (partial read-card-dir localpath)
                             download-nrdb-data)
          cards (fetch-cards card-download-fn (:card tables) sets (not (some #{"--no-card-images"} args)))]
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

(defn open-defs []
  (->> (io/file "src/clj/game/cards")
       file-seq
       (filter #(.isFile %))
       (filter #(clojure.string/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))

(defn type->dir
  [card]
  (case (:type card)
    "Agenda" "agendas"
    "Asset" "assets"
    "Event" "events"
    "Hardware" "hardware"
    "ICE" "ice"
    "Identity" "identities"
    "Operation" "operations"
    "Program" (if (and (:subtype card)
                       (> (.indexOf (:subtype card) "Icebreaker") -1))
                "icebreakers"
                "programs")
    "Resource" "resources"
    "Upgrade" "upgrades"))

(defn write-defs []
  (let [header (string/join
                 "\r\n"
                 ["(in-ns 'game.cards.%s)"
                  ""
                  "(def card-definition-%s"
                  "  {"])
        defs (->> (open-defs)
                  (map #(string/split % #"\r\n\r\n"))
                  (map #(filter (fn [x] (.startsWith x "   \"")) %))
                  flatten
                  (map string/split-lines))
        cards (->> @all-cards
                   (map (fn [[k v]] {(.replace k "'" "") v}))
                   (apply merge))]
    (doseq [card defs
          :let [title (-> card first string/trim read-string)
                card (rest card)
                all-card (cards title)
                card-type (type->dir all-card)
                filename (str "src/clj/game/cards/"
                              card-type "/"
                              (slugify title "_") ".clj")]]
      (io/make-parents filename)
      (println filename)
      (spit filename
            (str (format header card-type (slugify title))
                 (pr-str (:title (cards title))) "\r\n"
                 (string/join "\r\n" card)
                 "})\r\n")))))
