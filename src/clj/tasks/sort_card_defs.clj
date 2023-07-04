(ns tasks.sort-card-defs
  (:require [clojure.string :as str]
            [tasks.utils :refer [type->dir]]
            [clojure.java.io :as io]
            [jinteki.utils :refer [slugify]]
            [jinteki.cards :refer [all-cards]]))

(defn open-base-defs []
  (->> (io/file "src" "clj" "game" "cards")
       (#(.listFiles %))
       (filter #(str/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))

(def file-strings (open-base-defs))

(def types ["agendas" "assets" "basic" "events" "hardware" "ice" "identities" "operations" "programs" "resources" "upgrades"])

(defn split-em []
  (doseq [[idx t] (map-indexed (fn [idx itm] [idx itm]) types)
          :let [f (nth file-strings idx)
                header (-> f
                           (str/split #";; Card definitions\n\n")
                           (first)
                           (str ";; Card definitions\n\n"))
                card-defs (-> f
                              (str/split #";; Card definitions")
                              (second)
                              (str/split #"\n\n\(defcard ")
                              (->> (filterv not-empty)
                                   (mapv #(str "(defcard " %))
                                   (sort-by #(slugify (last (re-find #"defcard \"(.+)\"" %)) " "))
                                   (str/join "\n\n")))]]

    (spit (str "src/clj/game/cards/" t ".clj") (str header card-defs))))

(comment
  (split-em))
