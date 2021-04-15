(ns tasks.sort-card-tests
  (:require [clojure.string :as str]
            [tasks.utils :refer [type->dir]]
            [clojure.java.io :as io]
            [jinteki.utils :refer [slugify]]
            [jinteki.cards :refer [all-cards]]))

(defn open-base-tests []
  (->> (io/file "test" "clj" "game" "cards")
       (#(.listFiles %))
       (filter #(str/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))

(def file-strings (open-base-tests))

(def types
  ["agendas" "assets" "basic" "events" "hardware" "ice" "identities" "operations" "programs" "resources" "upgrades"])

(defn split-em []
  (doseq [[idx t] (map-indexed (fn [idx itm] [idx itm]) types)
            :let [f (nth file-strings idx)
                  header (-> f
                             (str/split #"\n\n")
                             (first)
                             (str "\n\n"))
                  tests (-> f
                            (str/split #"\n\n")
                            (next)
                            (->> (str/join "\n\n")
                                 (str "\n\n"))
                            (str/split #"\n\n\(deftest ")
                            (->> (filterv not-empty)
                                 (mapv #(str "(deftest " %))
                                 (sort-by #(let [normal (re-find #"deftest ([a-z\-]+)" %)
                                                 meta-data (re-find #"deftest \^\{:card-title \"([0-9a-z-]+)\"" %)]
                                             (or (last normal) (last meta-data))))
                                 (str/join "\n\n"))
                            (str))]]
    (spit (str "test/clj/game/cards/" t "_test.clj") (str header tests))))

(comment
  (split-em)
  )
