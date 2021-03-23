(ns tasks.split-defs
  (:require [clojure.string :as str]
            [tasks.utils :refer [type->dir deep-merge]]
            [clojure.java.io :as io]
            [jinteki.utils :refer [slugify]]
            [jinteki.cards :refer [all-cards]]))

(defn open-base-defs []
  (->> (.listFiles (io/file "src/clj/game/cards"))
       (filter #(str/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))

(def file-strings (open-base-defs))

(def types
  ["agendas" "assets" "basic" "events" "hardware" "ice" "identities" "operations" "programs" "resources" "upgrades"])

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
                                     (str/join "\n\n")))
                  ]]
      (spit (str "src/clj/game/cards/" t ".clj") (str header card-defs))))

(comment
  (split-em)
  )

(defn extract-defs []
  (let [header (str/join
                 "\n"
                 ["(in-ns 'game.cards.%s)"
                  ""
                  "(def card-definition-%s"
                  "  {"])
        footer "})\n"
        base-defs (->> (open-base-defs)
                       (map #(str/split % #";; Card definitions\n\(def card-definitions\n  \{")))
        prelude (->> base-defs
                     (map first)
                     (map str/trim))
        defs (->> base-defs
                  (map second)
                  (map str/trim)
                  (map #(str/join "" (drop-last 2 %)))
                  (map #(str "   " %))
                  (map #(str/split % #"\n\n"))
                  (map #(filter (fn [x] (str/starts-with? x "   \"")) %))
                  flatten
                  (map str/split-lines))]
    (doseq [card prelude
            :let [title (-> card str/split-lines first (str/split #"\.") last)
                  filename (str "src/clj/game/cards/" title ".clj")]]
      (spit filename (str card)))
    (doseq [card defs
            :let [title (-> card first str/trim read-string)
                  card (rest card)
                  all-card (get @all-cards title)
                  card-type (type->dir all-card)
                  filename (str "src/clj/game/cards/"
                                card-type "/"
                                (slugify title "_") ".clj")]]
      (io/make-parents filename)
      (spit filename
            (str (format header card-type (slugify title))
                 (pr-str (:title all-card)) "\n"
                 (str/join "\n" card)
                 footer)))))

(defn- open-individual-defs []
  (->> (io/file "src/clj/game/cards")
       file-seq
       (remove #(.isFile %))
       (drop 1)
       (mapcat file-seq)
       (filter #(.isFile %))
       (map slurp)))

(defn merge-defs []
  (let [header (str/join
                 "\n"
                 ["\n\n;; Card definitions"
                  "(def card-definitions"
                  "  {"])
        footer "})\n"
        defs (for [card (->> (open-individual-defs)
                             (map str/split-lines)
                             (map #(drop 3 %)))
                   :let [title (-> card first (str/split #"\{") second str/trim read-string)
                         end (->> card last (drop-last 2) (str/join ""))
                         card (->> card rest butlast (str/join "\n"))
                         card (->> [(pr-str title) card end]
                                   (remove str/blank?)
                                   (str/join "\n"))
                         all-card (get @all-cards title)
                         card-type (type->dir all-card)
                         filename (str "src/clj/game/cards/" card-type ".clj")]]
               {filename {title card}})
        card-defs (apply deep-merge defs)]
    (doseq [[filename cards] (sort-by key card-defs)]
      (spit filename
            (str header
                 (str/join "\n\n   " (vals (sort-by #(slugify (key %)) cards)))
                 footer)
            :append true))))
