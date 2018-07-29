(ns tasks.split-defs
  "NetrunnerDB import tasks"
  (:require [clojure.string :as string]
            [tasks.nrdb :refer [slugify]]
            [clojure.java.io :as io]
            [jinteki.cards :refer [all-cards]]))

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

(defn deep-merge [v & vs]
  ;; Pulled from https://gist.github.com/danielpcox/c70a8aa2c36766200a95#gistcomment-2313926
  (letfn [(rec-merge [v1 v2]
            (if (and (map? v1) (map? v2))
              (merge-with deep-merge v1 v2)
              v2))]
    (if (some identity vs)
      (reduce #(rec-merge %1 %2) v vs)
      v)))

(defn open-base-defs []
  (->> (io/file "src/clj/game/cards")
       .listFiles
       (filter #(clojure.string/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))

(defn extract-defs []
  (let [header (string/join
                 "\r\n"
                 ["(in-ns 'game.cards.%s)"
                  ""
                  "(def card-definition-%s"
                  "  {"])
        footer "})\r\n"
        base-defs (->> (open-base-defs)
                       (map #(string/split % #";; Card definitions\r\n\(def card-definitions\r\n  \{")))
        prelude (->> base-defs
                     (map first)
                     (map string/trim))
        defs (->> base-defs
                  (map second)
                  (map string/trim)
                  (map #(string/join "" (drop-last 2 %)))
                  (map #(string/split % #"\r\n\r\n"))
                  (map #(filter (fn [x] (.startsWith x "   \"")) %))
                  flatten
                  (map string/split-lines))]
    (doseq [card prelude
            :let [title (-> card string/split-lines first (string/split #"\.") last)
                  filename (str "src/clj/game/cards/" title ".clj")]]
      (spit filename (str card)))
    (doseq [card defs
            :let [title (-> card first string/trim read-string)
                  card (rest card)
                  all-card (@all-cards title)
                  card-type (type->dir all-card)
                  filename (str "src/clj/game/cards/"
                                card-type "/"
                                (slugify title "_") ".clj")]]
      (io/make-parents filename)
      (spit filename
            (str (format header card-type (slugify title))
                 (pr-str (:title (@all-cards title))) "\r\n"
                 (string/join "\r\n" card)
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
  (let [header (string/join
                 "\r\n"
                 ["\r\n;; Card definitions"
                  "(def card-definitions"
                  "  {"])
        footer "})\r\n"
        defs (for [card (->> (open-individual-defs)
                             (map string/split-lines)
                             (map #(drop 3 %)))
                   :let [title (-> card first (string/split #"\{") second string/trim read-string)
                         end (->> card last (drop-last 2) (string/join ""))
                         card (->> card rest butlast (string/join "\r\n"))
                         card (->> [(pr-str title) card end]
                                   (remove string/blank?)
                                   (string/join "\r\n"))
                         all-card (@all-cards title)
                         card-type (type->dir all-card)
                         filename (str "src/clj/game/cards/" card-type ".clj")]]
               {filename {title card}})
        card-defs (apply deep-merge defs)]
    (doseq [[filename cards] (sort-by key card-defs)]
      (spit filename
            (str header
                 (string/join "\r\n\r\n   " (vals (sort-by #(string/lower-case (key %)) cards)))
                 footer)
            :append true))))
