(ns tasks.split-tests
  (:require [clojure.string :as string]
            [tasks.utils :refer [type->dir deep-merge]]
            [clojure.java.io :as io]
            [jinteki.utils :refer [slugify]]
            [jinteki.cards :refer [all-cards]]))

(defn open-base-tests []
  (->> (io/file "test/clj/game/cards")
       .listFiles
       (filter #(string/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))


(defn extract-tests []
  (let [header (string/join
                 "\n"
                 ["(ns game-test.cards.%s.%s"
                  "  (:require [game.core :as core]"
                  "            [game.core.card :refer :all]"
                  "            [game.utils :as utils]"
                  "            [game.core-test :refer :all]"
                  "            [game.utils-test :refer :all]"
                  "            [game.macros-test :refer :all]"
                  "            [clojure.test :refer :all]))"
                  "\n"])
        footer "\n"
        base-tests (->> (open-base-tests)
                        (map #(string/split % #"\n\n")))
        tests (->> base-tests
                   (map rest)
                   flatten
                   (map string/split-lines))]
    (doseq [card tests
            :let [title (-> card first
                            (string/split #"\"}") last
                            (string/split #"\"") last
                            (string/split #" ") last)
                  all-card (some #(when (= title (:normalizedtitle %)) %) (vals @all-cards))
                  card-type (type->dir all-card)
                  filename (str "test/clj/game/cards/"
                                card-type "/"
                                (slugify title "_") "_test.clj")]]
      (io/make-parents filename)
      (spit filename
            (str (format header card-type title)
                 (string/join "\n" card)
                 footer)))))

(defn- open-individual-tests []
  (->> (io/file "test/clj/game/cards/")
       .listFiles
       (remove #(.isFile %))
       (mapcat file-seq)
       (filter #(.isFile %))
       (map slurp)))

(defn merge-tests []
  (let [header (string/join
                 "\n"
                 ["(ns game-test.cards.%s"
                  "  (:require [game.core :as core]"
                  "            [game.core.card :refer :all]"
                  "            [game.utils :as utils]"
                  "            [game.core-test :refer :all]"
                  "            [game.utils-test :refer :all]"
                  "            [game.macros-test :refer :all]"
                  "            [clojure.test :refer :all]))"
                  "\n"])
        footer "\n"
        normalized->title (->> @all-cards
                               (map #(select-keys (second %) [:normalizedtitle :title]))
                               (map vals)
                               (map vec)
                               (into {}))
        tests (for [card (->> (open-individual-tests)
                              (map #(string/split % #"\n\n"))
                              (mapcat rest)
                              (map string/split-lines))
                    :let [title (-> card first
                                    (string/split #"\"}") last
                                    (string/split #"\"") last
                                    (string/split #" ") last)
                          card (->> card
                                    (remove string/blank?)
                                    (string/join "\n"))
                          all-card (@all-cards (normalized->title title))
                          card-type (type->dir all-card)]]
                {card-type {title card}})
        card-tests (apply deep-merge tests)]
    (doseq [[card-type cards] (sort-by key card-tests)]
      (spit (str "test/clj/game/cards/" card-type "_test.clj")
            (str (format header card-type)
                 (string/join "\n\n" (vals (sort-by #(slugify (key %)) cards)))
                 footer)))))
