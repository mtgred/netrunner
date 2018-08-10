(ns tasks.split-tests
  (:require [clojure.string :as string]
            [tasks.utils :refer [slugify type->dir deep-merge]]
            [clojure.java.io :as io]
            [jinteki.cards :refer [all-cards]]))

(defn open-base-tests []
  (->> (io/file "test/clj/game_test/cards")
       .listFiles
       (filter #(string/ends-with? (.getPath %) ".clj"))
       sort
       (map slurp)))

(defn extract-tests []
  (let [header (string/join
                 "\r\n"
                 ["(ns game-test.cards.%s.%s"
                  "  (:require [game.core :as core]"
                  "            [game-test.core :refer :all]"
                  "            [game-test.utils :refer :all]"
                  "            [game-test.macros :refer :all]"
                  "            [clojure.test :refer :all]))"
                  "\r\n"])
        footer "\r\n"
        base-tests (->> (open-base-tests)
                        (map #(string/split % #"\r\n\r\n")))
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
                  filename (str "test/clj/game_test/cards/"
                                card-type "/"
                                (slugify title "_") ".clj")]]
      (io/make-parents filename)
      (spit filename
            (str (format header card-type title)
                 (string/join "\r\n" card)
                 footer)))))

(defn- open-individual-tests []
  (->> (io/file "test/clj/game_test/cards/")
       .listFiles
       (remove #(.isFile %))
       (mapcat file-seq)
       (filter #(.isFile %))
       (map slurp)))

(defn merge-tests []
  (let [header (string/join
                 "\r\n"
                 ["(ns game-test.cards.%s"
                  "  (:require [game.core :as core]"
                  "            [game.utils :as utils]"
                  "            [game-test.core :refer :all]"
                  "            [game-test.utils :refer :all]"
                  "            [game-test.macros :refer :all]"
                  "            [clojure.test :refer :all]))"
                  "\r\n"])
        footer "\r\n"
        normalized->title (->> @all-cards
                               (map #(select-keys (second %) [:normalizedtitle :title]))
                               (map vals)
                               (map vec)
                               (into {}))
        tests (for [card (->> (open-individual-tests)
                              (map #(string/split % #"\r\n\r\n"))
                              (mapcat rest)
                              (map string/split-lines))
                    :let [title (-> card first
                                    (string/split #"\"}") last
                                    (string/split #"\"") last
                                    (string/split #" ") last)
                          card (->> card
                                    (remove string/blank?)
                                    (string/join "\r\n"))
                          all-card (@all-cards (normalized->title title))
                          card-type (type->dir all-card)]]
                {card-type {title card}})
        card-tests (apply deep-merge tests)]
    (doseq [[card-type cards] (sort-by key card-tests)]
      (spit (str "test/clj/game_test/cards/" card-type ".clj")
            (str (format header card-type)
                 (string/join "\r\n\r\n" (vals (sort-by #(string/lower-case (key %)) cards)))
                 footer)))))
