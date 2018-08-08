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
