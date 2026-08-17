(ns jinteki.preconstructed-test
  (:require
   [clojure.edn :as edn]
   [clojure.test :refer [deftest is]]
   [jinteki.preconstructed :refer [all-matchups matchup-by-key
                                   gateway-beginner-corp gateway-beginner-runner
                                   gateway-intermediate-corp gateway-intermediate-runner]]))

(def card-titles
  (->> (slurp "data/cards.edn")
       (edn/read-string)
       (map :title)
       (set)))

(def all-decks
  (concat [gateway-beginner-corp gateway-beginner-runner
           gateway-intermediate-corp gateway-intermediate-runner]
          (mapcat #(let [{:keys [corp runner]} (matchup-by-key %)] [corp runner])
                  (sort all-matchups))))

(deftest precon-cards-exist
  (doseq [{:keys [cards] deck-name :name id :identity} all-decks]
    (is (contains? card-titles (:title id))
        (str deck-name ": unknown identity " (:title id)))
    (doseq [{:keys [card]} cards]
      (is (contains? card-titles card)
          (str deck-name ": unknown card " card)))))
