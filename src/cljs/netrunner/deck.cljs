(ns netrunner.deck
  (:require [clojure.string :refer [split split-lines join]]
            [netrunner.cardbrowser :as cb]))

(declare levenshtein)

(defn levenshtein-distance [a b]
  (cond
    (empty? a) (count b)
    (empty? b) (count a)
    :else (min
           (+ (if (= (first a) (first b)) 0 1) (levenshtein (rest a) (rest b)))
           (inc (levenshtein (rest a) b))
           (inc (levenshtein seq1 (rest a))))))

(def levenshtein (memoize levenshtein-distance))

(defn fuzzy-search [query]
  (loop [card nil
         min-dist 1000
         cards (:cards @cb/app-state)]
    (if (empty? cards)
      card
      (let [c (first cards)
            dist (levenshtein query (:title c))]
        (if (< dist min-dist)
          (recur c dist (rest cards))
          (recur card min-dist (rest cards)))))))

(defn lookup [query]
  (let [cards (:cards @cb/app-state)]
    (if-let [card (some #(when (= (-> % :title .toLowerCase) (.toLowerCase query)) %) cards)]
      card
      query
      ;; (loop [matches cards]
      ;;   (if (= (count matches) 1)
      ;;     ))
      )))

(defn parse-line [line]
  (let [tokens (split line " ")
        qty (js/parseInt (first tokens))
        cardname (join " " (rest tokens))]
    (when-not (js/isNaN qty)
      (println tokens)
      {:qty qty :card (lookup cardname)})))

(defn parse-deck [deck]
  (reduce #(if-let [card (parse-line %2)] (conj %1 card) %1) [] (split-lines deck)))

(defn check-deck [deck])

