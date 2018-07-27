(ns game-test.cards.card-definitions
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.cards :refer [all-cards card-definitions]]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs nil))

; (deftest defined-cards-are-real-cards
;   (doseq [[title card] @card-definitions]
;     (is (@all-cards title))))

; (deftest unimplemented-cards-should-still-return-something
;   (doseq [[title card] @all-cards]
;     (is (core/card-def card))))

(defn make-label
  [ab]
  (seq (utils/make-label ab)))

(deftest displayed-abilities-require-lables
  (doseq [[title card] (sort-by #(tasks.fetch/type->dir (val %)) @all-cards)
          :let [card (@card-definitions title)
                abilities (:abilities card)
                subroutines (:subroutines card)
                runner-abilities (:runner-abilities card)
                corp-abilities (:corp-abilities card)]]
    (when abilities
      (doseq [[idx ab] (map-indexed vector abilities)]
        (is (make-label ab) (str title " ability " idx " needs a label"))))
    (when subroutines
      (doseq [[idx sub] (map-indexed vector subroutines)]
        (is (make-label sub) (str title " subroutine " idx " needs a label"))))
    (when runner-abilities
      (doseq [[idx ab] (map-indexed vector runner-abilities)]
        (is (make-label ab) (str title " runner-ability " idx " needs a label"))))
    (when corp-abilities
      (doseq [[idx ab] (map-indexed vector corp-abilities)]
        (is (make-label ab) (str title " corp-ability " idx " needs a label"))))
    ))
