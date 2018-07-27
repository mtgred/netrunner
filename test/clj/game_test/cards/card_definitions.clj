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
  (doseq [[title card] @all-cards
          :let [card (@card-definitions title)
                abilities (:abilities card)
                subroutines (:subroutines card)
                runner-abilities (:runner-abilities card)
                corp-abilities (:corp-abilities card)]]
    ; (testing "Abilities need labels"
    ;   (when abilities
    ;     (doseq [ab abilities]
    ;       (is (make-label ab) (str title "'s ability needs a label"))))
    ;   (when subroutines
    ;     (doseq [sub subroutines]
    ;       (is (make-label sub) (str title "'s subroutine needs a label"))))
    ;   (when runner-abilities
    ;     (doseq [ab runner-abilities]
    ;       (is (make-label ab) (str title "'s runner-ability needs a label"))))
    ;   (when corp-abilities
    ;     (doseq [ab corp-abilities]
    ;       (is (make-label ab) (str title "'s corp-ability needs a label"))))
    ;   )
    ))
