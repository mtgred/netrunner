(ns game-test.cards.ice.resistor
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest resistor
  ;; Resistor - Strength equal to Runner tags, lose strength when Runner removes a tag
  (do-game
    (new-game {:corp {:deck ["Resistor"]}})
    (play-from-hand state :corp "Resistor" "HQ")
    (let [resistor (get-ice state :hq 0)]
      (core/rez state :corp resistor)
      (is (zero? (:current-strength (refresh resistor))) "No Runner tags; 0 strength")
      (core/gain-tags state :runner 2)
      (is (= 2 (:tag (get-runner))))
      (is (= 2 (:current-strength (refresh resistor))) "2 Runner tags; 2 strength")
      (take-credits state :corp)
      (core/remove-tag state :runner 1)
      (is (= 1 (:current-strength (refresh resistor))) "Runner removed 1 tag; down to 1 strength"))))
