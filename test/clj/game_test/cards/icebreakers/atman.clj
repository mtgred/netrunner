(ns game-test.cards.icebreakers.atman
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest atman
  ;; Atman
  (testing "Installing with 0 power counters"
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "0")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (zero? (get-counters atman :power)) "0 power counters")
        (is (zero? (:current-strength atman)) "0 current strength"))))
  (testing "Installing with 2 power counters"
    (do-game
      (new-game {:runner {:deck ["Atman"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Atman")
      (click-prompt state :runner "2")
      (is (= 3 (core/available-mu state)))
      (let [atman (get-program state 0)]
        (is (= 2 (get-counters atman :power)) "2 power counters")
        (is (= 2 (:current-strength atman)) "2 current strength")))))
