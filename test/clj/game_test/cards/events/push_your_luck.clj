(ns game-test.cards.events.push-your-luck
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest push-your-luck
  ;; Push Your Luck
  (testing "Corp guesses correctly"
    (do-game
      (new-game {:runner {:deck ["Push Your Luck"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Push Your Luck")
      (click-prompt state :corp "Odd")
      (click-prompt state :runner "3")
      (is (zero? (:credit (get-runner))) "Corp guessed correctly")))
  (testing "Corp guesses incorrectly"
    (do-game
      (new-game {:runner {:deck ["Push Your Luck"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Push Your Luck")
      (click-prompt state :corp "Even")
      (click-prompt state :runner "3")
      (is (= 6 (:credit (get-runner))) "Corp guessed incorrectly"))))
