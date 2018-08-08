(ns game-test.cards.hardware.zamba
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest zamba
  ;; Zamba - Whenever corp card is exposed you may gain 1 credit
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Zamba" (qty "Infiltration" 2)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Zamba")
    (is (= 6 (core/available-mu state)) "Gain 2 memory")
    (is (= 1 (:credit (get-runner))) "At 1 credit")
    (play-from-hand state :runner "Infiltration")
    (click-prompt state :runner "Expose a card")
    (click-card state :runner (get-ice state :archives 0))
    (is (= 2 (:credit (get-runner))) "Gained 1 credit from exposing")
    (play-from-hand state :runner "Infiltration")
    (click-prompt state :runner "Expose a card")
    (click-card state :runner (get-ice state :archives 0))
    (is (= 3 (:credit (get-runner))) "Gained 1 more credit from exposing")))
