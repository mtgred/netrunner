(ns game-test.cards.operations.targeted-marketing
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest targeted-marketing
  ;; Targeted Marketing
  (do-game
    (new-game {:corp {:deck ["Targeted Marketing"]}})
    (play-from-hand state :corp "Targeted Marketing")
    (click-prompt state :corp "Sure Gamble")
    (take-credits state :corp)
    (let [credits (:credit (get-corp))]
      (play-from-hand state :runner "Sure Gamble")
      (is (= (+ 10 credits) (:credit (get-corp))) "Corp gains 10 credits from Runner playing named card"))))
