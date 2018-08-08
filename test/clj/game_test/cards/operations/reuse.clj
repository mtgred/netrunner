(ns game-test.cards.operations.reuse
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest reuse
  ;; Reuse - Gain 2 credits for each card trashed from HQ
  (do-game
    (new-game {:corp {:deck [(qty "Reuse" 2) "Hive" "IQ"
                             "Ice Wall"]}})
    (play-from-hand state :corp "Reuse")
    (click-card state :corp (find-card "Ice Wall" (:hand (get-corp))))
    (click-card state :corp (find-card "Hive" (:hand (get-corp))))
    (click-card state :corp (find-card "IQ" (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (is (= 4 (count (:discard (get-corp)))) "3 cards trashed plus operation played")
    (is (= 11 (:credit (get-corp))) "Gained 6 credits")
    (is (= 1 (:click (get-corp))) "Spent 2 clicks")))
