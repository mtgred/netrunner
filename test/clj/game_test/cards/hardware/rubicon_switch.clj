(ns game-test.cards.hardware.rubicon-switch
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rubicon-switch
  ;; Rubicon Switch
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Pachinko"]}
               :runner {:deck ["Rubicon Switch"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Pachinko" "R&D")
    (let [iw (get-ice state :hq 0)
          pach (get-ice state :rd 0)]
      (core/rez state :corp iw)
      (take-credits state :corp)
      (play-from-hand state :runner "Rubicon Switch")
      (core/rez state :corp pach)
      (let [rs (get-hardware state 0)]
        (card-ability state :runner rs 0)
        (click-prompt state :runner "1")
        (click-card state :runner "Ice Wall")
        (is (:rezzed (refresh iw)) "Ice Wall rezzed last turn can't be targeted")
        (click-card state :runner "Pachinko")
        (is (not (:rezzed (refresh pach))) "Pachinko derezzed")
        (is (= 2 (:click (get-runner))) "Spent 1 click")
        (is (= 1 (:credit (get-runner))) "Spent 1c")))))
