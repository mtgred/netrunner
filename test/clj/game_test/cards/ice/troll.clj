(ns game-test.cards.ice.troll
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest troll
  ;; Troll
  (testing "Giving the runner a choice on successful trace shouldn't make runner pay trace first. #5335"
    (do-game
      (new-game {:corp {:deck ["Troll"]}})
      (play-from-hand state :corp "Troll" "HQ")
      (take-credits state :corp)
      (let [troll (get-ice state :hq 0)]
        (core/rez state :corp troll)
        (run-on state "HQ")
        (card-ability state :corp troll 0)
        (is (= :waiting (-> (get-runner) :prompt first :prompt-type)) "Runner waits for Corp to boost first")
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (click-prompt state :runner "End the run")
        (is (not (:run @state)) "Run is ended")))))
