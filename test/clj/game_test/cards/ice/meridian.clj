(ns game-test.cards.ice.meridian
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest meridian
  (testing "ETR"
    (do-game
      (new-game {:corp {:deck ["Meridian"]}})
      (play-from-hand state :corp "Meridian" "HQ")
      (take-credits state :corp)
      (let [mer (get-ice state :hq 0)]
        (core/rez state :corp (refresh mer))
        (run-on state :hq)
        (card-subroutine state :corp (refresh mer) 0)
        (click-prompt state :runner "End the run")
        (is (not (:run @state)) "Run is ended")
        (is (empty? (:scored (get-runner))) "Not in runner score area")
        (is (= 1 (count (get-ice state :hq))) "ICE still installed"))))
  (testing "Score as -1 point agenda"
    (do-game
      (new-game {:corp {:deck ["Meridian"]}})
      (play-from-hand state :corp "Meridian" "HQ")
      (take-credits state :corp)
      (let [mer (get-ice state :hq 0)]
        (core/rez state :corp (refresh mer))
        (run-on state :hq)
        (card-subroutine state :corp (refresh mer) 0)
        (click-prompt state :runner "Add Meridian to score area")
        (is (:run @state) "Run is still live")
        (is (= 1 (count (:scored (get-runner)))) "In runner score area")
        (is (= -1 (:agenda-point (get-runner))) "Worth -1 agenda points")
        (is (empty? (get-ice state :hq)) "ICE uninstalled")))))
