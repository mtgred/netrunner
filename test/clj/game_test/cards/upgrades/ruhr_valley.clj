(ns game-test.cards.upgrades.ruhr-valley
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ruhr-valley
  ;; Ruhr Valley
  (testing "Basic test - As an additional cost to make a run on this server, the Runner must spend a click."
    (do-game
      (new-game {:corp {:deck ["Ruhr Valley"]}})
      (play-from-hand state :corp "Ruhr Valley" "HQ")
      (take-credits state :corp)
      (let [ruhr (get-content state :hq 0)]
        (core/rez state :corp ruhr)
        (is (= 4 (:click (get-runner))))
        (run-on state :hq)
        (run-jack-out state)
        (is (= 2 (:click (get-runner))))
        (take-credits state :runner 1)
        (is (= 1 (:click (get-runner))))
        (is (not (core/can-run-server? state "HQ")) "Runner can't run - no additional clicks")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 4 (:click (get-runner))))
        (is (= 7 (:credit (get-runner))))
        (run-on state :hq)
        (run-successful state)
        (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash / 7 cr - 4 cr
        (is (= 2 (:click (get-runner))))
        (is (= 3 (:credit (get-runner))))
        (run-on state :hq)
        (run-jack-out state)
        (is (= 1 (:click (get-runner)))))))
  (testing "If the runner trashes with one click left, the ability to run is enabled"
    (do-game
      (new-game {:corp {:deck ["Ruhr Valley"]}})
      (play-from-hand state :corp "Ruhr Valley" "HQ")
      (take-credits state :corp)
      (let [ruhr (get-content state :hq 0)]
        (core/rez state :corp ruhr)
        (is (= 4 (:click (get-runner))))
        (run-on state :rd)
        (run-jack-out state)
        (is (= 3 (:click (get-runner))))
        (run-on state :hq)
        (run-successful state)
        (click-prompt state :runner "Pay 4 [Credits] to trash") ; pay to trash / 6 cr - 4 cr
        (is (= 1 (:click (get-runner))))
        (run-on state :hq)))))
