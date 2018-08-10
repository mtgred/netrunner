(ns game-test.cards.events.data-breach
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest data-breach
  ;; Data Breach
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck [(qty "Data Breach" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (play-from-hand state :runner "Data Breach")
      (core/no-action state :corp nil)
      (run-successful state)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
      (core/no-action state :corp nil)
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "No prompt to run a third time")
      (is (not (:run @state)) "Run is over")
      (play-from-hand state :runner "Data Breach")
      (run-jack-out state)
      (is (empty? (:prompt (get-runner))) "No option to run again on unsuccessful run")))
  (testing "FAQ 4.1 - ensure runner gets choice of activation order"
    (do-game
      (new-game {:runner {:deck ["Doppelgänger" (qty "Data Breach" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Doppelgänger")
      (play-from-hand state :runner "Data Breach")
      (core/no-action state :corp nil)
      (run-successful state)
      ; (click-prompt state :runner "No action")
      (click-prompt state :runner "Doppelgänger")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "HQ")
      (is (:run @state) "New run started")
      (is (= [:hq] (:server (:run @state))) "Running on HQ via Doppelgänger")
      (core/no-action state :corp nil)
      (run-successful state)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second Data Breach run on R&D triggered")
      (core/no-action state :corp nil)
      (run-successful state))))
