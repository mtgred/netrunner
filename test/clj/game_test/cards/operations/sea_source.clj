(ns game-test.cards.operations.sea-source
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sea-source
  ;; SEA Source
  (do-game
    (new-game {:corp {:deck ["SEA Source"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (take-credits state :runner)
    (is (zero? (:tag (get-runner))) "Runner should start with 0 tags")
    (play-from-hand state :corp "SEA Source")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (is (= 1 (:tag (get-runner))) "Runner should get 1 tag from losing SEA Source trace")))
