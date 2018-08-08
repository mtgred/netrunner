(ns game-test.cards.events.early-bird
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest early-bird
  ;; Early Bird - Priority, make a run and gain a click
  (do-game
    (new-game {:runner {:deck ["Early Bird"]}})
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (play-from-hand state :runner "Early Bird")
    (is (= 3 (:click (get-runner))) "Card not played, Early Bird priority restriction")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Early Bird")
    (click-prompt state :runner "Archives")
    (is (= 4 (:click (get-runner))) "Early Bird gains click")))
