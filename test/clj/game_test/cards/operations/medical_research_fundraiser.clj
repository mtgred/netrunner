(ns game-test.cards.operations.medical-research-fundraiser
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest medical-research-fundraiser
  ;; Medical Research Fundraiser - runner gains 8creds, runner gains 3creds
  (do-game
    (new-game {:corp {:deck ["Medical Research Fundraiser"]}})
    (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-from-hand state :corp "Medical Research Fundraiser")
    (is (= 10 (:credit (get-corp))) "Corp gains 8 credits")
    (is (= 8 (:credit (get-runner))) "Runner gains 3 credits")))
