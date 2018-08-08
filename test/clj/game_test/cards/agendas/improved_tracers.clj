(ns game-test.cards.agendas.improved-tracers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest improved-tracers
  ;; Improved Tracers
  (do-game
    (new-game {:corp {:deck ["Improved Tracers" "News Hound" "Information Overload"]}})
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "News Hound" "HQ")
    (play-from-hand state :corp "Information Overload" "R&D")
    (let [nh (get-ice state :hq 0)
          io (get-ice state :rd 0)]
      (core/rez state :corp nh)
      (core/rez state :corp io)
      (is (= 4 (:current-strength (refresh nh))) "Should start with base strength of 4")
      (is (= 7 (:credit (get-corp))) "Should have 7 credits after rez")
      (play-and-score state "Improved Tracers")
      (is (= 5 (:current-strength (refresh nh))) "Should gain 1 strength from 4 to 5")
      (take-credits state :corp)
      (run-on state "HQ")
      (card-subroutine state :corp nh 0)
      (is (= 1 (-> (get-corp) :prompt first :bonus)) "Should gain 1 bonus trace strength")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (:tag (get-runner))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :corp nh 0)
      (is (= 1 (-> (get-corp) :prompt first :bonus))
          "Should gain only 1 bonus trace strength regardless of number of runs in a turn")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 2 (:tag (get-runner))))
      (run-on state "R&D")
      (card-ability state :corp io 1)
      (is (zero? (-> (get-corp) :prompt first :bonus)) "Should gain 0 bonus trace strength, as it's an encounter ability"))))
