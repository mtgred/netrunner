(ns game-test.cards.agendas.broad-daylight
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest broad-daylight
  ;; Broad Daylight
  (testing "take bad pub"
    (do-game
      (new-game {:corp {:deck [(qty "Broad Daylight" 3)]}})
      (is (zero? (:bad-publicity (get-corp))) "Corp start with no bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (:bad-publicity (get-corp))) "Corp gains 1 bad pub")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "No")
      (is (= 1 (:bad-publicity (get-corp))) "Corp doesn't gain bad pub")
      (is (= 1 (get-counters (get-scored state :corp 1) :agenda)) "Should gain 1 agenda counter")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 2 (:bad-publicity (get-corp))) "Corp gains 1 bad pub")
      (is (= 2 (get-counters (get-scored state :corp 2) :agenda)) "Should gain 2 agenda counters")))
  (testing "deal damage"
    (do-game
      (new-game {:corp {:deck ["Broad Daylight"]}})
      (core/gain state :corp :bad-publicity 3)
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 4 (:bad-publicity (get-corp))) "Corp gains 1 bad pub")
      (is (= 4 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")
      (is (empty? (:discard (get-runner))) "Runner has no discarded cards")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner took 2 damage")
      (card-ability state :corp (get-scored state :corp 0) 0)
      (is (= 2 (count (:discard (get-runner)))) "Runner didn't take additional damage")))
  (testing "bad pub triggers"
    (do-game
      (new-game {:corp {:deck ["Broad Daylight" "Broadcast Square"]}})
      (core/gain state :corp :bad-publicity 1)
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 1 (:bad-publicity (get-corp))) "Corp start with one bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (:bad-publicity (get-corp))) "Doesn't gain additional bad pub yet")
      (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
      (click-prompt state :runner "0")  ;; Runner doesn't pump trace; loses trace
      (is (= 1 (:bad-publicity (get-corp))) "Blocks gaining additional bad pub")
      (is (= 1 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 1 agenda counter")))
  (testing "bad pub triggers - more cases"
    (do-game
      (new-game {:corp {:deck ["Broad Daylight" "Broadcast Square"]}})
      (core/gain state :corp :bad-publicity 1)
      (play-from-hand state :corp "Broadcast Square" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 1 (:bad-publicity (get-corp))) "Corp start with one bad pub")
      (play-and-score state "Broad Daylight")
      (click-prompt state :corp "Yes")
      (is (= 1 (:bad-publicity (get-corp))) "Doesn't gain additional bad pub yet")
      (click-prompt state :corp "0")  ;; Corp doesn't pump trace, base 3
      (click-prompt state :runner "5")  ;; Runner pumps trace; wins trace
      (is (= 2 (:bad-publicity (get-corp))) "Gains additional bad pub")
      (is (= 2 (get-counters (get-scored state :corp 0) :agenda)) "Should gain 2 agenda counter"))))
