(ns game-test.cards.ice.free-lunch
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest free-lunch
  ;; Free Lunch - Spend 1 power counter to make Runner lose 1c
  (do-game
    (new-game {:corp {:deck ["Free Lunch"]}})
    (play-from-hand state :corp "Free Lunch" "HQ")
    (let [fl (get-ice state :hq 0)]
      (core/rez state :corp fl)
      (card-subroutine state :corp fl 0)
      (is (= 1 (get-counters (refresh fl) :power)) "Free Lunch has 1 power counter")
      (card-subroutine state :corp fl 0)
      (is (= 2 (get-counters (refresh fl) :power)) "Free Lunch has 2 power counters")
      (is (= 5 (:credit (get-runner))))
      (card-ability state :corp (refresh fl) 0)
      (is (= 1 (get-counters (refresh fl) :power)) "Free Lunch has 1 power counter")
      (is (= 4 (:credit (get-runner))) "Runner lost 1 credit"))))
