(ns game-test.cards.ice.next-diamond
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest next-diamond
  ;; NEXT Diamond - Rez cost is lowered by 1 for each rezzed NEXT ice
  (testing "Base rez cost"
    (do-game
      (new-game {:corp {:deck ["NEXT Diamond"]}})
      (core/gain state :corp :credit 5)
      (is (= 10 (:credit (get-corp))) "Corp starts with 10 credits")
      (play-from-hand state :corp "NEXT Diamond" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (is (zero? (:credit (get-corp))) "Corp spends 10 credits to rez")))
  (testing "Lowered rez cost"
    (do-game
      (new-game {:corp {:deck ["NEXT Diamond" "NEXT Opal" "NEXT Bronze" "Kakugo"]}})
      (core/gain state :corp :credit 13 :click 1)
      (play-from-hand state :corp "NEXT Diamond" "HQ")
      (play-from-hand state :corp "NEXT Opal" "HQ")
      (play-from-hand state :corp "NEXT Bronze" "R&D")
      (play-from-hand state :corp "Kakugo" "Archives")
      (core/rez state :corp (get-ice state :hq 1))
      (core/rez state :corp (get-ice state :archives 0))
      (is (= 9 (:credit (get-corp))) "Corp starts with 9 credits")
      (core/rez state :corp (get-ice state :hq 0))
      (is (zero? (:credit (get-corp))) "Corp spends 9 credits to rez"))))
