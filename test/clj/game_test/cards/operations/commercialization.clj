(ns game-test.cards.operations.commercialization
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest commercialization
  ;; Commercialization
  (testing "Single advancement token"
    (do-game
      (new-game {:corp {:deck ["Commercialization"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 1)
      (play-from-hand state :corp "Commercialization")
      (click-card state :corp (refresh (get-ice state :hq 0)))
      (is (= 6 (:credit (get-corp))) "Gained 1 for single advanced ice from Commercialization")))
  (testing "Two advancement tokens"
    (do-game
      (new-game {:corp {:deck ["Commercialization"
                               "Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/add-counter state :corp (refresh (get-ice state :hq 0)) :advancement 2)
      (play-from-hand state :corp "Commercialization")
      (click-card state :corp (refresh (get-ice state :hq 0)))
      (is (= 7 (:credit (get-corp))) "Gained 2 for double advanced ice from Commercialization"))))
