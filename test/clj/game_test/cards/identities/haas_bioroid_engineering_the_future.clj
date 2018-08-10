(ns game-test.cards.identities.haas-bioroid-engineering-the-future
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest haas-bioroid-engineering-the-future
  ;; Engineereing the Future
  (testing "interaction with Employee Strike"
    (do-game
      (new-game {:corp {:id "Haas-Bioroid: Engineering the Future"
                        :deck [(qty "Eli 1.0" 3) "Paywall Implementation"]}
                 :runner {:deck ["Employee Strike"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits at turn end")
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
      (play-from-hand state :corp "Paywall Implementation")
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (is (= 8 (:credit (get-corp))) "Corp did not gain 1cr from EtF")
      (take-credits state :corp)
      (take-credits state :runner)
      (play-from-hand state :corp "Eli 1.0" "New remote")
      (is (= 9 (:credit (get-corp))) "Corp gained 1cr from EtF"))))
