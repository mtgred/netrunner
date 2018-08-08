(ns game-test.cards.ice.blockchain
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest blockchain
  (do-game
    (new-game {:corp {:deck ["Blockchain" (qty "Beanstalk Royalties" 5)]}})
    (core/gain state :corp :credit 2 :click 5)
    (play-from-hand state :corp "Blockchain" "HQ")
    (let [bc (get-ice state :hq 0)]
      (core/rez state :corp bc)
      (card-ability state :corp bc 0)
      (is (last-log-contains? state "uses Blockchain to gain 0 subroutines") "No subroutines gained because no Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (card-ability state :corp bc 0)
      (is (last-log-contains? state "uses Blockchain to gain 0 subroutines") "No subroutines gained because only 1 Transaction is in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (card-ability state :corp bc 0)
      (is (last-log-contains? state "uses Blockchain to gain 1 subroutine") "1 subroutine gained because 2 Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (card-ability state :corp bc 0)
      (is (last-log-contains? state "uses Blockchain to gain 1 subroutine") "1 subroutine gained because 3 Transactions are in Archives")
      (play-from-hand state :corp "Beanstalk Royalties")
      (card-ability state :corp bc 0)
      (is (last-log-contains? state "uses Blockchain to gain 2 subroutines") "2 subroutines gained because 4 Transactions are in Archives")
      (is (= 12 (:credit (get-corp))) "Corp has 12 credits from four Beanstalks")
      (card-subroutine state :corp bc 0)
      (is (= 13 (:credit (get-corp))) "Corp gained 1 credit from Blockchain")
      (is (= 4 (:credit (get-runner))) "Runner lost 1 credit from Blockchain"))))
