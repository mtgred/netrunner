(ns game-test.cards.resources.muertos-gang-member
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest muertos-gang-member
  ;; Muertos Gang Member - Install and Trash
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Tollbooth" "Ice Wall"]}
                 :runner {:deck [(qty "Hedge Fund" 3) "Muertos Gang Member"]}})
      (play-from-hand state :corp "Tollbooth" "HQ")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (let [toll (get-ice state :hq 0)
            iw (get-ice state :archives 0)]
        (core/rez state :corp iw)
        (core/move state :runner (find-card "Hedge Fund" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Muertos Gang Member")
        (click-card state :corp (refresh iw))
        (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-runner)))) "2 cards in Runner's hand")
        (let [muer (get-resource state 0)]
          (card-ability state :runner muer 0)
          (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
          (click-card state :corp toll)
          (is (:rezzed (refresh toll)) "Tollbooth was rezzed")))))
  (testing "Account for Reina interaction, #1098"
    (do-game
      (new-game {:corp {:deck ["Tollbooth" "Ice Wall"]}
                 :runner {:id "Reina Roja: Freedom Fighter"
                          :deck [(qty "Hedge Fund" 3) "Muertos Gang Member"]}})
      (play-from-hand state :corp "Tollbooth" "HQ")
      (play-from-hand state :corp "Ice Wall" "Archives")
      (let [toll (get-ice state :hq 0)
            iw (get-ice state :archives 0)]
        (core/rez state :corp iw)
        (take-credits state :corp)
        (core/lose state :corp :credit 100)
        (core/move state :runner (find-card "Hedge Fund" (:hand (get-runner))) :deck)
        (play-from-hand state :runner "Muertos Gang Member")
        (click-card state :corp (refresh iw))
        (is (not (:rezzed (refresh iw))) "Ice Wall derezzed")
        (is (= 2 (count (:hand (get-runner)))) "2 cards in Runner's hand")
        (let [muer (get-resource state 0)]
          (card-ability state :runner muer 0)
          (is (= 3 (count (:hand (get-runner)))) "Runner drew a card from Muertos")
          (click-card state :corp toll)
          (is (:rezzed (refresh toll)) "Tollbooth was rezzed")
          (is (zero? (:credit (get-corp))) "Corp has 0 credits"))))))
