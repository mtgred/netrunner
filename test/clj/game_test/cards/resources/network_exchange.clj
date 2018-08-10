(ns game-test.cards.resources.network-exchange
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest network-exchange
  ;; ICE install costs 1 more except for inner most
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Paper Wall" 3)]}
                 :runner {:deck ["Network Exchange"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Network Exchange")
      (take-credits state :runner)
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 8 (:credit (get-corp))) "Paid 0 to install Paper Wall")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 6 (:credit (get-corp))) "Paid 1 extra  to install Paper Wall")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (is (= 3 (:credit (get-corp))) "Paid 1 extra  to install Paper Wall")))
  (testing "Architect 1st sub should ignore additional install cost"
    (do-game
      (new-game {:corp {:deck [(qty "Architect" 3)]}
                 :runner {:deck ["Network Exchange"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (take-credits state :corp) ; corp has 7 credits
      (play-from-hand state :runner "Network Exchange")
      (take-credits state :runner)
      (let [architect (get-ice state :hq 0)]
        (core/rez state :corp architect)
        (is (= 3 (:credit (get-corp))) "Corp has 3 credits after rez")
        (core/move state :corp (find-card "Architect" (:hand (get-corp))) :deck)
        (card-subroutine state :corp architect 0)
        (click-prompt state :corp (find-card "Architect" (:deck (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 3 (:credit (get-corp))) "Corp has 7 credits")))))
