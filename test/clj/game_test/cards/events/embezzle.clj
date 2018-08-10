(ns game-test.cards.events.embezzle
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest embezzle
  ;; Embezzle
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Archer"]}
                 :runner {:deck ["Embezzle"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "ICE")
      (is (= 2 (count (:discard (get-corp)))) "HQ card trashed")
      (is (= 12 (:credit (get-runner))))))
  (testing "Check that trashed cards are trashed face-up"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Embezzle"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "ICE")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed")
      (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen")
      (is (= 8 (:credit (get-runner)))))))
