(ns game-test.cards.operations.cerebral-static
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cerebral-static
  ;; Cerebral Static
  (testing "vs Chaos Theory"
    (do-game
      (new-game {:corp {:deck ["Cerebral Static" "Lag Time"]}
                 :runner {:id "Chaos Theory: Wünderkind"
                          :deck [(qty "Sure Gamble" 3)]}})
      (is (= 5 (core/available-mu state)) "CT starts with 5 memory")
      (play-from-hand state :corp "Cerebral Static")
      (is (= 4 (core/available-mu state)) "Cerebral Static causes CT to have 4 memory")
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (core/available-mu state)) "CT 5 memory restored"))))
