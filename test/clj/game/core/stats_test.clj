(ns game.core.stats-test
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest click-count
  (testing "clicks gained"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                 :runner {:deck [(qty "Sure Gamble" 10)]}})
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (take-credits state :runner)
      (is (= 9 (get-in @state [:stats :corp :gain :click])) "Corp has started 3 turns")
      (is (= 8 (get-in @state [:stats :runner :gain :click])) "Runner has started 2 turns")
      (is (= 8 (get-in @state [:stats :corp :gain :card])) "Corp drew 3 times")
      (is (= 5 (get-in @state [:stats :runner :gain :card])) "Runner did not draw")
      (is (= 6 (get-in @state [:stats :corp :gain :credit])) "Corp gained 6 credits")
      (is (= 8 (get-in @state [:stats :runner :gain :credit])) "Runner gained 8 credits")
      (is (= 8 (get-in @state [:stats :runner :gain :click])) "Runner has started 2 turns")
      (is (= 6 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 6 credits")
      (is (= 8 (get-in @state [:stats :runner :click :credit])) "Runner clicked for 8 credits")
      (play-from-hand state :corp "Hedge Fund")
      (is (= 6 (get-in @state [:stats :corp :click :credit])) "Corp clicked for 6 credits")
      (is (= 15 (get-in @state [:stats :corp :gain :credit])) "Corp gained 15 credits")
      (is (= 5 (get-in @state [:stats :corp :spent :credit])) "Corp spent 5 credits")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (is (= 8 (get-in @state [:stats :runner :click :credit])) "Corp clicked for 8 credits")
      (is (= 17 (get-in @state [:stats :runner :gain :credit])) "Corp gained 17 credits")
      (is (= 5 (get-in @state [:stats :runner :spent :credit])) "Runner spent 5 credits"))))
