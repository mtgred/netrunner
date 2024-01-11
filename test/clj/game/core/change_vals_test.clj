(ns game.core.change-vals-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest change-test
  (testing "base vs additional"
    (do-game
      (new-game)
      (testing "Bad Publicity"
        (is (zero? (count-bad-pub state)) "Corp starts with 0 bad pub")
        (change state :corp :bad-publicity 1)
        (is (= 1 (count-bad-pub state)) "Corp has gained 1 bad pub")
        (is (= 1 (get-in @state [:corp :bad-publicity :base])) "Only gained in the base")
        (is (zero? (get-in @state [:corp :bad-publicity :additional])) "Only gained in the base")
        (change state :corp :bad-publicity -1)
        (is (zero? (count-bad-pub state)) "Corp has lost 1 bad pub")
        (is (zero? (get-in @state [:corp :bad-publicity :base])) "Only lost in the base")
        (is (zero? (get-in @state [:corp :bad-publicity :additional])) "No change on loss either"))
      (testing "Tags"
        (is (zero? (count-tags state)) "Runner starts with 0 tags")
        (is (zero? (count-real-tags state)))
        (change state :runner :tag 1)
        (is (= 1 (count-tags state)) "Runner has gained 1 tag")
        (is (= 1 (count-real-tags state)))
        (change state :runner :tag -1)
        (is (zero? (count-tags state)) "Runner has lost 1 tag")
        (is (zero? (count-real-tags state))))))
  (testing "Generic changes"
    (testing "Agenda points"
      (do-game
        (new-game)
        (is (zero? (get-in @state [:corp :agenda-point])) "Corp starts with 0 agenda points")
        (change state :corp :agenda-point 1)
        (is (= 1 (get-in @state [:corp :agenda-point])) "Corp has gained 1 agenda point")
        (change state :corp :agenda-point -1)
        (is (zero? (get-in @state [:corp :agenda-point])) "Corp has lost 1 agenda point")
        (change state :corp :agenda-point -1)
        (is (= -1 (get-in @state [:corp :agenda-point])) "Corp can go below 0 agenda points")))
    (testing "Link"
      (do-game
        (new-game)
        (is (zero? (get-link state)) "Runner starts with 0 link")
        (change state :runner :link 1)
        (is (= 1 (get-link state)) "Runner has gained 1 link")
        (change state :runner :link -1)
        (is (zero? (get-link state)) "Runner has lost 1 link")
        (change state :runner :link -1)
        (is (= -1 (get-link state)) "Runner can go below 0 link")))
    (testing "Hand size"
      (do-game
        (new-game)
        (is (= 5 (hand-size :runner)) "Runner starts with 5 hand size")
        (change state :runner :hand-size 1)
        (is (= 6 (hand-size :runner)) "Runner has gained 1 hand size")
        (change state :runner :hand-size -1)
        (is (= 5 (hand-size :runner)) "Runner has lost 1 hand size")
        (change state :runner :hand-size -6)
        (is (neg? (hand-size :runner)) "Runner has negative hand size")))
    (testing "Memory"
      (do-game
        (new-game)
        (is (= 4 (core/available-mu state :runner)) "Runner starts with 4 MU")
        (change state :runner :memory 1)
        (is (= 5 (core/available-mu state :runner)) "Runner has gained 1 memory")
        (change state :runner :memory -1)
        (is (= 4 (core/available-mu state :runner)) "Runner has lost 1 memory")
        (change state :runner :memory -6)
        (is (neg? (core/available-mu state :runner)) "Runner has negative memory")))))
