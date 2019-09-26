(ns game-test.engine.actions
  (:require [game.core :as core]
            [game.utils :as utils]
            [jinteki.utils :as jutils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest change-test
  (testing "base vs additional"
    (do-game
      (new-game)
      (testing "Bad Publicity"
        (is (zero? (jutils/count-bad-pub state)) "Corp starts with 0 bad pub")
        (core/change state :corp {:key :bad-publicity :delta 1})
        (is (= 1 (jutils/count-bad-pub state)) "Corp has gained 1 bad pub")
        (is (= 1 (get-in @state [:corp :bad-publicity :base])) "Only gained in the base")
        (is (zero? (get-in @state [:corp :bad-publicity :additional])) "Only gained in the base")
        (core/change state :corp {:key :bad-publicity :delta -1})
        (is (zero? (jutils/count-bad-pub state)) "Corp has lost 1 bad pub")
        (is (zero? (get-in @state [:corp :bad-publicity :base])) "Only lost in the base")
        (is (zero? (get-in @state [:corp :bad-publicity :additional])) "No change on loss either"))
      (testing "Tags"
        (is (zero? (jutils/count-tags state)) "Runner starts with 0 tags")
        (core/change state :runner {:key :tag :delta 1})
        (is (= 1 (jutils/count-tags state)) "Runner has gained 1 tag")
        (is (= 1 (get-in @state [:runner :tag :base])) "Only gained in the base")
        (is (zero? (get-in @state [:runner :tag :additional])) "Only gained in the base")
        (core/change state :runner {:key :tag :delta -1})
        (is (zero? (jutils/count-tags state)) "Runner has lost 1 tag")
        (is (zero? (get-in @state [:runner :tag :base])) "Only gained in the base")
        (is (zero? (get-in @state [:runner :tag :additional])) "No change on loss either"))))
  (testing "Generic changes"
    (do-game
      (new-game)
      (testing "Agenda points"
        (is (zero? (get-in @state [:corp :agenda-point])) "Corp starts with 0 agenda points")
        (core/change state :corp {:key :agenda-point :delta 1})
        (is (= 1 (get-in @state [:corp :agenda-point])) "Corp has gained 1 agenda point")
        (core/change state :corp {:key :agenda-point :delta -1})
        (is (zero? (get-in @state [:corp :agenda-point])) "Corp has lost 1 agenda point")
        (core/change state :corp {:key :agenda-point :delta -1})
        (is (= -1 (get-in @state [:corp :agenda-point])) "Corp can go below zero agenda points")))))
