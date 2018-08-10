(ns game-test.cards.agendas.market-research
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest market-research
  ;; Market Research
  (do-game
    (new-game {:corp {:deck [(qty "Market Research" 2)]}})
    (testing "Runner is not tagged"
      (play-and-score state "Market Research")
      (is (= 2 (:agenda-point (get-corp))) "Only 4 advancements: scored for standard 2 points"))
    (testing "Runner is tagged"
      (core/gain state :runner :tag 1)
      (play-and-score state "Market Research")
      (is (= 5 (:agenda-point (get-corp))) "5 advancements: scored for 3 points"))))
