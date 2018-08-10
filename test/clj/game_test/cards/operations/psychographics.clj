(ns game-test.cards.operations.psychographics
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest psychographics
  ;; Psychographics - Place advancements up to the number of Runner tags on a card
  (do-game
    (new-game {:corp {:deck ["Psychographics" "Project Junebug"]}})
    (core/gain state :runner :tag 4)
    (play-from-hand state :corp "Project Junebug" "New remote")
    (let [pj (get-content state :remote1 0)]
      (play-from-hand state :corp "Psychographics")
      (click-prompt state :corp "4")
      (click-card state :corp pj)
      (is (= 1 (:credit (get-corp))) "Spent 4 credits")
      (is (= 4 (get-counters (refresh pj) :advancement)) "Junebug has 4 advancements"))))
