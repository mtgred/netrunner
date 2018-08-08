(ns game-test.cards.agendas.breaking-news
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest breaking-news
  ;; Breaking News
  (do-game
    (new-game {:corp {:deck [(qty "Breaking News" 3)]}})
    (play-and-score state "Breaking News")
    (is (= 2 (get-in @state [:runner :tag])) "Runner receives 2 tags from Breaking News")
    (take-credits state :corp)
    (is (zero? (get-in @state [:runner :tag]))) "Two tags removed at the end of the turn"))
