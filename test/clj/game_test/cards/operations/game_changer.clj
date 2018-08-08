(ns game-test.cards.operations.game-changer
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest game-changer
  (letfn [(game-changer-test [num-agenda]
            (do-game
              (new-game {:corp {:deck ["Game Changer" "Hostile Takeover"]}
                         :runner {:deck [(qty "Fan Site" num-agenda)]}})
              (take-credits state :corp)
              (core/gain state :runner :click num-agenda)
              (dotimes [_ num-agenda]
                (play-from-hand state :runner "Fan Site"))
              (take-credits state :runner)
              (play-and-score state "Hostile Takeover")
              (is (= num-agenda (count (get-scored state :runner)))
                  (str "Runner should have " (utils/quantify num-agenda "Fan Site") " in play"))
              (let [clicks (:click (get-corp))
                    n (dec num-agenda)]
                (play-from-hand state :corp "Game Changer")
                (is (= (+ n clicks) (:click (get-corp))) (str "Corp should gain " (utils/quantify n "click")))
                (is (= 1 (-> (get-corp) :rfg count)) "Game Changer should be in rfg zone now"))))]
    (doall (map game-changer-test (range 5)))))
