(ns game-test.cards.resources.investigative-journalism
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest investigative-journalism
  ;; Investigative Journalism - 4 clicks and trash to give the Corp 1 bad pub
  (do-game
    (new-game {:runner {:deck ["Investigative Journalism"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Investigative Journalism")
    (is (empty? (get-resource state)) "Corp has no bad pub, couldn't install")
    (core/gain state :corp :bad-publicity 1)
    (play-from-hand state :runner "Investigative Journalism")
    (take-credits state :runner)
    (take-credits state :corp)
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (:click (get-runner))) "Spent 4 clicks")
    (is (= 1 (count (:discard (get-runner)))) "IJ is trashed")
    (is (= 2 (:bad-publicity (get-corp))) "Corp took 1 bad publicity")))
