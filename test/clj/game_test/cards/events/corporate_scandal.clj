(ns game-test.cards.events.corporate-scandal
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest corporate-scandal
  ;; Corporate Scandal - Corp has 1 additional bad pub even with 0
  (do-game
    (new-game {:corp {:deck ["Elizabeth Mills"]}
               :runner {:deck ["Corporate Scandal" "Activist Support"
                               "Raymond Flint" "Investigative Journalism"]}})
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 5 :click 1)
    (play-from-hand state :runner "Raymond Flint")
    (play-from-hand state :runner "Corporate Scandal")
    (is (empty? (:prompt (get-runner))) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :runner "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-resource state 1))) "IJ able to be installed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-runner))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :runner "Activist Support")
    (take-credits state :runner)
    (let [em (get-content state :remote1 0)]
      (core/rez state :corp em)
      (is (= 1 (:has-bad-pub (get-corp))) "Corp still has BP")
      (take-credits state :corp)
      (is (zero? (:bad-publicity (get-corp))) "Corp has BP, didn't take 1 from Activist Support"))))
