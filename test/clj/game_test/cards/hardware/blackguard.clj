(ns game-test.cards.hardware.blackguard
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Blackguard"
                               "Snitch"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Blackguard")
    (is (= 6 (core/available-mu state)) "Runner has 6 MU")
    (play-from-hand state :runner "Snitch")
    (let [snitch (get-program state 0)
          iwall (get-ice state :archives 0)]
      (run-on state :archives)
      (card-ability state :runner snitch 0)
      (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))
