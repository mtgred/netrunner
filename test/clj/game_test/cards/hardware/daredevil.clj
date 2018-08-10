(ns game-test.cards.hardware.daredevil
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest daredevil
  ;; Daredevil
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
               :runner {:deck ["Daredevil" (qty "Sure Gamble" 3) (qty "Easy Mark" 2)]}})
    (starting-hand state :runner ["Daredevil"])
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Daredevil")
    (is (= 6 (core/available-mu state)) "Gained 2 MU")
    (run-on state "HQ")
    (is (empty? (:hand (get-runner))) "No cards drawn")
    (run-jack-out state)
    (run-on state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")
    (run-jack-out state)
    (run-on state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "No cards drawn")))
