(ns game-test.cards.ice.enigma
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest enigma
  ;; Enigma - Force Runner to lose 1 click if able
  (do-game
    (new-game {:corp {:deck ["Enigma"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (let [enig (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-runner))))
      (core/rez state :corp enig)
      (card-subroutine state :corp enig 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click"))))
