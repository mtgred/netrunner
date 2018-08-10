(ns game-test.cards.resources.jak-sinclair
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jak-sinclair
  ;; Jak Sinclair
  (testing "Lost clicks carry through to when turn starts fully #1764"
    (do-game
      (new-game {:corp {:deck [(qty "Enigma" 3)]}
                 :runner {:deck [(qty "Jak Sinclair" 3)]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Jak Sinclair")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [eni (get-ice state :hq 0)
            jak (get-resource state 0)]
        (core/rez state :corp eni)
        (is (:runner-phase-12 @state) "Runner in Step 1.2")
        (card-ability state :runner jak 0)
        (click-prompt state :runner "HQ")
        (card-subroutine state :corp (refresh eni) 0)
        (run-successful state)
        (core/end-phase-12 state :runner nil)
        (is (= 3 (:click (get-runner))) "Enigma took a click")))))
