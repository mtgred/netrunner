(ns game-test.cards.agendas.next-wave-2
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest next-wave-2
  ;; NEXT Wave 2
  (do-game
    (new-game {:corp {:deck [(qty "NEXT Wave 2" 2) "NEXT Bronze"]}})
    (is (zero? (:brain-damage (get-runner))) "Runner should start with 0 brain damage")
    (play-from-hand state :corp "NEXT Bronze" "HQ")
    (let [nxbr (get-ice state :hq 0)]
      (core/rez state :corp nxbr))
    (play-and-score state "NEXT Wave 2")
    (click-prompt state :corp "No")
    (is (zero? (:brain-damage (get-runner))) "Runner should stay at 0 brain damage")
    (play-and-score state "NEXT Wave 2")
    (click-prompt state :corp "Yes")
    (is (= 1 (:brain-damage (get-runner))) "Runner should gain 1 brain damage")))
