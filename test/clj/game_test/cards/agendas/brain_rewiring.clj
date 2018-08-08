(ns game-test.cards.agendas.brain-rewiring
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest brain-rewiring
  ;; Brain Rewiring
  (do-game
    (new-game {:corp {:deck ["Brain Rewiring"]}})
    (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
    (play-and-score state "Brain Rewiring")
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "2")
    (is (= 1 (count (:hand (get-runner)))))))
