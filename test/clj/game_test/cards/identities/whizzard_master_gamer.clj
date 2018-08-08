(ns game-test.cards.identities.whizzard-master-gamer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest whizzard-master-gamer
  ;; Whizzard - Recurring credits
  (do-game
    (new-game {:runner {:id "Whizzard: Master Gamer"
                        :deck ["Sure Gamble"]}})
    (let [click-whizzard (fn [n] (dotimes [i n] (card-ability state :runner (:identity (get-runner)) 0)))]
      (is (changes-credits (get-runner) 1 (click-whizzard 1)))
      (is (changes-credits (get-runner) 2 (click-whizzard 5)) "Can't take more than 3 Whizzard credits")
      (take-credits state :corp)
      (is (changes-credits (get-runner) 3 (click-whizzard 3)) "Credits reset at start of Runner's turn")
      (take-credits state :runner)
      (is (changes-credits (get-runner) 0 (click-whizzard 1)) "Credits don't reset at start of Corp's turn"))))
