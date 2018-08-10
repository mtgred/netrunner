(ns game-test.cards.programs.self-modifying-code
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest self-modifying-code
  ;; Trash & pay 2 to search deck for a program and install it. Shuffle.
  (do-game
    (new-game {:runner {:deck [(qty "Self-modifying Code" 3) "Reaver"]}})
    (starting-hand state :runner ["Self-modifying Code" "Self-modifying Code"])
    (core/gain state :runner :credit 5)
    (take-credits state :corp)
    (play-from-hand state :runner "Self-modifying Code")
    (play-from-hand state :runner "Self-modifying Code")
    (let [smc1 (get-program state 0)
          smc2 (get-program state 1)]
      (card-ability state :runner smc1 0)
      (click-prompt state :runner (find-card "Reaver" (:deck (get-runner))))
      (is (= 6 (:credit (get-runner))) "Paid 2 for SMC, 2 for install - 6 credits left")
      (is (= 1 (core/available-mu state)) "SMC MU refunded")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner smc2 0)
      (is (= 1 (count (:hand (get-runner)))) "1 card drawn due to Reaver before SMC program selection")
      (is (zero? (count (:deck (get-runner)))) "Deck empty"))))
