(ns game-test.cards.operations.economic-warfare
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest economic-warfare
  ;; Economic Warfare - If successful run last turn, make the runner lose 4 credits if able
  (do-game
    (new-game {:corp {:deck [(qty "Economic Warfare" 3)]}})
    (play-from-hand state :corp "Economic Warfare")
    (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
    (is (= 3 (count (:hand (get-corp)))) "Corp still has 3 cards")
    (take-credits state :corp)
    (run-on state :archives)
    (run-successful state)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (play-from-hand state :corp "Economic Warfare")
    (is (zero? (:credit (get-runner))) "Runner has 0 credits")
    (take-credits state :corp)
    (run-on state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Economic Warfare")
    (is (= 3 (:credit (get-runner))) "Runner has 3 credits")))
