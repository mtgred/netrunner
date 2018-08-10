(ns game-test.cards.events.rip-deal
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest rip-deal
  ;; Rip Deal - replaces number of HQ accesses with heap retrieval
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 2)(qty "Vanilla" 2)]}
                 :runner {:deck ["The Gauntlet" "Rip Deal" (qty "Easy Mark" 2)]}})
      (trash-from-hand state :runner "Easy Mark")
      (trash-from-hand state :runner "Easy Mark")
      (take-credits state :corp)
      (play-from-hand state :runner "Rip Deal")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= "Choose 1 card(s) to move from the Heap to your Grip" (-> (get-runner) :prompt first :msg)))))
  (testing "with Gauntlet #2942"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 2)(qty "Vanilla" 2)]}
                 :runner {:deck ["The Gauntlet" "Rip Deal" (qty "Easy Mark" 2)]}})
      (trash-from-hand state :runner "Easy Mark")
      (trash-from-hand state :runner "Easy Mark")
      (play-from-hand state :corp "Vanilla" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "The Gauntlet")
      (play-from-hand state :runner "Rip Deal")
      (run-successful state)
      (click-prompt state :runner "1")
      (click-prompt state :runner "Replacement effect")
      (is (= "Choose 2 card(s) to move from the Heap to your Grip" (-> (get-runner) :prompt first :msg))))))
