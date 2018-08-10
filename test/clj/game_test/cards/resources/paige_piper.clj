(ns game-test.cards.resources.paige-piper
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest paige-piper
  ;; Paige Piper
  (testing "interaction with Frantic Coding. Issue #2190"
    (do-game
      (new-game {:runner {:deck ["Paige Piper" (qty "Frantic Coding" 2) (qty "Sure Gamble" 3)
                                 (qty "Gordian Blade" 2) "Ninja" (qty "Bank Job" 3) (qty "Indexing" 2)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Paige Piper" "Frantic Coding" "Frantic Coding"])
      (play-from-hand state :runner "Paige Piper")
      (click-prompt state :runner "No")
      (take-credits state :runner) ; now 8 credits
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (click-prompt state :runner (find-card "Gordian Blade" (:deck (get-runner))))
      (is (= 1 (count (get-program state))) "Installed Gordian Blade")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "0")
      (is (= 1 (count (:discard (get-runner)))) "Paige Piper intervention stopped Frantic Coding from trashing 9 cards")
      (is (= 5 (:credit (get-runner))) "No charge to install Gordian")
      ;; a second Frantic Coding will not trigger Paige (once per turn)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (click-prompt state :runner (find-card "Ninja" (:deck (get-runner))))
      (is (= 2 (count (get-program state))) "Installed Ninja")
      (is (= 11 (count (:discard (get-runner)))) "11 cards in heap")
      (is (= 2 (:credit (get-runner))) "No charge to install Ninja"))))
