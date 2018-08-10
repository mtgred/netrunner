(ns game-test.cards.operations.riot-suppression
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest riot-suppression
  ;; Riot Suppression - lose 3 clicks or take 1 brain damage
  (testing "Take 1 brain damage"
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no brain damage")
      (click-prompt state :runner "Yes")
      (is (= 1 (count (:discard (get-runner)))) "1 card lost to brain damage")
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 4 (:click (get-runner))) "Runner has all clicks the following turn")))
  (testing "Lose 3 clicks"
    (do-game
      (new-game {:corp {:deck ["Riot Suppression" "Adonis Campaign"]}})
      (play-from-hand state :corp "Adonis Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (take-credits state :runner)
      (play-from-hand state :corp "Riot Suppression")
      (is (empty? (:discard (get-runner))) "Runner discard is empty")
      (is (zero? (:brain-damage (get-runner))) "Runner starts with no brain damage")
      (click-prompt state :runner "No")
      (is (empty? (:discard (get-runner))) "Runner discard statys empty")
      (is (zero? (:brain-damage (get-runner))) "Runner takes no brain damage")
      (is (= 1 (count (:discard (get-corp)))) "No corp cards trashed")
      (is (= 1 (count (:rfg (get-corp)))) "Riot Suppestion removed from game")
      (take-credits state :corp)
      (is (= 1 (:click (get-runner))) "Runner has 3 fewer clicks following turn"))))
