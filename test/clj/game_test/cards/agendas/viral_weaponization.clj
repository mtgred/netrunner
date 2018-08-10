(ns game-test.cards.agendas.viral-weaponization
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest viral-weaponization
  ;; Viral Weaponization - at the end of turn scored, do 1 net damage for each card in grip
  (testing "Score on corp turn"
    (do-game
      (new-game {:corp {:deck [(qty "Viral Weaponization" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
      (play-and-score state "Viral Weaponization")
      (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Runner takes damage at end of turn")
      (core/click-draw state :runner 1)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-runner)))) "Runner doesn't take damage in future turns")
      (play-from-hand state :runner "Sure Gamble")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")
      (play-and-score state "Viral Weaponization")
      (take-credits state :corp)
      (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")))
  (testing "Score on runners turn"
    (do-game
      (new-game {:corp {:deck ["Viral Weaponization" "Plan B"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]}})
      (starting-hand state :runner ["Sure Gamble" "Sure Gamble"])
      (play-from-hand state :corp "Plan B" "New remote")
      (core/add-prop state :corp (get-content state :remote1 0) :advance-counter 4)
      (take-credits state :corp)
      (run-on state "Server 1")
      (run-successful state)
      (click-prompt state :corp "Yes")
      (click-card state :corp (find-card "Viral Weaponization" (:hand (get-corp))))
      (is (= ["Pay 1 [Credits] to trash" "No action"] (:choices (prompt-map :runner))))
      (click-prompt state :runner "No action")
      (is (= 2 (count (:hand (get-runner)))) "Runner doesn't take damage when scored")
      (take-credits state :runner)
      (is (zero? (count (:hand (get-runner)))) "Runner takes damage at end of turn"))))
