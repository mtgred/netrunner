(ns game-test.cards.agendas.fetal-ai
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest fetal-ai
  ;; Fetal AI
  (testing "basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]}})
      (play-from-hand state :corp "Fetal AI" "New remote")
      (take-credits state :corp 2)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 2 [Credits] to steal")
      (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
      (is (= 3 (:credit (get-runner))) "Runner paid 2cr to steal Fetal AI")
      (is (= 1 (count (:scored (get-runner)))) "Runner stole Fetal AI"))
    (testing "can't afford to steal"
      (do-game
        (new-game {:corp {:deck [(qty "Fetal AI" 3)]}
                   :runner {:deck [(qty "Sure Gamble" 3) (qty "Diesel" 3) (qty "Quality Time" 3)]}})
        (play-from-hand state :corp "Fetal AI" "New remote")
        (take-credits state :corp 2)
        (core/lose state :runner :credit 5)
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (= 3 (count (:hand (get-runner)))) "Runner took 2 net damage from Fetal AI")
        (is (zero? (count (:scored (get-runner)))) "Runner could not steal Fetal AI")))))
