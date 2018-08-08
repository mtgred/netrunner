(ns game-test.cards.upgrades.crisium-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest crisium-grid
  ;; Crisium Grid
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 2)]}
                 :runner {:deck ["Desperado" "Temüjin Contract"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (core/rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (is (= 4 (:credit (get-corp))) "Corp has 4 credits")
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Desperado")
      (play-from-hand state :runner "Temüjin Contract")
      (click-prompt state :runner "HQ")
      (run-empty-server state "HQ")
      (is (= 2 (:credit (get-runner))) "No Desperado or Temujin credits")
      (is (not (:successful-run (:register (get-runner)))) "No successful run in register")))
  (testing "with Gauntlet, #3082"
    (do-game
      (new-game {:corp {:deck [(qty "Crisium Grid" 2)(qty "Vanilla" 2)]}
                 :runner {:deck ["The Gauntlet" "Temüjin Contract"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (core/rez state :corp (get-content state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "The Gauntlet")
      (run-on state "HQ")
      (run-successful state)
      (is (seq (:prompt (get-runner))) "The Gauntlet has a prompt"))))
