(ns game-test.cards.icebreakers.na-not-k
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest na-not-k
  ;; Na'Not'K - Strength adjusts accordingly when ice installed during run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Architect" "Eli 1.0"]}
                 :runner {:deck ["Na'Not'K"]}})
      (play-from-hand state :corp "Architect" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Na'Not'K")
      (let [nanotk (get-program state 0)
            architect (get-ice state :hq 0)]
        (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (core/rez state :corp architect)
        (is (= 2 (:current-strength (refresh nanotk))) "1 ice on HQ")
        (card-subroutine state :corp (refresh architect) 1)
        (click-card state :corp (find-card "Eli 1.0" (:hand (get-corp))))
        (click-prompt state :corp "HQ")
        (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength"))))
  (testing "Strength adjusts accordingly when run redirected to another server"
    (do-game
      (new-game {:corp {:deck ["Susanoo-no-Mikoto" "Crick" "Cortex Lock"]}
                 :runner {:deck ["Na'Not'K"]}})
      (play-from-hand state :corp "Cortex Lock" "HQ")
      (play-from-hand state :corp "Susanoo-no-Mikoto" "HQ")
      (play-from-hand state :corp "Crick" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Na'Not'K")
      (let [nanotk (get-program state 0)
            susanoo (get-ice state :hq 1)]
        (is (= 1 (:current-strength (refresh nanotk))) "Default strength")
        (run-on state "HQ")
        (core/rez state :corp susanoo)
        (is (= 3 (:current-strength (refresh nanotk))) "2 ice on HQ")
        (card-subroutine state :corp (refresh susanoo) 0)
        (is (= 2 (:current-strength (refresh nanotk))) "1 ice on Archives")
        (run-jack-out state)
        (is (= 1 (:current-strength (refresh nanotk))) "Back to default strength")))))
