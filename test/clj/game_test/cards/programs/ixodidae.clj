(ns game-test.cards.programs.ixodidae
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ixodidae
  ;; Ixodidae should not trigger on psi-games
  (do-game
    (new-game {:corp {:deck ["Snowflake"]}
               :runner {:deck ["Ixodidae" "Lamprey"]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp at 7 credits")
    (play-from-hand state :runner "Ixodidae")
    (play-from-hand state :runner "Lamprey")
    (is (= 3 (:credit (get-runner))) "Runner paid 3 credits to install Ixodidae and Lamprey")
    (run-on state :hq)
    (let [s (get-ice state :hq 0)]
      (core/rez state :corp s)
      (card-subroutine state :corp s 0)
      (is (prompt-is-card? state :corp s) "Corp prompt is on Snowflake")
      (is (prompt-is-card? state :runner s) "Runner prompt is on Snowflake")
      (is (= 6 (:credit (get-corp))) "Corp paid 1 credit to rezz Snowflake")
      (click-prompt state :corp "1 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Corp paid 1 credit to psi game")
      (is (= 2 (:credit (get-runner))) "Runner did not gain 1 credit from Ixodidae when corp spent on psi game")
      (run-continue state)
      (run-successful state)
      (is (= 4 (:credit (get-corp))) "Corp lost 1 credit to Lamprey")
      (is (= 3 (:credit (get-runner))) "Runner gains 1 credit from Ixodidae due to Lamprey"))))
