(ns game-test.cards.identities.nisei-division-the-next-generation
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest nisei-division-the-next-generation
  ;; Nisei Division - Gain 1 credit from every psi game
  (do-game
    (new-game {:corp {:id "Nisei Division: The Next Generation"
                      :deck [(qty "Snowflake" 2)]}})
    (play-from-hand state :corp "Snowflake" "HQ")
    (play-from-hand state :corp "Snowflake" "HQ")
    (take-credits state :corp)
    (let [s1 (get-in @state [:corp :servers :hq :ices 0])
          s2 (get-in @state [:corp :servers :hq :ices 1])]
      (run-on state "HQ")
      (core/rez state :corp s2)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s2 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "0 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game")
      (core/no-action state :corp nil)
      (core/rez state :corp s1)
      (is (= 4 (:credit (get-corp))))
      (card-subroutine state :corp s1 0)
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (= 5 (:credit (get-corp))) "Gained 1 credit from psi game"))))
