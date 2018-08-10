(ns game-test.cards.operations.enforcing-loyalty
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest enforcing-loyalty
  ;; Enforcing Loyalty - Win trace to trash installed card not of Runner's faction
  (do-game
    (new-game {:corp {:deck [(qty "Enforcing Loyalty" 2)]}
               :runner {:id "Chaos Theory: Wünderkind"
                        :deck ["Inti" "Caldera"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "Caldera")
    (take-credits state :runner)
    (play-from-hand state :corp "Enforcing Loyalty")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-program state 0))
    (is (empty? (:discard (get-runner))) "Can't target Inti; matches Runner faction")
    (click-card state :corp (get-resource state 0))
    (is (= 1 (count (:discard (get-runner)))) "Caldera trashed")))
