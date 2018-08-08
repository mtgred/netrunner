(ns game-test.cards.events.freedom-through-equality
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest freedom-through-equality
  ;; Move Freedom Through Equality to runner score on another steal
  ;; Check only one current used
  (do-game
    (new-game {:corp {:deck [(qty "Project Beale" 2)]}
               :runner {:deck ["Street Peddler" (qty "\"Freedom Through Equality\"" 3) "Sure Gamble"]}})
    (starting-hand state :runner ["Street Peddler"
                                  "\"Freedom Through Equality\""
                                  "\"Freedom Through Equality\""
                                  "Sure Gamble"])
    (play-from-hand state :corp "Project Beale" "New remote")
    (play-from-hand state :corp "Project Beale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Freedom Through Equality not moved from Peddler to score area")
    (take-credits state :runner)
    (take-credits state :corp)
    (run-empty-server state "Server 2")
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "\"Freedom Through Equality\"")
    (play-from-hand state :runner "\"Freedom Through Equality\"")
    (click-prompt state :runner "Steal")
    (is (= 3 (count (:scored (get-runner)))) "Freedom Through Equality moved to score area")
    (is (= 5 (:agenda-point (get-runner))) "Freedom Through Equality for 1 agenda point")))
