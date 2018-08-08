(ns game-test.cards.upgrades.forced-connection
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest forced-connection
  ;; Forced Connection - ambush, trace(3) give the runner 2 tags
  (do-game
    (new-game {:corp {:deck [(qty "Forced Connection" 3)]}})
    (starting-hand state :corp ["Forced Connection" "Forced Connection"])
    (play-from-hand state :corp "Forced Connection" "New remote")
    (take-credits state :corp)
    (is (zero? (:tag (get-runner))) "Runner starts with 0 tags")
    (run-empty-server state :remote1)
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (is (= 2 (:tag (get-runner))) "Runner took two tags")
    (run-empty-server state "Archives")
    (is (= 2 (:tag (get-runner))) "Runner doesn't take tags when accessed from Archives")
    (run-empty-server state "HQ")
    (click-prompt state :corp "0")
    (click-prompt state :runner "3")
    (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash
    (is (= 2 (:tag (get-runner))) "Runner doesn't take tags when trace won")))
