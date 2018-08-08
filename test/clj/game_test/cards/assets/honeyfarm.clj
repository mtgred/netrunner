(ns game-test.cards.assets.honeyfarm
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest honeyfarm
  ;; Honeyfarm - lose one credit on access
  (do-game
    (new-game {:corp {:deck [(qty "Honeyfarm" 3)]}})
    (trash-from-hand state :corp "Honeyfarm")
    (play-from-hand state :corp "Honeyfarm" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= 4 (:credit (get-runner))))
    (run-empty-server state "Archives")
    (is (= 3 (:credit (get-runner))))
    (run-empty-server state "HQ")
    (is (= 2 (:credit (get-runner))))))
