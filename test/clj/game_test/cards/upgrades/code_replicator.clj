(ns game-test.cards.upgrades.code-replicator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest code-replicator
  ;; Code Replicator - trash to make runner approach passed (rezzed) ice again
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) "Code Replicator"]}})
    (core/gain state :corp :click 1)
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Code Replicator" "HQ")
    (take-credits state :corp)
    (run-on state "HQ")
    (is (= 3 (:position (get-in @state [:run]))) "Initial position outermost Ice Wall")
    (let [cr (get-content state :hq 0)
          i1 (get-ice state :hq 0)
          i2 (get-ice state :hq 1)
          i3 (get-ice state :hq 2)]
      (core/rez state :corp cr)
      (is (= 5 (:credit (get-corp))))
      (core/rez state :corp i3)
      (run-continue state)
      (is (= 2 (:position (get-in @state [:run]))) "Passed Ice Wall")
      (card-ability state :corp cr 0)
      (is (= 3 (:position (get-in @state [:run]))) "Runner approaching previous Ice Wall")
      (is (empty? (get-content state :hq))
          "Code Replicatior trashed from root of HQ"))))
