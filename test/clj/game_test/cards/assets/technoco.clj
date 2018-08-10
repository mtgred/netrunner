(ns game-test.cards.assets.technoco
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest technoco
  ;; TechnoCo - Increase program / hardware / virtual cost by 1 and gain 1 when they are installed
  (do-game
    (new-game {:corp {:deck ["TechnoCo"]}
               :runner {:deck ["Misdirection"       ;; 0 cost program
                               "Bookmark"           ;; 0 cost hardware
                               "Ice Analyzer"       ;; 0 cost virtual resource
                               "Fall Guy"]}})        ;; 0 cost non-virtual resource
    (play-from-hand state :corp "TechnoCo" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (is (= 5 (:credit (get-corp))) "Corp at 5 credits")
    (is (= 5 (:credit (get-runner))) "Runner at 5 credits")
    (play-from-hand state :runner "Misdirection")
    (is (= 6 (:credit (get-corp))) "Corp gained a credit")
    (is (= 4 (:credit (get-runner))) "Runner spent an extra credit")
    (play-from-hand state :runner "Bookmark")
    (is (= 7 (:credit (get-corp))) "Corp gained a credit")
    (is (= 3 (:credit (get-runner))) "Runner spent an extra credit")
    (play-from-hand state :runner "Ice Analyzer")
    (is (= 8 (:credit (get-corp))) "Corp gained a credit")
    (is (= 2 (:credit (get-runner))) "Runner spent an extra credit")
    (play-from-hand state :runner "Fall Guy")
    (is (= 8 (:credit (get-corp))) "Corp did not gain a credit")
    (is (= 2 (:credit (get-runner))) "Runner did not spend an extra credit")))
