(ns game-test.cards.events.modded
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest modded
  ;; Modded - Install a program or piece of hardware at a 3 credit discount
  (do-game
    (new-game {:runner {:deck [(qty "Modded" 2)
                               "HQ Interface"
                               "Nerve Agent"
                               "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Modded")
    (click-card state :runner (find-card "Earthrise Hotel" (:hand (get-runner))))
    (is (empty? (get-resource state)) "Can't install resources with Modded")
    (click-card state :runner (find-card "HQ Interface" (:hand (get-runner))))
    (is (= 1 (count (get-hardware state))) "Installed HQ Interface")
    (is (= 4 (:credit (get-runner))) "Paid 1 credit instead of 4")
    (play-from-hand state :runner "Modded")
    (click-card state :runner (find-card "Nerve Agent" (:hand (get-runner))))
    (is (= 1 (count (get-program state))) "Installed Nerve Agent")
    (is (= 4 (:credit (get-runner))) "Paid 0 credits")))
