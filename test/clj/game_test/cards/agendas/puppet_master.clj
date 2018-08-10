(ns game-test.cards.agendas.puppet-master
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest puppet-master
  ;; Puppet Master - game progresses if no valid targets. Issue #1661.
  (do-game
    (new-game {:corp {:deck ["Puppet Master"]}})
    (play-and-score state "Puppet Master")
    (take-credits state :corp)
    (run-empty-server state :archives)
    (click-prompt state :corp "Done")
    (is (empty? (:prompt (get-runner))) "Runner's waiting prompt resolved")))
