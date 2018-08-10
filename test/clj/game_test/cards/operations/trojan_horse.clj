(ns game-test.cards.operations.trojan-horse
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest trojan-horse
  ;; Trojan Horse
  (do-game
    (new-game {:corp {:deck ["Trojan Horse" "Dedicated Response Team"]}
               :runner {:deck ["Wyrm"]}})
    (play-from-hand state :corp "Dedicated Response Team" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Wyrm")
    (run-empty-server state :remote1)
    (take-credits state :runner)
    (is (zero? (-> (get-runner) :discard count)) "Runner should start with 0 cards in heap")
    (play-from-hand state :corp "Trojan Horse")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-program state 0))
    (is (= 1 (-> (get-runner) :discard count)) "Wyrm should be in heap after Runner loses Trojan Horse trace")))
