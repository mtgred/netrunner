(ns game-test.cards.identities.liza-talking-thunder-prominent-legislator
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest liza-talking-thunder-prominent-legislator
  ;; Liza Talking Thunder: Prominent Legislator
  (do-game
    (new-game {:runner {:id "Liza Talking Thunder: Prominent Legislator"
                        :deck [(qty "Sure Gamble" 7)]}})
    (take-credits state :corp)
    (run-empty-server state "R&D")
    (is (= 7 (count (:hand (get-runner)))) "Drew 2 cards from successful run on Archives")
    (is (= 1 (:tag (get-runner))) "Took 1 tag from successful run on Archives")))
