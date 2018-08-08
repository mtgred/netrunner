(ns game-test.cards.resources.eden-shard
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest eden-shard
  ;; Eden Shard - Install from Grip in lieu of accessing R&D; trash to make Corp draw 2
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Eden Shard"]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (run-on state :rd)
      (core/no-action state :corp nil)
      (play-from-hand state :runner "Eden Shard")
      (is (= 5 (:credit (get-runner))) "Eden Shard installed for 0c")
      (is (not (:run @state)) "Run is over")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 3 (count (:hand (get-corp)))) "Corp drew 2 cards")
      (is (= 1 (count (:discard (get-runner)))) "Eden Shard trashed")))
  (testing "Do not install when accessing cards"
    (do-game
      (new-game {:runner {:deck ["Eden Shard"]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (is (= 1 (count (:hand (get-corp)))))
      (run-empty-server state :rd)
      (play-from-hand state :runner "Eden Shard")
      (is (not (get-resource state 0)) "Eden Shard not installed")
      (is (= 1 (count (:hand (get-runner)))) "Eden Shard not installed"))))
