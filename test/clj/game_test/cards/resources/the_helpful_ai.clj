(ns game-test.cards.resources.the-helpful-ai
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest the-helpful-ai
  ;; The Helpful AI - +1 link; trash to give an icebreaker +2 str until end of turn
  (do-game
    (new-game {:runner {:deck ["The Helpful AI" "Corroder"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Helpful AI")
    (is (= 1 (:link (get-runner))) "Gained 1 link")
    (play-from-hand state :runner "Corroder")
    (let [corr (get-program state 0)]
      (card-ability state :runner (get-resource state 0) 0)
      (click-card state :runner corr)
      (is (= 4 (:current-strength (refresh corr))) "Corroder has +2 strength")
      (is (= 1 (count (:discard (get-runner)))) "Helpful AI trashed")
      (is (zero? (:link (get-runner))))
      (take-credits state :runner)
      (is (= 2 (:current-strength (refresh corr))) "Corroder back to default strength"))))
