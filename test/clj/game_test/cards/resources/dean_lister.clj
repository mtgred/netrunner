(ns game-test.cards.resources.dean-lister
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dean-lister
  ;; Basic test
  (do-game
    (new-game {:runner {:deck ["Dean Lister" "Faust" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Dean Lister")
    (play-from-hand state :runner "Faust")
    (run-on state :archives)
    (let [faust (get-program state 0)
          dean (get-resource state 0)]
      (is (= 2 (:current-strength faust)) "Faust at 2 strength")
      (is (zero? (-> (get-runner) :discard count)) "Dean Lister not discarded yet")
      (card-ability state :runner dean 0)
      (click-card state :runner faust)
      (is (= 1 (-> (get-runner) :discard count)) "Dean Lister trashed to use its abilitiy")
      (is (= 5 (:current-strength (refresh faust))) "Faust at 5 strength (2 base + 3 from Dean)")
      (card-ability state :runner faust 1) ;boost by 2
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 6 (:current-strength (refresh faust))) "Faust at 6 strength (2 base + 2 from Dean + 2 from boost)")
      (run-jack-out state)
      (is (= 2 (:current-strength (refresh faust))) "Dean Lister effect ends after run"))))
