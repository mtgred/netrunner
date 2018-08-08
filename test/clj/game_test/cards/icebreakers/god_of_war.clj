(ns game-test.cards.icebreakers.god-of-war
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest god-of-war
  ;; God of War - Take 1 tag to place 2 virus counters
  (do-game
    (new-game {:runner {:deck ["God of War"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "God of War")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [gow (get-program state 0)]
      (card-ability state :runner gow 2)
      (is (= 1 (:tag (get-runner))))
      (is (= 2 (get-counters (refresh gow) :virus)) "God of War has 2 virus counters"))))
