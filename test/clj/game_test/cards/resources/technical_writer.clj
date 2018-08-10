(ns game-test.cards.resources.technical-writer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest technical-writer
  ;; Technical Writer - Gain 1c per program/hardware install; click/trash to take all credits
  (do-game
    (new-game {:runner {:deck ["Technical Writer" (qty "Faerie" 2)
                               "Vigil" "Same Old Thing"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "Technical Writer")
    (let [tw (get-resource state 0)]
      (play-from-hand state :runner "Faerie")
      (is (= 1 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :runner "Faerie")
      (is (= 2 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :runner "Vigil")
      (is (= 3 (get-counters (refresh tw) :credit)) "Tech Writer gained 1c")
      (play-from-hand state :runner "Same Old Thing")
      (is (= 3 (get-counters (refresh tw) :credit)) "No credit gained for resource install")
      (card-ability state :runner tw 0)
      (is (= 6 (:credit (get-runner))) "Gained 3 credits")
      (is (zero? (:click (get-runner))) "Spent 1 click")
      (is (= 1 (count (:discard (get-runner)))) "Technical Writer trashed"))))
