(ns game-test.cards.resources.officer-frank
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest officer-frank
  ;; Officer Frank - meat damage to trash 2 from HQ
  (do-game
    (new-game {:corp {:deck ["Swordsman" (qty "Hedge Fund" 2)]}
               :runner {:deck ["Officer Frank" "Skulljack" (qty "Respirocytes" 4)]}})
    (play-from-hand state :corp "Swordsman" "Archives")
    (take-credits state :corp)
    (starting-hand state :runner ["Officer Frank" "Skulljack" "Respirocytes" "Respirocytes" "Respirocytes" "Respirocytes"])
    (play-from-hand state :runner "Officer Frank")
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
    (play-from-hand state :runner "Skulljack")
    (is (= 3 (count (:hand (get-runner)))) "Took 1 brain damage")
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
    (let [sm (get-ice state :archives 0)]
      (run-on state :archives)
      (core/rez state :corp sm)
      (card-subroutine state :corp sm 0)
      (run-jack-out state))
    (is (= 2 (count (:hand (get-runner)))) "Took 1 net damage")
    (card-ability state :runner (get-resource state 0) 0)
    (is (zero? (count (:discard (get-corp)))) "Nothing discarded from HQ")
    (play-from-hand state :runner "Respirocytes")
    (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")
    (card-ability state :runner (get-resource state 0) 0)
    (is (= 2 (count (:discard (get-corp)))) "Two cards trashed from HQ")))
