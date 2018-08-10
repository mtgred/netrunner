(ns game-test.cards.hardware.respirocytes
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest respirocytes
  (testing "Should draw multiple cards when multiple respirocytes are in play"
    (do-game
      (new-game {:runner {:deck [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"])
      (dotimes [_ 2]
        (play-from-hand state :runner "Respirocytes"))
      (is (= 2 (count (:discard (get-runner)))) "2 damage done")
      (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")))
  (testing "Respirocytes should not trigger after being trashed (issue #3699)"
    (do-game
      (new-game {:runner {:deck ["Respirocytes" (qty "Sure Gamble" 20)]}})
      (starting-hand state :runner ["Respirocytes" "Sure Gamble"])
      (take-credits state :corp)
      (play-from-hand state :runner "Respirocytes")
      (is (= 1 (-> (get-runner) :discard count)) "Took 1 damage from Respirocytes")
      (is (= 1 (-> (get-runner) :hand count)) "Drew 1 from Respirocytes")
      (let [respirocytes (get-hardware state 0)]
        (is (= 1 (get-counters (refresh respirocytes) :power)) "Respirocytes drew once")
        (take-credits state :runner)
        (take-credits state :corp)
        (dotimes [n 2]
          (play-from-hand state :runner "Sure Gamble")
          (is (= 1 (-> (get-runner) :hand count)) "Drew 1 from Respirocytes")
          (take-credits state :runner)
          (take-credits state :corp))
        (is (= 1 (-> (get-runner) :hand count)) "1 card in hand")
        (is (zero? (-> (get-runner) :rig :hardware count)) "Respirocytes expired")
        (play-from-hand state :runner "Sure Gamble")
        (is (= 0 (-> (get-runner) :hand count))
            "Respirocytes did not trigger when trashed")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 0 (-> (get-runner) :hand count))
            "Respirocytes still does not trigger when trashed")))))
