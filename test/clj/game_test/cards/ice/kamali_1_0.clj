(ns game-test.cards.ice.kamali-1-0
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest kamali-1-0
  ;; Kamali 1.0
  (do-game
    (new-game {:corp {:deck ["Kamali 1.0"]}
               :runner {:deck ["Astrolabe" "Decoy"
                               "Cache" "Hedge Fund"]}})
    (play-from-hand state :corp "Kamali 1.0" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (play-from-hand state :runner "Decoy")
    (play-from-hand state :runner "Cache")
    (let [kamali (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp kamali)
      (card-subroutine state :corp kamali 0)
      (is (zero? (:brain-damage (get-runner))) "Runner starts with 0 brain damage")
      (click-prompt state :runner "Take 1 brain damage")
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
      (card-subroutine state :corp kamali 1)
      (is (empty? (:discard (get-runner))) "Runner starts with no discarded cards")
      (click-prompt state :runner "Trash an installed piece of hardware")
      (click-card state :runner (get-hardware state 0))
      (is (empty? (get-hardware state)) "Astrolabe trashed")
      (is (= 1 (count (:discard (get-runner)))) "Runner trashed 1 card")
      (card-subroutine state :corp kamali 2)
      (is (= 1 (count (:discard (get-runner)))) "Runner starts with 1 discarded card")
      (click-prompt state :runner "Trash an installed program")
      (click-card state :runner (get-program state 0))
      (is (empty? (get-program state)) "Cache trashed")
      (is (= 2 (count (:discard (get-runner)))) "Runner trashed 1 card"))))
