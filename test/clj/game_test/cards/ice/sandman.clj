(ns game-test.cards.ice.sandman
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sandman
  ;; Sandman - add an installed runner card to the grip
  (do-game
    (new-game {:corp {:deck ["Sandman"]}
               :runner {:deck ["Inti" "Scrubber"]}})
    (play-from-hand state :corp "Sandman" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "Scrubber")
    (is (zero? (count (:hand (get-runner)))) "Runner's hand is empty")
    (run-on state "HQ")
    (let [sand (get-ice state :hq 0)]
      (core/rez state :corp (refresh sand))
      (card-subroutine state :corp (refresh sand) 0)
      (click-card state :corp (find-card "Inti" (get-in (get-runner) [:rig :program])))
      (is (= 1 (count (:hand (get-runner)))) "Runner has 1 card in hand")
      (card-subroutine state :corp (refresh sand) 0)
      (click-card state :corp (find-card "Scrubber" (get-in (get-runner) [:rig :resource])))
      (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards in hand")
      (card-subroutine state :corp (refresh sand) 0)
      (is (empty? (:prompt (get-corp))) "Sandman doesn't fire if no installed cards"))))
