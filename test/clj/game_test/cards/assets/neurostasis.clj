(ns game-test.cards.assets.neurostasis
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest neurostasis
  ;; Neurostasis - ambush, shuffle cards into the stack
  (do-game
    (new-game {:corp {:deck ["Neurostasis"]}
               :runner {:deck [(qty "Cache" 3)]}})
    (play-from-hand state :corp "Neurostasis" "New remote")
    (let [neuro (get-content state :remote1 0)]
      ;; Single advance Neurostasis
      (core/advance state :corp {:card (refresh neuro)})
      (take-credits state :corp)
      ;; Run on Neurostasis with 3 programs
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (play-from-hand state :runner "Cache")
      (run-empty-server state "Server 1")
      (is (= 5 (:credit (get-corp))) "Corp starts with 5 credits")
      (click-prompt state :corp "Yes")
      ;; Corp can shuffle one program
      (click-card state :corp (get-program state 1))
      ;; There should be two Caches left
      (is (= 2 (:credit (get-corp))) "Spent 3 credits to fire ambush")
      (is (= 2 (count (get-program state))) "Removed one installed program")
      (is (= 1 (count (:deck (get-runner)))) "Shuffled one program into the stack")
      (take-credits state :runner)
      (core/advance state :corp {:card (refresh neuro)})
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (is (= 3 (:credit (get-corp))) "Corp starts with 3 credits")
      (click-prompt state :corp "Yes")
      ;; Corp can shuffle two programs
      (click-card state :corp (get-program state 1))
      (click-card state :corp (get-program state 0))
      (is (= 0 (:credit (get-corp))) "Spent 3 credits to fire ambush")
      (is (empty? (get-program state)) "Removed one installed program")
      (is (= 3 (count (:deck (get-runner)))) "Shuffled two programs into the stack"))))
