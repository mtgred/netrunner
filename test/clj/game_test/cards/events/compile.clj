(ns game-test.cards.events.compile
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^{:card-title "compile"}
  compile-test
  ;; Compile - Make a run, and install a program for free which is shuffled back into stack
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Compile" "Gordian Blade"]}})
      (starting-hand state :runner ["Compile"])
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (click-prompt state :runner "OK")  ; notification that Compile must be clicked to install
      (let [compile-card (first (get-in @state [:runner :play-area]))]
        (card-ability state :runner compile-card 0)
        (click-prompt state :runner "Stack")
        (click-prompt state :runner (find-card "Gordian Blade" (:deck (get-runner))))
        (is (:installed (get-program state 0)) "Gordian Blade should be installed"))
      (let [deck (count (:deck (get-runner)))]
        (run-jack-out state)
        (is (= (+ 1 deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
        (is (nil? (get-program state 0))))))
  (testing "with Self-modifying Code, neither SMC nor other card should be shuffled back in"
    (do-game
      (new-game {:runner {:deck ["Compile" "Clone Chip"
                                 (qty "Self-modifying Code" 3)]}})
      (starting-hand state :runner ["Compile" "Clone Chip"])
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Clone Chip")
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (click-prompt state :runner "OK")  ; notification that Compile must be clicked to install
      (let [compile-card (first (get-in @state [:runner :play-area]))
            clone-chip (get-hardware state 0)]
        (card-ability state :runner compile-card 0)
        (click-prompt state :runner "Stack")
        (click-prompt state :runner (find-card "Self-modifying Code" (:deck (get-runner))))
        (let [smc (get-program state 0)]
          (card-ability state :runner smc 0)
          (click-prompt state :runner (find-card "Self-modifying Code" (:deck (get-runner))))
          (card-ability state :runner clone-chip 0)
          (click-card state :runner (find-card "Self-modifying Code" (:discard (get-runner))))))
      (let [deck (count (:deck (get-runner)))]
        (run-jack-out state)
        (is (= deck (count (:deck (get-runner)))) "No card was shuffled back into the stack"))))
  (testing "vs ending the run via corp action. #3639"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Compile" "Gordian Blade"]}})
      (starting-hand state :runner ["Compile"])
      (play-from-hand state :corp "Ice Wall" "Archives")
      (let [iw (get-ice state :archives 0)]
        (core/rez state :corp iw)
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (play-from-hand state :runner "Compile")
        (click-prompt state :runner "Archives")
        (click-prompt state :runner "OK")  ; notification that Compile must be clicked to install
        (let [compile-card (first (get-in @state [:runner :play-area]))]
          (card-ability state :runner compile-card 0)
          (click-prompt state :runner "Stack")
          (click-prompt state :runner (find-card "Gordian Blade" (:deck (get-runner))))
          (is (:installed (get-program state 0)) "Gordian Blade should be installed"))
        (let [deck (count (:deck (get-runner)))]
          (card-subroutine state :corp iw 0)
          (is (= (+ 1 deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
          (is (nil? (get-program state 0))))))))
