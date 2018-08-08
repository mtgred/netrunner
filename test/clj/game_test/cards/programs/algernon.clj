(ns game-test.cards.programs.algernon
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest algernon
  ;; Algernon - pay 2 credits to gain a click, trash if no successful run
  (testing "Use, successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (run-successful state)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed")))
  (testing "Use, no successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "Yes")
      (is (= 6 (:credit (get-runner))) "Runner pays 2 credits")
      (is (= 5 (:click (get-runner))) "Runner gains 1 click")
      (run-on state "Archives")
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (is (= 1 (count (:discard (get-runner)))) "Algernon trashed")
      (is (empty? (get-program state)) "No programs installed")))
  (testing "Not used, no successful run"
    (do-game
      (new-game {:runner {:deck ["Algernon"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Algernon")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (= 8 (:credit (get-runner))) "Runner starts with 8 credits")
      (is (= 4 (:click (get-runner))) "Runner starts with 4 clicks")
      (click-prompt state :runner "No")
      (is (= 8 (:credit (get-runner))) "No credits spent")
      (is (= 4 (:click (get-runner))) "No clicks gained")
      (run-on state "Archives")
      (core/jack-out state :runner nil)
      (take-credits state :runner)
      (is (empty? (:discard (get-runner))) "No cards trashed")
      (is (= "Algernon" (:title (get-program state 0))) "Algernon still installed"))))
