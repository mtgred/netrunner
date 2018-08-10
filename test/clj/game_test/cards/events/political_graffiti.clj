(ns game-test.cards.events.political-graffiti
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest political-graffiti
  ;; Political Graffiti - swapping with Turntable works / purging viruses restores points
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Breaking News" "Chronos Project"]}
                 :runner {:deck ["Turntable" "Political Graffiti"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (:agenda-point (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Political Graffiti")
      (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (click-card state :runner (find-card "Breaking News" (:scored (get-corp))))
      (is (zero? (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
      (play-from-hand state :runner "Turntable")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (let [tt (get-hardware state 0)]
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Breaking News" (:scored (get-corp))))
        (is (= 1 (:agenda-point (get-corp))))
        (is (zero? (:agenda-point (get-runner))))
        (take-credits state :runner)
        (core/purge state :corp)
        (is (= 1 (:agenda-point (get-corp))))
        (is (= 1 (:agenda-point (get-runner)))))))
  (testing "forfeiting agenda with Political Graffiti does not refund double points. Issue #2765"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" "Sacrifice"]}
                 :runner {:deck ["Political Graffiti"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (:agenda-point (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Political Graffiti")
      (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (click-card state :runner (find-card "Hostile Takeover" (:scored (get-corp))))
      (is (zero? (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
      (take-credits state :runner)
      (play-from-hand state :corp "Sacrifice")
      (click-card state :corp (get-scored state :corp 0))
      (is (zero? (:agenda-point (get-corp))) "Forfeiting agenda did not refund extra agenda points ")
      (is (= 1 (count (:discard (get-runner)))) "Political Graffiti is in the Heap"))))
