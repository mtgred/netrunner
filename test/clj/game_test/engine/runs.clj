(ns game-test.engine.runs
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ^:test-refresh/focus run-timing
  (testing "with no ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (take-credits state :corp)
      (run-on state :archives)
      (is (= :approach-server (:phase (:run @state))) "No approach ice as no ice is installed")
      (click-prompt state :runner "Continue")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Continue")
      (is (nil? (:run @state)))))
  (testing "with an ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (:run @state))
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (core/rez state :corp (get-ice state :remote1 0))
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Done")
      (is (nil? (:run @state)) "ice Wall subroutine ends the run")))
  (testing "with ice and a breaker"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state :rd)
      (is (:run @state))
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (core/rez state :corp (get-ice state :rd 0))
      (click-prompt state :corp "Done")
      (is (= :encounter-ice (:phase (:run @state))) "Corp rez ice window")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Done")
      (is (= :approach-server (:phase (:run @state))) "Approach server jack out prompt")
      (click-prompt state :runner "Continue")
      (is (= :approach-server (:phase (:run @state))) "Approach server (corp-phase-43) rez and paid ability window")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Continue")
      (click-prompt state :runner "No action")
      (is (nil? (:run @state)))))
  (testing "with ice with on-encounter effect"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Tollbooth"]
                        :credits 10}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (let [tollbooth (get-ice state :remote1 0)]
        (core/rez state :corp tollbooth)
        (take-credits state :corp))
      (run-on state :remote1)
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))))
      (let [credits (:credit (get-runner))]
        (click-prompt state :corp "Done")
        (click-prompt state :runner "Done")
        (is (= (- credits 3) (:credit (get-runner))) "Tollbooth forced the runner to pay 3"))
      (is (= :encounter-ice (:phase (:run @state))))
      (click-prompt state :runner "Done")
      (is (nil? (:run @state)))))
  (testing "with paid ability before ice with on-encounter effect"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Tollbooth"]
                        :credits 10}
                 :runner {:deck ["Corroder"]
                          :hand ["Self-modifying Code"]
                          :credits 4}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Self-modifying Code")
      (run-on state :remote1)
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))))
      (let [credits (:credit (get-runner))]
        (core/rez state :corp (get-ice state :remote1 0))
        (click-prompt state :corp "Done")
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner "Corroder")
        (is (zero? (:credit (get-runner))) "Can't afford Tollbooth")
        (is (= :approach-ice (:phase (:run @state))) "Haven't left the approach window yet")
        (click-prompt state :runner "Done")
        (is (nil? (:run @state))))))
  (testing "with bypass"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (core/rez state :corp iw)
        (take-credits state :corp))
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "Server 1")
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Done")
      (is (= :approach-server (:phase (:run @state))) "Inside Job has bypassed Ice Wall")
      (click-prompt state :runner "Jack out")))
  (testing "with bypass vs cannot be bypassed"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Guard"]}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Guard" "New remote")
      (let [guard (get-ice state :remote1 0)]
        (core/rez state :corp guard)
        (take-credits state :corp))
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "Server 1")
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Done")
      (is (= :encounter-ice (:phase (:run @state))) "Inside Job hasn't bypassed Guard")
      (click-prompt state :runner "Done")
      (is (nil? (:run @state)))))
  (testing "with bypass vs ice with on-encounter effect"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (let [tollbooth (get-ice state :remote1 0)]
        (core/rez state :corp tollbooth)
        (take-credits state :corp))
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "Server 1")
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (click-prompt state :corp "Done")
      (click-prompt state :runner "Done")
      (is (= :approach-server (:phase (:run @state))) "Inside Job has bypassed Ice Wall")
      (click-prompt state :runner "Jack out")
      (is (nil? (:run @state)))))
  (testing "with paid ability that ends the run during encounter (Border Control)"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Border Control" "Tollbooth"]
                        :credits 20}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (core/rez state :corp (get-ice state :remote1 0))
      (play-from-hand state :corp "Border Control" "Server 1")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (:run @state))
      (click-prompt state :runner "Continue")
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (let [credits (:credit (get-runner))]
        (core/rez state :corp (get-ice state :remote1 1))
        (card-ability state :corp (get-ice state :remote1 1) 0)
        (click-prompt state :corp "Done")
        (is (nil? (:run @state)) "Pressing Done properly handles the ended run")
        (is (= credits (:credit (get-runner))) "Runner shouldn't lose any credits to Tollbooth"))))
  )
