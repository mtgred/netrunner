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
      (run-next-phase state)
      (run-continue state)
      (run-successful state)
      (is (nil? (:run @state)))))
  (testing "with an ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (:run @state))
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (core/rez state :corp (get-ice state :remote1 0))
      (core/no-action state :corp nil)
      (core/resolve-unbroken-subs! state :corp (get-ice state :remote1 0))
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))))
      (core/rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))))
      (card-ability state :runner (get-program state 0) 0) ; Icebreaker
      (click-prompt state :runner "End the run")
      (run-continue state)
      (run-next-phase state)
      (is (= :approach-server (:phase (:run @state))))
      (run-successful state)
      (click-prompt state :runner "No action") ; Access Hedge Fund
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))))
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (is (= (- credits 3) (:credit (get-runner))) "Tollbooth forced the runner to pay 3"))
      (is (= :encounter-ice (:phase (:run @state))))
      (core/resolve-unbroken-subs! state :corp (get-ice state :remote1 0))
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))))
      (let [credits (:credit (get-runner))]
        (core/rez state :corp (get-ice state :remote1 0))
        (card-ability state :runner (get-program state 0) 0)
        (click-prompt state :runner "Corroder")
        (is (zero? (:credit (get-runner))) "Can't afford Tollbooth")
        (is (= :approach-ice (:phase (:run @state))) "Haven't left the approach window yet")
        (run-continue state)
        (is (nil? (:run @state)) "Can't afford Tollbooth, so run ends"))))
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (run-continue state)
      (is (= :pass-ice (:phase (:run @state))) "Inside Job has marked the bypass")
      (run-next-phase state)
      (is (= :approach-server (:phase (:run @state))) "Inside Job has bypassed Ice Wall")
      (run-jack-out state)))
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Inside Job hasn't bypassed Guard")
      (core/resolve-unbroken-subs! state :corp (get-ice state :remote1 0))
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (run-continue state)
      (is (= :pass-ice (:phase (:run @state))) "Inside Job has bypassed Tollbooth")
      (run-next-phase state)
      (run-jack-out state)
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
      (run-next-phase state)
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (let [credits (:credit (get-runner))]
        (core/rez state :corp (get-ice state :remote1 1))
        (card-ability state :corp (get-ice state :remote1 1) 0)
        (is (nil? (:run @state)) "Pressing Done properly handles the ended run")
        (is (= credits (:credit (get-runner))) "Runner shouldn't lose any credits to Tollbooth"))))
  )
