(ns game.engine.runs-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.utils-test :refer :all]
            [game.macros-test :refer :all]
            [clojure.test :refer :all]))

(deftest run-timing
  (testing "with no ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (take-credits state :corp)
      (run-on state :archives)
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
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (core/rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (fire-subs state (get-ice state :remote1 0))
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
      (is (= :approach-ice (:phase (:run @state))))
      (core/rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))))
      (card-ability state :runner (get-program state 0) 0) ; Icebreaker
      (click-prompt state :runner "End the run")
      (run-continue state)
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
      (take-credits state :corp)
      (run-on state :remote1)
      (core/rez state :corp (get-ice state :remote1 0))
      (is (= :approach-ice (:phase (:run @state))))
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (is (= (- credits 3) (:credit (get-runner))) "Tollbooth forced the runner to pay 3"))
      (is (= :encounter-ice (:phase (:run @state))))
      (fire-subs state (get-ice state :remote1 0))
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
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (run-continue state)
      (is (= :approach-server (:phase (:run @state))) "Inside Job has bypassed Ice Wall")
      (run-jack-out state)))
  (testing "with bypass vs cannot be bypassed"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Guard"]}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Guard" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "Server 1")
      (core/rez state :corp (get-ice state :remote1 0))
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Inside Job hasn't bypassed Guard")
      (fire-subs state (get-ice state :remote1 0))
      (is (nil? (:run @state)))))
  (testing "with bypass vs ice with on-encounter effect"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Tollbooth"]
                        :credits 10}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Tollbooth" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "Server 1")
      (core/rez state :corp (get-ice state :remote1 0))
      (is (= :approach-ice (:phase (:run @state))) "Inside Job hasn't done the effect yet")
      (let [credits (:credit (get-runner))]
        (run-continue state)
        (is (= :approach-server (:phase (:run @state))) "Inside Job has bypassed Tollbooth")
        (is (= credits (:credit (get-runner)))))
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
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (let [credits (:credit (get-runner))]
        (core/rez state :corp (get-ice state :remote1 1))
        (card-ability state :corp (get-ice state :remote1 1) 0)
        (is (nil? (:run @state)) "Pressing Done properly handles the ended run")
        (is (= credits (:credit (get-runner))) "Runner shouldn't lose any credits to Tollbooth")))))

(deftest buffered-continue
  (testing "Buffered continue on approaching ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (is (= :approach-ice (:phase (:run @state))) "Runner in approach on ice")
        (is (not (:no-action (:run @state))) "no-action is not set yet")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Still in approach on ice")
        (is (= :runner (:no-action (:run @state))) "Runner pressed Continue button")
        (core/no-action state :corp nil)
        (is (= :approach-server (:phase (:run @state))) "Corp pressed Continue button, now approaching server")
        (is (not (:no-action (:run @state))) "no-action is reset")))
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (is (= :approach-ice (:phase (:run @state))) "Runner in approach on ice")
        (is (not (:no-action (:run @state))) "no-action is not set yet")
        (core/no-action state :corp nil)
        (is (= :approach-ice (:phase (:run @state))) "Still in approach on ice")
        (is (= :corp (:no-action (:run @state))) "Corp pressed Continue button")
        (core/continue state :runner nil)
        (is (= :approach-server (:phase (:run @state))) "Runner pressed Continue button, now approaching server")
        (is (not (:no-action (:run @state))) "no-action is reset"))))
  (testing "Buffered continue on encountering ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (core/rez state :corp iw)
        (run-continue state)
        (is (= :encounter-ice (:phase (:run @state))) "Runner in encounter with ice")
        (is (not (:no-action (:run @state))) "no-action is not set yet")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Still in encounter with ice")
        (is (= :runner (:no-action (:run @state))) "Runner pressed Continue button")
        (core/no-action state :corp nil)
        (is (= :approach-server (:phase (:run @state))) "Corp pressed Continue button, now approaching server")
        (is (not (:no-action (:run @state))) "no-action is reset"))))
  (testing "Buffered continue on encountering ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (core/rez state :corp iw)
        (run-continue state)
        (is (= :encounter-ice (:phase (:run @state))) "Runner in encounter with ice")
        (is (not (:no-action (:run @state))) "no-action is not set yet")
        (core/no-action state :corp nil)
        (is (= :encounter-ice (:phase (:run @state))) "Still in encounter with ice")
        (is (= :corp (:no-action (:run @state))) "Corp pressed Continue button")
        (core/continue state :runner nil)
        (is (= :approach-server (:phase (:run @state))) "Runner pressed Continue button, now approaching server")
        (is (not (:no-action (:run @state))) "no-action is reset")))))
