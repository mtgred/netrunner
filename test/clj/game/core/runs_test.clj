(ns game.core.runs-test
  (:require [game.core :as core]
            [game.core-test :refer :all]
            [game.core.card :refer :all]
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
      (rez state :corp (get-ice state :remote1 0))
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
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))))
      (card-ability state :runner (get-program state 0) 0) ; Icebreaker
      (click-prompt state :runner "End the run")
      (run-continue state)
      (is (= :approach-server (:phase (:run @state))))
      (run-continue state)
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
      (rez state :corp (get-ice state :remote1 0))
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
        (rez state :corp (get-ice state :remote1 0))
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
        (rez state :corp iw)
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
      (rez state :corp (get-ice state :remote1 0))
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
      (rez state :corp (get-ice state :remote1 0))
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
      (rez state :corp (get-ice state :remote1 0))
      (play-from-hand state :corp "Border Control" "Server 1")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (:run @state))
      (is (= :approach-ice (:phase (:run @state))) "Corp rez ice window")
      (let [credits (:credit (get-runner))]
        (rez state :corp (get-ice state :remote1 1))
        (card-ability state :corp (get-ice state :remote1 1) 0)
        (is (nil? (:run @state)) "Pressing Done properly handles the ended run")
        (is (= credits (:credit (get-runner))) "Runner shouldn't lose any credits to Tollbooth"))))
  (testing "trashing a solo ice on an empty server #4940"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Corroder" "Knifed"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Knifed")
      (click-prompt state :runner "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (is (nil? (get-ice state :remote1 0)) "Ice Wall is trashed")
      (is (nil? (:run @state)) "Ice Wall is trashed, so run has been ended")))
  (testing "Redirection updates current-ice. #5047"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Vanilla" "Ice Wall"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (play-from-hand state :corp "Ice Wall" "Archives")
      (rez state :corp (get-ice state :archives 0))
      (take-credits state :corp)
      (run-on state :hq)
      (is (= "Vanilla" (:title (:ice (ffirst (core/turn-events state :corp :approach-ice))))))
      (is (= 1 (count (core/turn-events state :corp :approach-ice))))
      (is (last-log-contains? state "Runner approaches Vanilla"))
      (core/redirect-run state :corp "Archives" :approach-ice)
      (run-next-phase state)
      (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")
      (is (= "Ice Wall" (:title (:ice (ffirst (core/turn-events state :corp :approach-ice))))))
      (is (= 2 (count (core/turn-events state :corp :approach-ice))))
      (is (last-log-contains? state "Runner approaches Ice Wall"))))
  (testing "cr 1.4 6.8.2c: any other priority window is closed normally"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Embolus" "Giordano Memorial Field" "Hostile Takeover"]
                        :credits 20}})
      (play-from-hand state :corp "Embolus" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (core/add-counter state :corp (get-content state :remote1 0) :power 4)
      (play-from-hand state :corp "Giordano Memorial Field" "New remote")
      (rez state :corp (get-content state :remote2 0))
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (run-empty-server state :remote3)
      (click-prompt state :runner "Steal")
      (changes-val-macro
        -1 (get-counters (get-content state :remote1 0) :power)
        "Embolus loses a power counter even tho GMF is resolved first and ends the run"
        (run-empty-server state :remote2)
        (is (= "Choose a trigger to resolve" (:msg (prompt-map :corp))))
        (is (= ["Embolus" "Giordano Memorial Field"] (map :title (prompt-buttons :corp))))
        (click-prompt state :corp "Giordano Memorial Field")
        (click-prompt state :runner "End the run")))))

(deftest replace-access
  (testing "'You may' only"
    (testing "and choosing replacement effect"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Hedge Fund"]}
                   :runner {:hand ["Account Siphon"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Account Siphon")
        (run-continue state)
        (is (= ["Account Siphon" "Access cards"] (prompt-buttons :runner)) "Runner can choose")
        (click-prompt state :runner "Account Siphon")
        (is (second-last-log-contains? state "Runner uses the replacement effect from Account Siphon")
            "Replacement effect is noted")
        (is (empty? (:prompt (get-runner))) "No access, no replacement effects")))
    (testing "and choosing to access cards"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Hedge Fund"]}
                   :runner {:hand ["Account Siphon"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Account Siphon")
        (run-continue state)
        (is (= ["Account Siphon" "Access cards"] (prompt-buttons :runner)) "Runner can choose")
        (click-prompt state :runner "Access cards")
        (is (second-last-log-contains? state "Runner chooses to access cards instead of use a replacement effect")
            "Not choosing replacement effect is noted")
        (is (= "You accessed Hedge Fund." (:msg (prompt-map :runner))) "Normal access prompt")
        (click-prompt state :runner "No action")
        (is (empty? (:prompt (get-runner))) "No access, no replacement effects"))))
  (testing "must replacement effects only"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:hand ["Security Testing"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "HQ")
      (run-empty-server state :hq)
      (is (empty? (:prompt (get-runner))))
      (is (second-last-log-contains? state "Runner uses the replacement effect from Security Testing")
          "Replacement effect is noted")
      (is (empty? (:prompt (get-runner))) "No access, no replacement effects")))
  (testing "'You may' and must replacement effects"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:hand ["Account Siphon" "Security Testing"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-prompt state :runner "HQ")
      (play-from-hand state :runner "Account Siphon")
      (run-continue state)
      (is (= ["Account Siphon" "Security Testing"] (prompt-buttons :runner)) "Runner can choose")
      (click-prompt state :runner "Account Siphon")
      (is (second-last-log-contains? state "Runner uses the replacement effect from Account Siphon")
          "Replacement effect is noted")
      (is (empty? (:prompt (get-runner))) "No access, no replacement effects"))))

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
        (core/continue state :corp nil)
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
        (core/continue state :corp nil)
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
        (rez state :corp iw)
        (run-continue state)
        (is (= :encounter-ice (:phase (:run @state))) "Runner in encounter with ice")
        (is (not (:no-action (:run @state))) "no-action is not set yet")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Still in encounter with ice")
        (is (= :runner (:no-action (:run @state))) "Runner pressed Continue button")
        (core/continue state :corp nil)
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
        (rez state :corp iw)
        (run-continue state)
        (is (= :encounter-ice (:phase (:run @state))) "Runner in encounter with ice")
        (is (not (:no-action (:run @state))) "no-action is not set yet")
        (core/continue state :corp nil)
        (is (= :encounter-ice (:phase (:run @state))) "Still in encounter with ice")
        (is (= :corp (:no-action (:run @state))) "Corp pressed Continue button")
        (core/continue state :runner nil)
        (is (= :approach-server (:phase (:run @state))) "Runner pressed Continue button, now approaching server")
        (is (not (:no-action (:run @state))) "no-action is reset")))))

(deftest auto-no-action
  (testing "tower of rezzed ice, runner breaks everything and automatically continues until approaching server"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Vanilla" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Corroder"]}})
      (dotimes [_ 2] (play-from-hand state :corp "Vanilla" "HQ"))
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [v0 (get-ice state :hq 0)
            v1 (get-ice state :hq 1)
            cor (get-program state 0)]
        (rez state :corp v0)
        (rez state :corp v1)
        (run-on state :hq)
        (core/toggle-auto-no-action state :corp nil)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (is (= (refresh v1) (core/get-current-ice state)) "Approaching v1")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice")
        (card-ability state :runner cor 0)
        (click-prompt state :runner "End the run")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (is (= (refresh v0) (core/get-current-ice state)) "Approaching v0")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})
        (is (= :approach-server (:phase (:run @state))) "Approaching server")
        (core/continue state :runner nil)
        (is (= :approach-server (:phase (:run @state))) "Still approaching server, waiting on Corp")
        (core/continue state :corp nil)
        (is (= :access-server (:phase (:run @state))) "Accessing server")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run ended"))))
  (testing "stop at unrezzed ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Vanilla" 3)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Corroder"]}})
      (dotimes [_ 2] (play-from-hand state :corp "Vanilla" "HQ"))
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [v0 (get-ice state :hq 0)
            v1 (get-ice state :hq 1)
            cor (get-program state 0)]
        (rez state :corp v1)
        (run-on state :hq)
        (core/toggle-auto-no-action state :corp nil)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (is (= (refresh v1) (core/get-current-ice state)) "Approaching v1")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice")
        (core/play-dynamic-ability state :runner {:dynamic "auto-pump-and-break" :card (refresh cor)})
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (is (= (refresh v0) (core/get-current-ice state)) "Approaching v0")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, waiting on Corp")
        (rez state :corp v0 {:press-continue true})
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice"))))
  (testing "auto-no-action on toggling setting"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Corroder"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [v0 (get-ice state :hq 0)
            cor (get-program state 0)]
        (rez state :corp v0)
        (run-on state :hq)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, waiting on Corp")
        (core/toggle-auto-no-action state :corp nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice"))))
  (testing "no auto-no-action on toggling setting on unrezzed ice"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Corroder"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [v0 (get-ice state :hq 0)
            cor (get-program state 0)]
        (run-on state :hq)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, waiting on Corp")
        (core/toggle-auto-no-action state :corp nil)
        (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, because ice is unrezzed")))))

(deftest hide-continue-msg
  (testing "No message for Runner on approach"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (is (= :approach-ice (:phase (:run @state))) "Runner approaches ice")
        (core/continue state :runner nil)
        (is (not (last-log-contains? state "Runner has no further action.")) "Message is not shown for Runner on approach"))))
  (testing "Message for Corp on approach"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (is (= :approach-ice (:phase (:run @state))) "Runner approaches ice")
        (core/continue state :corp nil)
        (is (last-log-contains? state "Corp has no further action.") "Message is shown for Corp on approach"))))
  (testing "Message for Runner on encounter"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp iw)
        (run-continue state)
        (is (= :encounter-ice (:phase (:run @state))) "Runner encounters ice")
        (core/continue state :runner nil)
        (is (last-log-contains? state "Runner has no further action.") "Message is shown for Runner on encounter"))))
  (testing "No message for Corp on encounter"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (let [iw (get-ice state :remote1 0)]
        (take-credits state :corp)
        (run-on state :remote1)
        (rez state :corp iw)
        (run-continue state)
        (is (= :encounter-ice (:phase (:run @state))) "Runner encounters ice")
        (core/continue state :corp nil)
        (is (not (last-log-contains? state "Corp has no further action.")) "Message is not shown for Corp on encounter")))))

(deftest continue-and-jack-out
  (testing "Approach next ice still happens on jack out"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Ice Wall" 2)]}
                 :runner {:hand ["Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [iw0 (get-ice state :remote1 0)
            iw1 (get-ice state :remote1 1)
            cor (get-program state 0)]
        (run-on state :remote1)
        (rez state :corp iw1)
        (run-continue state)
        (core/continue state :corp nil)
        (card-ability state :runner cor 0)
        (click-prompt state :runner "End the run")
        (core/continue state :runner {:jack-out true})
        (is (second-last-log-contains? state "Runner approaches") "Approach triggers still happened")
        (is (last-log-contains? state "Runner jacks out") "Runner got jacked out")))))

(deftest continue-no-action
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Ice Wall" 2)]}
               :runner {:hand ["Devil Charm"]
                        :discard ["Paperclip"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Devil Charm")
    (run-on state :rd)
    (core/continue state :runner nil)
    (rez state :corp (get-ice state :rd 0) {:press-continue true})
    (is (prompt-is-type? state :corp :waiting) "Corp shouldn't get runner's prompts")))

(deftest multi-access
  (testing "Correct handling of multi accesses with draws in between accesses"
    (testing "Drawing cards underneath the currently accessed card"
      (do-game
        (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                          :hand ["Advanced Assembly Lines" "Brain Rewiring" "Chiyashi"
                                 "DNA Tracker" "Excalibur" "Fire Wall" "Gold Farmer"]
                          :deck []}})
        (core/move state :corp (find-card "Advanced Assembly Lines" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Brain Rewiring" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Gold Farmer" (:hand (get-corp))) :deck)
        ; Deck is now ABCDEFG from top to bottom
        (take-credits state :corp)
        (run-on state :rd)
        (core/access-bonus state :runner :rd 4)
        (run-continue state :access-server)
        (is (= "You accessed Advanced Assembly Lines." (-> (get-runner) :prompt first :msg)) "Accessed A")
        (click-prompt state :runner "No action")
        (is (= "You accessed Brain Rewiring." (-> (get-runner) :prompt first :msg)) "Accessed B")
        (click-prompt state :runner "Steal")
        (click-prompt state :corp "Draw 2 cards")
        (is (find-card "Advanced Assembly Lines"  (:hand (get-corp))) "Drawn A")
        (is (find-card "Chiyashi" (:hand (get-corp))) "Drawn C")
        (is (= "You accessed DNA Tracker." (-> (get-runner) :prompt first :msg)) "Accessed D")
        (click-prompt state :runner "No action")
        (is (= "You accessed Excalibur." (-> (get-runner) :prompt first :msg)) "Accessed E")
        (click-prompt state :runner "No action")
        (is (= "You accessed Fire Wall." (-> (get-runner) :prompt first :msg)) "Accessed F")
        (click-prompt state :runner "No action")
        (is (empty? (:prompt (get-runner))) "No more accesses")
        (is (= "DNA Tracker" (-> (get-corp) :deck first :title)) "D on top")))
    (testing "Drawing cards above the currently accessed card"
      (do-game
        (new-game {:corp {:id "Sportsmetal: Go Big or Go Home"
                          :hand ["Advanced Assembly Lines" "Brainstorm" "Chiyashi"
                                 "Domestic Sleepers" "Excalibur" "Fire Wall" "Gold Farmer"]
                          :deck []}})
        (core/move state :corp (find-card "Advanced Assembly Lines" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Domestic Sleepers" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Gold Farmer" (:hand (get-corp))) :deck)
        ; Deck is now ABCDEFG from top to bottom
        (take-credits state :corp)
        (run-on state :rd)
        (core/access-bonus state :runner :rd 4)
        (run-continue state :access-server)
        (is (= "You accessed Advanced Assembly Lines." (-> (get-runner) :prompt first :msg)) "Accessed A")
        (click-prompt state :runner "No action")
        (is (= "You accessed Brainstorm." (-> (get-runner) :prompt first :msg)) "Accessed B")
        (click-prompt state :runner "No action")
        (is (= "You accessed Chiyashi." (-> (get-runner) :prompt first :msg)) "Accessed C")
        (click-prompt state :runner "No action")
        (is (= "You accessed Domestic Sleepers." (-> (get-runner) :prompt first :msg)) "Accessed D")
        (click-prompt state :runner "Steal")
        (click-prompt state :corp "Draw 2 cards")
        (is (count (filter #(= "Advanced Assembly Lines" %) (:hand (get-corp)))) "Drawn A")
        (is (count (filter #(= "Brainstorm" %) (:hand (get-corp)))) "Drawn C")
        (is (= "You accessed Excalibur." (-> (get-runner) :prompt first :msg)) "Accessed E")
        (click-prompt state :runner "No action")
        (is (empty? (:prompt (get-runner))) "No more accesses")
        (is (= "Chiyashi" (-> (get-corp) :deck first :title)) "C on top"))))
  (testing "Correct handling of multi accesses with shuffle in between accesses"
    (testing "Shuffle from Bacterial Programming"
      (do-game
        (new-game {:corp {:hand ["Advanced Assembly Lines" "Bacterial Programming" "Chiyashi"
                                 "DNA Tracker" "Excalibur" "Fire Wall" "Gold Farmer"
                                 "Hostile Infrastructure"]
                          :deck []}})
        (core/move state :corp (find-card "Advanced Assembly Lines" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Bacterial Programming" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Gold Farmer" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Hostile Infrastructure" (:hand (get-corp))) :deck)
        ; Deck is now ABCDEFGH from top to bottom
        (take-credits state :corp)
        (run-on state :rd)
        (core/access-bonus state :runner :rd 4)
        (run-continue state :access-server)
        (is (= "You accessed Advanced Assembly Lines." (-> (get-runner) :prompt first :msg)) "Accessed A")
        (click-prompt state :runner "No action")
        (is (= "You accessed Bacterial Programming." (-> (get-runner) :prompt first :msg)) "Accessed B")
        (click-prompt state :runner "Steal")
        (click-prompt state :corp "Yes")
        (click-prompt state :corp "Done")
        (click-prompt state :corp "Done")
        (click-prompt state :corp "Hostile Infrastructure")
        (click-prompt state :corp "Gold Farmer")
        (click-prompt state :corp "Fire Wall")
        (click-prompt state :corp "Excalibur")
        (click-prompt state :corp "DNA Tracker")
        (click-prompt state :corp "Chiyashi")
        (click-prompt state :corp "Advanced Assembly Lines")
        (click-prompt state :corp "Done")
        (is (= "You accessed Advanced Assembly Lines." (-> (get-runner) :prompt first :msg)) "Accessed A again")
        (click-prompt state :runner "No action")
        (is (= "You accessed Chiyashi." (-> (get-runner) :prompt first :msg)) "Accessed C")
        (click-prompt state :runner "No action")
        (is (= "You accessed DNA Tracker." (-> (get-runner) :prompt first :msg)) "Accessed D")
        (click-prompt state :runner "No action")
        (is (empty? (:prompt (get-runner))) "No more accesses")
        (is (= "Advanced Assembly Lines" (-> (get-corp) :deck first :title)) "A on top")))
    (testing "Shuffle from paid ability during accesses"
      (do-game
        (new-game {:corp {:hand ["Advanced Assembly Lines" "Brainstorm" "Chrysalis" "DNA Tracker" "Efficiency Committee"
                                 "Shannon Claire"]
                          :deck []}})
        (core/move state :corp (find-card "Advanced Assembly Lines" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Chrysalis" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Efficiency Committee" (:hand (get-corp))) :deck)
        (play-from-hand state :corp "Shannon Claire" "New remote")
        (let [sc (get-content state :remote1 0)]
          (rez state :corp sc)
          ; Deck is now ABCD from top to bottom
          (take-credits state :corp)
          (run-on state :rd)
          (core/access-bonus state :runner :rd 3)
          (run-continue state :access-server)
          (is (= "You accessed Advanced Assembly Lines." (-> (get-runner) :prompt first :msg)) "Accessed A")
          (click-prompt state :runner "No action")
          (is (= "You accessed Brainstorm." (-> (get-runner) :prompt first :msg)) "Accessed B")
          (click-prompt state :runner "No action")
          (is (= "You are encountering Chrysalis. Allow its subroutine to fire?" (-> (get-runner) :prompt first :msg)) "Accessing C")
          (card-ability state :corp sc 1)
          (click-prompt state :corp "Efficiency Committee")
          ; R&D was shuffled
          (click-prompt state :runner "No") ; not letting sub fire
          (click-prompt state :runner "No action") ; end accessing C
          (if (= "Chrysalis" (-> (get-corp) :deck first :title))
            (do
              (is (= "You are encountering Chrysalis. Allow its subroutine to fire?" (-> (get-runner) :prompt first :msg)) "Accessing top card being C")
              (click-prompt state :runner "No") ; not letting sub fire
              (click-prompt state :runner "No action")) ; end accessing C
            (do
              (is (= (str "You accessed " (-> (get-corp) :deck first :title) ".")
                     (-> (get-runner) :prompt first :msg))
                  "Accessing top card of R&D")
              (click-prompt state :runner "No action")))
          (is (empty? (:prompt (get-runner))) "No more accesses")))))
  (testing "Reordering cards during multi access"
    (testing "Reorder through Anansi sub"
      (do-game
        (new-game {:corp {:hand ["Advanced Assembly Lines" "Brainstorm" "Chrysalis" "DNA Tracker" "Excalibur"
                                 "Marcus Batty" "Anansi"]
                          :deck []
                          :credits 10}})
        (core/move state :corp (find-card "Advanced Assembly Lines" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Chrysalis" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
        (play-from-hand state :corp "Marcus Batty" "R&D")
        (play-from-hand state :corp "Anansi" "R&D")
        (let [mb (get-content state :rd 0)
              an (get-ice state :rd 0)]
          (rez state :corp mb)
          ; Deck is now ABCDE from top to bottom
          (take-credits state :corp)
          (run-on state :rd)
          (core/access-bonus state :runner :rd 3)
          (run-continue state :approach-server)
          (rez state :corp an)
          (run-continue state :access-server)
          (click-prompt state :runner "Marcus Batty")
          (click-prompt state :runner "No action")
          (is (= "You accessed Advanced Assembly Lines." (:msg (prompt-map :runner))) "Accessed A")
          (click-prompt state :runner "No action")
          (is (= "You accessed Brainstorm." (:msg (prompt-map :runner))) "Accessed B")
          (click-prompt state :runner "No action")
          (is (= "You are encountering Chrysalis. Allow its subroutine to fire?" (:msg (prompt-map :runner))) "Accessing C")
          (click-prompt state :runner "No")
          (card-ability state :corp mb 0)
          (click-prompt state :corp "2 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-card state :corp (refresh an))
          (click-prompt state :corp "Rearrange the top 5 cards of R&D")
          (click-prompt state :corp "Excalibur")
          (click-prompt state :corp "DNA Tracker")
          (click-prompt state :corp "Chrysalis")
          (click-prompt state :corp "Brainstorm")
          (click-prompt state :corp "Advanced Assembly Lines")
          (click-prompt state :corp "Done")
          (click-prompt state :runner "No action")
          (is (= "You accessed Advanced Assembly Lines." (:msg (prompt-map :runner))) "Accessed A")
          (click-prompt state :runner "No action")
          (is (empty? (:prompt (get-runner))) "No more accesses"))))))
