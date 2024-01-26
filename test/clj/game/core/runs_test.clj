(ns game.core.runs-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]
   [game.utils :as utils]))

(deftest run-timing-with-no-ice
    ;; with no ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (take-credits state :corp)
      (run-on state :archives)
      (run-continue state)
      (is (nil? (:run @state)))))

(deftest run-timing-with-an-ice
    ;; with an ice
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
      (is (nil? (:run @state)) "ice Wall subroutine ends the run")
      (is (nil? (get-in @state [:end-run :ended])) "Ended status cleared")))

(deftest run-timing-with-ice-and-a-breaker
    ;; with ice and a breaker
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
      (is (= :movement (:phase (:run @state))))
      (run-continue state)
      (click-prompt state :runner "No action") ; Access Hedge Fund
      (is (nil? (:run @state)))))

(deftest run-timing-with-ice-with-on-encounter-effect
    ;; with ice with on-encounter effect
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

(deftest run-timing-with-paid-ability-before-ice-with-on-encounter-effect
    ;; with paid ability before ice with on-encounter effect
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
      (rez state :corp (get-ice state :remote1 0))
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Corroder")
      (is (zero? (:credit (get-runner))) "Can't afford Tollbooth")
      (is (= :approach-ice (:phase (:run @state))) "Haven't left the approach window yet")
      (run-continue state)
      (is (nil? (:run @state)) "Can't afford Tollbooth, so run ends")))

(deftest run-timing-with-bypass
    ;; with bypass
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
      (is (= :movement (:phase (:run @state))) "Inside Job has bypassed Ice Wall")
      (run-jack-out state)))

(deftest run-timing-with-bypass-vs-cannot-be-bypassed
    ;; with bypass vs cannot be bypassed
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

(deftest run-timing-with-bypass-vs-ice-with-on-encounter-effect
    ;; with bypass vs ice with on-encounter effect
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
        (is (= :movement (:phase (:run @state))) "Inside Job has bypassed Tollbooth")
        (is (= credits (:credit (get-runner)))))
      (run-jack-out state)
      (is (nil? (:run @state)))))

(deftest run-timing-with-paid-ability-that-ends-the-run-during-encounter-border-control
    ;; with paid ability that ends the run during encounter (Border Control)
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

(deftest run-timing-trashing-a-solo-ice-on-an-empty-server-4940
    ;; trashing a solo ice on an empty server #4940
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

(deftest run-timing-redirection-updates-current-ice-5047
    ;; Redirection updates current-ice. #5047
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

(deftest run-timing-changing-phases-fires-end-of-encounter-events
    ;; Changing phases fires end of encounter events
    (do-game
     (new-game {:runner {:hand ["Corroder"]}
                :corp {:deck [(qty "Hedge Fund" 5)]
                       :hand ["Vanilla" "Ice Wall"]}})
     (play-from-hand state :corp "Vanilla" "HQ")
     (rez state :corp (get-ice state :hq 0))
     (play-from-hand state :corp "Ice Wall" "Archives")
     (rez state :corp (get-ice state :archives 0))
     (take-credits state :corp)
     (play-from-hand state :runner "Corroder")
     (run-on state :hq)
     (run-continue state :encounter-ice)
     (let [cor (get-program state 0)]
       (card-ability state :runner cor 1)
       (is (= "Vanilla" (:title (core/get-current-ice state))))
       (is (= 3 (:current-strength (refresh cor))))
       (core/redirect-run state :corp "Archives" :approach-ice)
       (run-continue state)
       (is (= [:archives] (get-in @state [:run :server])) "Runner now running on Archives")
       (run-continue state :encounter-ice)
       (is (= 1 (count (:encounters @state))))
       (is (= "Ice Wall" (:title (core/get-current-ice state))))
       (is (= 2 (:current-strength (refresh cor)))))))

(deftest run-timing-cr-1-4-6-8-2c-any-other-priority-window-is-closed-normally
    ;; cr 1.4 6.8.2c: any other priority window is closed normally
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
      (is (changed? [(get-counters (get-content state :remote1 0) :power) -1]
            (run-empty-server state :remote2)
            (is (= "Choose a trigger to resolve" (:msg (prompt-map :corp))))
            (is (= ["Embolus" "Giordano Memorial Field"] (map :title (prompt-buttons :corp))))
            (click-prompt state :corp "Giordano Memorial Field")
            (click-prompt state :runner "End the run"))
          "Embolus loses a power counter even tho GMF is resolved first and ends the run")))

(deftest replace-access-you-may-only
    ;; 'You may' only
    (testing "and choosing replacement effect"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Hedge Fund"]}
                   :runner {:hand ["Account Siphon"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Account Siphon")
        (run-continue state)
        (is (= ["Account Siphon" "Breach HQ"] (prompt-buttons :runner)) "Runner can choose")
        (click-prompt state :runner "Account Siphon")
        (is (second-last-log-contains? state "Runner uses the replacement effect from Account Siphon")
            "Replacement effect is noted")
        (is (no-prompt? state :runner) "No access, no replacement effects")))
    (testing "and choosing to access cards"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Hedge Fund"]}
                   :runner {:hand ["Account Siphon"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Account Siphon")
        (run-continue state)
        (is (= ["Account Siphon" "Breach HQ"] (prompt-buttons :runner)) "Runner can choose")
        (click-prompt state :runner "Breach HQ")
        (is (last-n-log-contains? state 2 "Runner chooses to breach HQ instead of use a replacement effect")
            "Not choosing replacement effect is noted")
        (is (accessing state "Hedge Fund") "Normal access prompt")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No access, no replacement effects"))))

(deftest replace-access-must-replacement-effects-only
    ;; must replacement effects only
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
      (is (no-prompt? state :runner))
      (is (second-last-log-contains? state "Runner uses the replacement effect from Security Testing")
          "Replacement effect is noted")
      (is (no-prompt? state :runner) "No access, no replacement effects")))

(deftest replace-access-you-may-and-must-replacement-effects
    ;; 'You may' and must replacement effects
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
      (is (no-prompt? state :runner) "No access, no replacement effects")))

(deftest buffered-continue-buffered-continue-on-approaching-ice
    ;; Buffered continue on approaching ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (= :approach-ice (:phase (:run @state))) "Runner in approach on ice")
      (is (not (:no-action (:run @state))) "no-action is not set yet")
      (core/continue state :runner nil)
      (is (= :approach-ice (:phase (:run @state))) "Still in approach on ice")
      (is (= :runner (:no-action (:run @state))) "Runner pressed Continue button")
      (core/continue state :corp nil)
      (is (= :movement (:phase (:run @state))) "Corp pressed Continue button, now approaching server")
      (is (not (:no-action (:run @state))) "no-action is reset")))

(deftest buffered-continue-buffered-continue-on-encountering-ice
    ;; Buffered continue on encountering ice
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
        (is (not (:no-action (core/get-current-encounter state))) "no-action is not set yet")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Still in encounter with ice")
        (is (= :runner (:no-action (core/get-current-encounter state))) "Runner pressed Continue button")
        (core/continue state :corp nil)
        (is (= :movement (:phase (:run @state))) "Corp pressed Continue button, now approaching server")
        (is (not (:no-action (:run @state))) "no-action is reset"))))

(deftest auto-no-action-tower-of-rezzed-ice-runner-breaks-everything-and-automatically-continues-until-approaching-server
    ;; tower of rezzed ice, runner breaks everything and automatically continues until approaching server
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
        (is (= :movement (:phase (:run @state))) "Movement")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (is (= (refresh v0) (core/get-current-ice state)) "Approaching v0")
        (core/continue state :runner nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice")
        (auto-pump-and-break state (refresh cor))
        (is (= :movement (:phase (:run @state))) "Movement before approaching server")
        (core/continue state :runner nil)
        (is (= :movement (:phase (:run @state))) "Still before approaching server, waiting on Corp")
        (core/continue state :corp nil)
        (is (= :success (:phase (:run @state))) "Accessing server")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run ended"))))

(deftest auto-no-action-stop-at-unrezzed-ice
    ;; stop at unrezzed ice
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
        (auto-pump-and-break state (refresh cor))
        (is (= :movement (:phase (:run @state))) "Movement phase")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (is (= (refresh v0) (core/get-current-ice state)) "Approaching v0")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, waiting on Corp")
        (rez state :corp v0)
        (core/continue state :corp nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice"))))

(deftest auto-no-action-auto-no-action-on-toggling-setting
    ;; auto-no-action on toggling setting
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Corroder"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [v0 (get-ice state :hq 0)]
        (rez state :corp v0)
        (run-on state :hq)
        (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
        (core/continue state :runner nil)
        (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, waiting on Corp")
        (core/toggle-auto-no-action state :corp nil)
        (is (= :encounter-ice (:phase (:run @state))) "Encountering ice"))))

(deftest auto-no-action-no-auto-no-action-on-toggling-setting-on-unrezzed-ice
    ;; no auto-no-action on toggling setting on unrezzed ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Vanilla" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Corroder"]}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state :hq)
      (is (= :approach-ice (:phase (:run @state))) "Approaching ice")
      (core/continue state :runner nil)
      (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, waiting on Corp")
      (core/toggle-auto-no-action state :corp nil)
      (is (= :approach-ice (:phase (:run @state))) "Still approaching ice, because ice is unrezzed")))

(deftest hide-continue-msg-no-message-for-runner-on-approach
    ;; No message for Runner on approach
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (= :approach-ice (:phase (:run @state))) "Runner approaches ice")
      (core/continue state :runner nil)
      (is (not (last-log-contains? state "Runner has no further action.")) "Message is not shown for Runner on approach")))

(deftest hide-continue-msg-message-for-corp-on-approach
    ;; Message for Corp on approach
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (run-on state :remote1)
      (is (= :approach-ice (:phase (:run @state))) "Runner approaches ice")
      (core/continue state :corp nil)
      (is (last-log-contains? state "Corp has no further action.") "Message is shown for Corp on approach")))

(deftest hide-continue-msg-message-for-runner-on-encounter
    ;; Message for Runner on encounter
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

(deftest hide-continue-msg-no-message-for-corp-on-encounter
    ;; No message for Corp on encounter
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
        (is (not (last-log-contains? state "Corp has no further action.")) "Message is not shown for Corp on encounter"))))

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
    (rez state :corp (get-ice state :rd 0))
    (core/continue state :corp nil)
    (is (prompt-is-type? state :corp :waiting) "Corp shouldn't get runner's prompts")))

(deftest multi-access-correct-handling-of-multi-accesses-with-draws-in-between-accesses
    ;; Correct handling of multi accesses with draws in between accesses
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
        (run-continue state :success)
        (is (accessing state "Advanced Assembly Lines") "Accessed A")
        (click-prompt state :runner "No action")
        (is (accessing state "Brain Rewiring") "Accessed B")
        (click-prompt state :runner "Steal")
        (click-prompt state :corp "Draw 2 cards")
        (is (find-card "Advanced Assembly Lines"  (:hand (get-corp))) "Drawn A")
        (is (find-card "Chiyashi" (:hand (get-corp))) "Drawn C")
        (is (accessing state "DNA Tracker") "Accessed D")
        (click-prompt state :runner "No action")
        (is (accessing state "Excalibur") "Accessed E")
        (click-prompt state :runner "No action")
        (is (accessing state "Fire Wall") "Accessed F")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No more accesses")
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
        (run-continue state :success)
        (is (accessing state "Advanced Assembly Lines") "Accessed A")
        (click-prompt state :runner "No action")
        (is (accessing state "Brainstorm") "Accessed B")
        (click-prompt state :runner "No action")
        (is (accessing state "Chiyashi") "Accessed C")
        (click-prompt state :runner "No action")
        (is (accessing state "Domestic Sleepers") "Accessed D")
        (click-prompt state :runner "Steal")
        (click-prompt state :corp "Draw 2 cards")
        (is (count (filter #(= "Advanced Assembly Lines" %) (:hand (get-corp)))) "Drawn A")
        (is (count (filter #(= "Brainstorm" %) (:hand (get-corp)))) "Drawn C")
        (is (accessing state "Excalibur") "Accessed E")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No more accesses")
        (is (= "Chiyashi" (-> (get-corp) :deck first :title)) "C on top"))))

(deftest multi-access-correct-handling-of-multi-accesses-with-shuffle-in-between-accesses
    ;; Correct handling of multi accesses with shuffle in between accesses
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
        (run-continue state :success)
        (is (accessing state "Advanced Assembly Lines") "Accessed A")
        (click-prompt state :runner "No action")
        (is (accessing state "Bacterial Programming") "Accessed B")
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
        (is (accessing state "Advanced Assembly Lines") "Accessed A again")
        (click-prompt state :runner "No action")
        (is (accessing state "Chiyashi") "Accessed C")
        (click-prompt state :runner "No action")
        (is (accessing state "DNA Tracker") "Accessed D")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No more accesses")
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
          (run-continue state :success)
          (is (accessing state "Advanced Assembly Lines") "Accessed A")
          (click-prompt state :runner "No action")
          (is (accessing state "Brainstorm") "Accessed B")
          (click-prompt state :runner "No action")
          (is (= "Chrysalis" (:title (core/get-current-ice state))) "Accessing C")
          (card-ability state :corp sc 1)
          (click-prompt state :corp "Efficiency Committee")
          ; R&D was shuffled
          (encounter-continue state) ; end encounter
          (click-prompt state :runner "No action") ; end accessing C
          (let [top-card-title (-> (get-corp) :deck first :title)]
            (if (= "Chrysalis" top-card-title)
              (do
                (is (= top-card-title (:title (core/get-current-ice state))) "Accessing top card being C")
                (encounter-continue state)) ; end encounter
              (is (accessing state top-card-title) "Accessing top card of R&D")))
          (click-prompt state :runner "No action")
          (is (no-prompt? state :runner) "No more accesses")))))

(deftest multi-access-reordering-cards-during-multi-access
    ;; Reordering cards during multi access
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
          (run-continue state :movement)
          (rez state :corp an)
          (run-continue state :success)
          (click-prompt state :runner "Marcus Batty")
          (click-prompt state :runner "No action")
          (is (accessing state "Advanced Assembly Lines") "Accessed A")
          (click-prompt state :runner "No action")
          (is (accessing state "Brainstorm") "Accessed B")
          (click-prompt state :runner "No action")
          (is (= "Chrysalis" (:title (core/get-current-ice state))) "Accessing C")
          (encounter-continue state)
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
          (is (accessing state "Advanced Assembly Lines") "Accessed A")
          (click-prompt state :runner "No action")
          (is (no-prompt? state :runner) "No more accesses")))))

(deftest forced-encounters-forced-encounters-during-run
    ;; Forced encounters - During run
    (testing "Forced encounters during access continues access after completion"
      (do-game
       (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                         :hand ["Ice Wall" "Ganked!"]}})
       (play-from-hand state :corp "Ice Wall" "R&D")
       (play-from-hand state :corp "Ganked!" "R&D")
       (let [iw (get-ice state :rd 0)]
         (rez state :corp iw)
         (take-credits state :corp)
         (run-on state :rd)
         (run-continue state)
         (run-continue state)
         (run-continue state)
         (is (not (core/get-current-encounter state)) "The runner should not be encountering an ice before access")
         (click-prompt state :runner "Unrezzed upgrade")
         (click-prompt state :corp "Yes")
         (click-card state :corp iw)
         (is (core/get-current-encounter state) "The runner should be encountering an ice")
         (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
         (is (not (accessing state "Hedge Fund")) "Access paused while encounter is active")
         (encounter-continue state)
         (is (not (second-last-log-contains? state "Runner passes .")) "Should not pass ice in a forced encounter.")
         (is (not (last-log-contains? state "Runner approaches R&D.")) "Should not approach server after forced encounter.")
         (is (accessing state "Hedge Fund") "Continue access after encounter ends")
         (click-prompt state :runner "No action")
         (is (empty? (:run @state)) "The run has ended"))))
    (testing "Central - Forced encounters during access that end the run stop further access"
      (do-game
       (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                         :hand ["Ice Wall" "Ganked!"]}})
       (play-from-hand state :corp "Ice Wall" "R&D")
       (play-from-hand state :corp "Ganked!" "R&D")
       (let [iw (get-ice state :rd 0)]
         (rez state :corp iw)
         (take-credits state :corp)
         (run-on state :rd)
         (run-continue state)
         (run-continue state)
         (run-continue state)
         (is (not (core/get-current-encounter state)) "The runner should not be encountering an ice before access")
         (click-prompt state :runner "Unrezzed upgrade")
         (click-prompt state :corp "Yes")
         (click-card state :corp iw)
         (is (core/get-current-encounter state) "The runner should be encountering an ice")
         (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
         (is (not (accessing state "Hedge Fund")) "Access paused while encounter is active")
         (fire-subs state (refresh iw))
         (is (no-prompt? state :runner) "Encounter has ended and not accessing additional cards")
         (is (empty? (:run @state)) "The run has ended")
         (is (nil? (get-in @state [:end-run :ended])) "Ended status cleared")
         (is (-> @state :runner :register :accessed-cards) "The runner accessed cards this run")
         (is (nil? (-> @state :stats :runner :access :cards)) "No cards were directly accessed (Ganked! is trashed before this increments)"))))
    (testing "Central - Stacked forced encounters during access that end the run stop further access"
      (do-game
       (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                         :hand ["Konjin" "Ice Wall" "Ganked!"]}})
       (play-from-hand state :corp "Konjin" "R&D")
       (play-from-hand state :corp "Ice Wall" "HQ")
       (play-from-hand state :corp "Ganked!" "R&D")
       (let [iw (get-ice state :hq 0)
             konjin (get-ice state :rd 0)]
         (rez state :corp iw)
         (rez state :corp konjin)
         (take-credits state :corp)
         (run-on state :rd)
         (run-continue state)
         (click-prompt state :corp "0 [Credits]")
         (click-prompt state :runner "0 [Credits]")
         (run-continue state)
         (run-continue state)
         (is (not (core/get-current-encounter state)) "The runner should not be encountering an ice before access")
         (click-prompt state :runner "Unrezzed upgrade")
         (click-prompt state :corp "Yes")
         (click-card state :corp konjin)
         (is (= (refresh konjin) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
         (click-prompt state :corp "0 [Credits]")
         (click-prompt state :runner "1 [Credits]")
         (click-card state :corp iw)
         (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
         (is (= 2 (count (:encounters @state))))
         (is (not (accessing state "Hedge Fund")) "Access paused while encounter is active")
         (fire-subs state (refresh iw))
         (is (no-prompt? state :runner) "Encounter has ended and not accessing additional cards")
         (is (empty? (:run @state)) "The run has ended")
         (is (nil? (get-in @state [:end-run :ended])) "Ended status cleared")
         (is (-> @state :runner :register :accessed-cards) "The runner accessed cards this run")
         (is (nil? (-> @state :stats :runner :access :cards)) "No cards were directly accessed (Ganked! is trashed before this increments)"))))
    (testing "Remote - Forced encounters during access that end the run stop further access"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ice Wall" "Ganked!" "PAD Campaign"]}})
        (play-from-hand state :corp "Ice Wall" "New remote")
        (play-from-hand state :corp "Ganked!" "Server 1")
        (play-from-hand state :corp "PAD Campaign" "Server 1")
        (take-credits state :corp)
        (let [iw (get-ice state :remote1 0)]
          (rez state :corp iw)
          (run-on state :remote1)
          (run-continue state)
          (run-continue state)
          (run-continue state)
          (is (not (core/get-current-encounter state)) "The runner should not be encountering an ice before access")
          (click-card state :runner (get-content state :remote1 0))
          (click-prompt state :corp "Yes")
          (click-card state :corp iw)
          (is (core/get-current-encounter state) "The runner should be encountering an ice")
          (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
          (fire-subs state (refresh iw))
          (is (no-prompt? state :runner) "Encounter has ended and not accessing additional cards")
          (is (empty? (:run @state)) "The run has ended")))))

(deftest forced-encounters-ice-breakers-and-broken-subroutines-reset-after-a-forced-encounter-ends
    ;; Ice breakers and broken subroutines reset after a forced encounter ends
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Ganked!"]}
                 :runner {:hand ["Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Ganked!" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (let [iw (get-ice state :rd 0)
            cor (get-program state 0)]
        (rez state :corp iw)
        (run-on state :rd)
        (run-continue state)
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "Unrezzed upgrade")
        (click-prompt state :corp "Yes")
        (click-card state :corp iw)
        (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
        (card-ability state :runner cor 1)
        (card-ability state :runner cor 0)
        (click-prompt state :runner "End the run")
        (is (= 3 (:current-strength (refresh cor))) "Corroder's strength boosted to 3")
        (is (-> (refresh iw) :subroutines first :broken) "Ice Wall's subroutine has been broken")
        (encounter-continue state)
        (is (= 2 (:current-strength (refresh cor))) "Corroder's strength reset to 2")
        (is (not (-> (refresh iw) :subroutines first :broken)) "Ice Wall's subroutine is no longer broken")
        (is (accessing state "Hedge Fund") "Continue access after encounter ends")
        (click-prompt state :runner "No action"))))

(deftest forced-encounters-forced-encounters-outside-of-run
    ;; Forced Encounters - Outside of run
    (testing "Forced encounters outside of a run end properly on continue"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ice Wall" "Ganked!"]}
                   :runner {:hand ["Corroder" "Gordian Blade" "Quest Completed"]
                            :credits 20}})
        (play-from-hand state :corp "Ice Wall" "New remote")
        (play-from-hand state :corp "Ganked!" "Server 1")
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Gordian Blade")
        (take-credits state :runner)
        (take-credits state :corp)
        (let [iw (get-ice state :remote1 0)
              cor (get-program state 0)
              gord (get-program state 1)]
          (rez state :corp iw)
          (run-empty-server state :archives)
          (run-empty-server state :rd)
          (click-prompt state :runner "No action")
          (run-empty-server state :hq)
          (click-prompt state :runner "No action")
          (play-from-hand state :runner "Quest Completed")
          (click-card state :runner (get-content state :remote1 0))
          (click-prompt state :corp "Yes")
          (click-card state :corp iw)
          (is (core/get-current-encounter state) "The runner should be encountering an ice")
          (is (= (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
          (is (last-log-contains? state "Runner encounters Ice Wall protecting Server 1 at position 0.") "Encounter message sent")
          (card-ability state :runner cor 1)
          (card-ability state :runner cor 0)
          (click-prompt state :runner "End the run")
          (card-ability state :runner gord 1)
          (is (= 3 (:current-strength (refresh cor))) "Corroder's strength boosted to 3")
          (is (= 3 (:current-strength (refresh gord))) "Gordian Blade's strength boosted to 3")
          (is (-> (refresh iw) :subroutines first :broken) "Ice Wall's subroutine has been broken")
          (encounter-continue state)
          (is (not (second-last-log-contains? state "Runner passes .")) "Should not pass ice in a forced encounter.")
          (is (not (last-log-contains? state "Runner approaches R&D.")) "Should not approach server after forced encounter.")
          (is (= 2 (:current-strength (refresh cor))) "Corroder's strength reset to 2")
          (is (= 2 (:current-strength (refresh gord))) "Gordian Blade's strength reset to 2")
          (is (not (-> (refresh iw) :subroutines first :broken)) "Ice Wall's subroutine is no longer broken")
          (is (no-prompt? state :runner)) "Encounter has ended")))
    (testing "Forced encounters outside of a run end properly when an 'End the run' subroutine is fired"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Little Engine" "Ganked!"]}
                   :runner {:hand ["Corroder" "Quest Completed"]}})
        (play-from-hand state :corp "Little Engine" "New remote")
        (play-from-hand state :corp "Ganked!" "Server 1")
        (take-credits state :corp)
        (play-from-hand state :runner "Corroder")
        (take-credits state :runner)
        (take-credits state :corp)
        (let [le (get-ice state :remote1 0)
              cor (get-program state 0)]
          (rez state :corp le)
          (run-empty-server state :archives)
          (run-empty-server state :rd)
          (click-prompt state :runner "No action")
          (run-empty-server state :hq)
          (click-prompt state :runner "No action")
          (play-from-hand state :runner "Quest Completed")
          (click-card state :runner (get-content state :remote1 0))
          (click-prompt state :corp "Yes")
          (click-card state :corp le)
          (is (core/get-current-encounter state) "The runner should be encountering an ice")
          (is (= (refresh le) (core/get-current-ice state)) "The runner should be encountering Little Engine")
          (is (last-log-contains? state "Runner encounters Little Engine protecting Server 1 at position 0.") "Encounter message sent")
          (card-ability state :runner cor 1)
          (is (= 3 (:current-strength (refresh cor))) "Corroder's strength boosted to 3")
          (fire-subs state (refresh le))
          (is (= 2 (:current-strength (refresh cor))) "Corroder's strength reset to 2")
          (is (not= 8 (@state :runner :credits)) "Little Engine's third subroutine should not have fired")
          (is (no-prompt? state :runner) "Encounter has ended")
          (is (nil? (get-in @state [:end-run :ended])) "Ended status cleared")
          (is (not (get-in @state [:runner :register :unsuccessful-run :remote1])) "Not a run"))))
    (testing "Forced encounters outside of a run end properly when a 'Jack out' event occurs"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Little Engine" "Ganked!"]}
                   :runner {:hand ["Flip Switch" "Quest Completed"]}})
        (play-from-hand state :corp "Little Engine" "New remote")
        (play-from-hand state :corp "Ganked!" "Server 1")
        (take-credits state :corp)
        (play-from-hand state :runner "Flip Switch")
        (take-credits state :runner)
        (take-credits state :corp)
        (let [le (get-ice state :remote1 0)
              fs (get-hardware state 0)]
          (rez state :corp le)
          (run-empty-server state :archives)
          (run-empty-server state :rd)
          (click-prompt state :runner "No action")
          (run-empty-server state :hq)
          (click-prompt state :runner "No action")
          (play-from-hand state :runner "Quest Completed")
          (click-card state :runner (get-content state :remote1 0))
          (click-prompt state :corp "Yes")
          (click-card state :corp le)
          (is (core/get-current-encounter state) "The runner should be encountering an ice")
          (is (= (refresh le) (core/get-current-ice state)) "The runner should be encountering Little Engine")
          (is (last-log-contains? state "Runner encounters Little Engine protecting Server 1 at position 0.") "Encounter message sent")
          (card-ability state :runner fs 0)
          (is (not= 9 (@state :runner :credits)) "Little Engine's third subroutine should not have fired")
          (is (no-prompt? state :runner) "Encounter has ended")
          (is (nil? (get-in @state [:end-run :ended])) "Ended status cleared")
          (is (not (get-in @state [:runner :register :unsuccessful-run :remote1])) "Not a run")))))

(deftest forced-encounters-forced-encounters-redirection
    ;; Forced Encounters - Redirection
    (testing "Forced encounter into redirection outside of access changes position"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ice Wall" "Konjin" "Bullfrog"]
                          :credits 10}})
        (play-from-hand state :corp "Ice Wall" "Archives")
        (play-from-hand state :corp "Bullfrog" "R&D")
        (play-from-hand state :corp "Konjin" "HQ")
        (let [iw (get-ice state :archives 0)
              bf (get-ice state :rd 0)
              konjin (get-ice state :hq 0)]
          (rez state :corp iw)
          (rez state :corp bf)
          (rez state :corp konjin)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-card state :corp bf)
          (is (= :hq (-> @state :run :server first)) "Run is on HQ")
          (fire-subs state (refresh bf))
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-prompt state :corp "Archives")
          (is (= :archives (-> @state :run :server first)) "Run is now on Archives")
          (is (= "Bullfrog" (:title (core/get-current-ice state))) "The runner still encountering Bullfrog")
          (is (= "Bullfrog" (:title (get-ice state :archives 1))) "Bullfrog moved to Archives")
          (run-continue state)
          (is (= (refresh konjin) (core/get-current-ice state)) "The Runner returns to encountering Konjin since the timing of the run hasn't changed")
          (run-continue state)
          (is (last-log-contains? state "Runner passes Bullfrog") "Should pass Bullfrog as it is the ice at current position")
          (run-continue state)
          (is (= (refresh iw) (core/get-current-ice state)) "The runner encounters Ice Wall"))))
    (testing "Forced encounter into redirection outside of access changes position"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ice Wall" "Konjin" "Bullfrog"]
                          :credits 10}})
        (play-from-hand state :corp "Ice Wall" "Archives")
        (play-from-hand state :corp "Bullfrog" "R&D")
        (play-from-hand state :corp "Konjin" "HQ")
        (let [iw (get-ice state :archives 0)
              bf (get-ice state :rd 0)
              konjin (get-ice state :hq 0)]
          (rez state :corp iw)
          (rez state :corp bf)
          (rez state :corp konjin)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-card state :corp bf)
          (is (= :hq (-> @state :run :server first)) "Run is on HQ")
          (fire-subs state (refresh bf))
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-prompt state :corp "Archives")
          (is (= :archives (-> @state :run :server first)) "Run is now on Archives")
          (is (= "Bullfrog" (:title (core/get-current-ice state))) "The runner still encountering Bullfrog")
          (is (= "Bullfrog" (:title (get-ice state :archives 1))) "Bullfrog moved to Archives")
          (run-continue state)
          (is (= (refresh konjin) (core/get-current-ice state)) "The Runner returns to encountering Konjin since the timing of the run hasn't changed")
          (run-continue state)
          (is (last-log-contains? state "Runner passes Bullfrog") "Should pass Bullfrog as it is the ice at current position")
          (run-continue state)
          (is (= (refresh iw) (core/get-current-ice state)) "The runner encounters Ice Wall"))))
    (testing "Forced encounter into redirection that changes phase ends encounter"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Konjin" "Cell Portal"]
                          :credits 10}})
        (play-from-hand state :corp "Cell Portal" "R&D")
        (play-from-hand state :corp "Konjin" "HQ")
        (let [cp (get-ice state :rd 0)
              konjin (get-ice state :hq 0)]
          (rez state :corp cp)
          (rez state :corp konjin)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-card state :corp cp)
          (is (= :hq (-> @state :run :server first)) "Run is on HQ")
          (fire-subs state (refresh cp))
          (click-prompt state :runner "No")
          (is (= :hq (-> @state :run :server first)) "Run is still on HQ")
          (is (= :approach-ice (:phase (:run @state))) "Timing changed to the Approach Ice phase")
          (is (empty? (:encounters @state)) "Encounters have ended")
          (run-continue state)
          (is (= (refresh konjin) (core/get-current-ice state)) "The Runner encounters Konjin")
          (is (= "Choose an amount to spend for Konjin" (:msg (prompt-map :corp))) "New encounter triggers Konjin again"))))
    (testing "Forced encounter into redirection during access does not change position or stop access"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Ice Wall" "Konjin" "Bullfrog" "Ganked!"]
                          :credits 10}})
        (play-from-hand state :corp "Ice Wall" "Archives")
        (play-from-hand state :corp "Bullfrog" "R&D")
        (play-from-hand state :corp "Konjin" "HQ")
        (let [iw (get-ice state :archives 0)
              bf (get-ice state :rd 0)
              konjin (get-ice state :hq 0)]
          (rez state :corp iw)
          (rez state :corp bf)
          (rez state :corp konjin)
          (take-credits state :corp)
          (run-on state :hq)
          (run-continue state)
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "0 [Credits]")
          (run-continue state)
          (run-continue state)
          (click-prompt state :corp "Yes")
          (click-card state :corp konjin)
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-card state :corp bf)
          (is (= :hq (-> @state :run :server first)) "Run is on HQ")
          (fire-subs state (refresh bf))
          (click-prompt state :corp "0 [Credits]")
          (click-prompt state :runner "1 [Credits]")
          (click-prompt state :corp "Archives")
          (is (= "Bullfrog" (:title (get-ice state :archives 1))) "Bullfrog moved to Archives")
          (is (-> (get-ice state :archives 1) :subroutines first :fired) "Bullfrog subroutine has fired")
          (is (= :hq (-> @state :run :server first)) "Run is still on HQ")
          (is (= :success (:phase (:run @state))) "Run still in Success phase")
          (is (not (-> @state :run :prevent-access)) "Access should not be prevented")))))

(deftest etr-outside-of-runs-does-not-prevent-new-runs
  ;; ETR outside of a run or encounter does not prevent new runs
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Ice Wall"]}})
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (let [iw (get-ice state :hq 0)]
     (rez state :corp iw)
     (core/process-action "subroutine" state :corp {:card (refresh iw) :subroutine 0}))
   (run-on state :rd)
   (is (get-run) "There is a run in progress")))

(deftest ice-trashed-during-movement-does-not-reapproach-passed-ice
  ;; Ice trashed during Movement - Runner does not reapproach passed ice
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Wall of Static" "Ice Wall" "Vanilla" "Hedge Fund"]
                     :credits 10}})
   (play-from-hand state :corp "Vanilla" "HQ")
   (play-from-hand state :corp "Ice Wall" "HQ")
   (play-from-hand state :corp "Wall of Static" "HQ")
   (take-credits state :corp)
   (let [vanilla (get-ice state :hq 0)
         iw (get-ice state :hq 1)
         ws (get-ice state :hq 2)]
     (rez state :corp vanilla)
     (rez state :corp iw)
     (rez state :corp ws)
     ;; middle ice trashed - move to approach next ice
     (run-on state "HQ")
     (run-continue state :encounter-ice)
     (is (utils/same-card? ws (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue state :movement)
     (trash-card state :corp (refresh iw))
     (run-continue state :approach-ice)
     (is (utils/same-card? vanilla (core/get-current-ice state)) "Approaching Vanilla")
     (run-continue-until state :movement)
     (run-jack-out state)
     ;; inner most ice trashed - move to success
     (run-on state "HQ")
     (run-continue state :encounter-ice)
     (is (utils/same-card? ws (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue state :movement)
     (trash-card state :corp (refresh vanilla))
     (run-continue state :success))))

(deftest ice-trashed-during-approach-does-not-reapproach-passed-ice
  ;; Ice trashed during Approach - Runner does not reapproach passed ice
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Wall of Static" "Ice Wall" "Vanilla" "Hedge Fund"]
                     :credits 10}})
   (play-from-hand state :corp "Vanilla" "HQ")
   (play-from-hand state :corp "Ice Wall" "HQ")
   (play-from-hand state :corp "Wall of Static" "HQ")
   (take-credits state :corp)
   (let [vanilla (get-ice state :hq 0)
         iw (get-ice state :hq 1)
         ws (get-ice state :hq 2)]
     (rez state :corp vanilla)
     (rez state :corp iw)
     (rez state :corp ws)
     ;; middle ice trashed - continue to movement then to approach next ice
     (run-on state "HQ")
     (run-continue state :encounter-ice)
     (is (utils/same-card? ws (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue-until state :approach-ice)
     (is (utils/same-card? iw (core/get-current-ice state)) "Approaching Ice Wall")
     (trash-card state :corp (refresh iw))
     (run-continue state :movement)
     (run-continue state :approach-ice)
     (is (utils/same-card? vanilla (core/get-current-ice state)) "Approaching Vanilla")
     (run-continue-until state :movement)
     (run-jack-out state)
     ;; inner most ice trashed - continue to Movement at innermost position
     (run-on state "HQ")
     (run-continue state :encounter-ice)
     (is (utils/same-card? ws (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue-until state :approach-ice)
     (is (utils/same-card? vanilla (core/get-current-ice state)) "Approaching Vanilla")
     (trash-card state :corp (refresh vanilla))
     (run-continue state :movement)
     (run-continue state :success))))

(deftest ice-trashed-during-encounter-does-not-reapproach-passed-ice
  ;; Ice trashed during Encounter - Runner does not reapproach passed ice
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Wall of Static" "Ice Wall" "Vanilla" "Hedge Fund"]
                     :credits 10}})
   (play-from-hand state :corp "Vanilla" "HQ")
   (play-from-hand state :corp "Ice Wall" "HQ")
   (play-from-hand state :corp "Wall of Static" "HQ")
   (take-credits state :corp)
   (let [vanilla (get-ice state :hq 0)
         iw (get-ice state :hq 1)
         ws (get-ice state :hq 2)]
     (rez state :corp vanilla)
     (rez state :corp iw)
     (rez state :corp ws)
     ;; middle ice trashed - continue to movement then to approach next ice
     (run-on state "HQ")
     (run-continue state :encounter-ice)
     (is (utils/same-card? ws (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue-until state :encounter-ice)
     (is (utils/same-card? iw (core/get-current-ice state)) "Encountering Ice Wall")
     (trash-card state :corp (refresh iw))
     (run-continue state :movement)
     (run-continue state :approach-ice)
     (is (utils/same-card? vanilla (core/get-current-ice state)) "Approaching Vanilla")
     (run-continue-until state :movement)
     (run-jack-out state)
     ;; inner most ice trashed - continue to Movement at innermost position
     (run-on state "HQ")
     (run-continue state :encounter-ice)
     (is (utils/same-card? ws (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue-until state :encounter-ice)
     (is (utils/same-card? vanilla (core/get-current-ice state)) "Approaching Vanilla")
     (trash-card state :corp (refresh vanilla))
     (run-continue state :movement)
     (run-continue state :success))))
