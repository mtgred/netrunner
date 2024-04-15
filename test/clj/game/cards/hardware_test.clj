(ns game.cards.hardware-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest acacia
  ;; Acacia - Optionally gain credits for number of virus tokens then trash
  (do-game
      (new-game {:runner {:deck ["Acacia" "Virus Breeding Ground" "Datasucker"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Acacia")
      (play-from-hand state :runner "Virus Breeding Ground")
      (play-from-hand state :runner "Datasucker")
      (core/add-counter state :runner (get-resource state 0) :virus 4)
      (core/add-counter state :runner (get-program state 0) :virus 3)
      (take-credits state :runner)
      (is (= 2 (:credit (get-runner))) "Runner initial credits")
      (purge state :corp)
      (click-prompt state :runner "Yes")
      (is (= 9 (:credit (get-runner))) "Runner gained 9 credits")
      (is (= 1 (count (:discard (get-runner)))) "Acacia has trashed")))

(deftest acacia-issue-4280-interaction-with-llds-energy-regulator
    ;; Issue #4280: Interaction with LLDS Energy Regulator
    (do-game
      (new-game {:runner {:deck ["Acacia" "LLDS Energy Regulator" "Datasucker"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Acacia")
      (play-from-hand state :runner "Datasucker")
      (play-from-hand state :runner "LLDS Energy Regulator")
      (core/add-counter state :runner (get-program state 0) :virus 3)
      (take-credits state :runner)
      (let [llds (get-program state 1)]
        (is (changed? [(:credit (get-runner)) 0]
              (purge state :corp)
              (click-prompt state :runner "Yes"))
            "Runner didn't get credits before deciding on LLDS")
        (is (changed? [(:credit (get-runner)) -3]
              (card-ability state :runner (refresh llds) 0))
            "Runner pays 3 for LLDS")
        (is (changed? [(:credit (get-runner)) 3]
              (click-prompt state :runner "Done"))
            "Runner got Acacia credits")
        (is (zero? (count (:discard (get-runner)))) "Acacia has not been trashed"))))

(deftest acacia-effect-counts-both-runner-and-corp-virus-counters
    ;; Effect counts both Runner and Corp virus counters
    (do-game
      (new-game {:runner {:deck ["Acacia"]}
                 :corp {:deck ["Sandstone"]}})
      (play-from-hand state :corp "Sandstone" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Acacia")
      (take-credits state :runner)
      (let [sandstone (get-ice state :hq 0)]
        (rez state :corp sandstone)
        (core/add-counter state :corp sandstone :virus 1)
        (is (= 1 (get-counters (refresh sandstone) :virus)) "Sandstone has 1 virus counter")
        (is (= 7 (:credit (get-runner))) "Runner credits should be 7")
        (purge state :corp)
        (click-prompt state :runner "Yes")
        (is (= 8 (:credit (get-runner))) "Runner gained 1 credit from Sandstone's virus counter"))))

(deftest adjusted-matrix
  ;; Adjusted Matrix
  (before-each [state (new-game {:corp {:hand ["Hive"]
                                        :credits 10}
                                 :runner {:hand ["Corroder" "Adjusted Matrix"]
                                          :credits 15}})
                _ (do (play-from-hand state :corp "Hive" "HQ")
                      (rez state :corp (get-ice state :hq 0))
                      (take-credits state :corp)
                      (play-from-hand state :runner "Corroder")
                      (play-from-hand state :runner "Adjusted Matrix")
                      (click-card state :runner "Corroder"))
                hive (get-ice state :hq 0)
                corroder (get-program state 0)]
    (testing "Adds AI to icebreaker subtype"
      (do-game state
        (is (has-subtype? (refresh corroder) "AI"))))
    (testing "Break ability spends a click"
      (do-game state
        (run-on state :hq)
        (run-continue state)
        (let [clicks (:click (get-runner))
              credits (:credit (get-runner))]
          (card-ability state :runner (first (:hosted (refresh corroder))) 0)
          (click-prompt state :runner "End the run")
          (is (= -1 (- (:click (get-runner)) clicks)) "Ability costs a click")
          (is (zero? (- credits (:credit (get-runner)))) "Ability costs no credits"))))
    (testing "break ability breaks 1 subroutine"
      (do-game state
        (run-on state :hq)
        (run-continue state)
        (card-ability state :runner (first (:hosted (refresh corroder))) 0)
        (click-prompt state :runner "End the run")
        (is (:broken (first (:subroutines (refresh hive)))) "The break ability worked")
        (is (no-prompt? state :runner) "Break ability is one at a time")))))

(deftest airbladex-jsrd-ed
  (do-game
    (new-game {:runner {:hand ["AirbladeX (JSRF Ed.)" "Sure Gamble"]}
               :corp {:hand ["Funhouse" "Envelope"]}})
    (play-from-hand state :corp "Envelope" "HQ")
    (play-from-hand state :corp "Funhouse" "HQ")
    (core/gain state :corp :credit 10)
    (take-credits state :corp)
    (play-from-hand state :runner "AirbladeX (JSRF Ed.)")
    (let [airbladex (get-hardware state 0)]
      (run-on state :hq)
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (no-prompt? state :runner) "No Funhouse prompt")
      (is (= 2 (get-counters (refresh airbladex) :power)) "Spent 1 hosted power counter")
      (run-continue state)
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (fire-subs state (get-ice state :hq 0))
      (is (changed? [(count (:hand (get-runner))) 0]
            (card-ability state :runner airbladex 0))
          "1 net damage prevented")
      (is (= 1 (get-counters (refresh airbladex) :power)) "Spent 1 hosted power counter"))))

(deftest airbladex-jsrd-ed-no-prevent-prompt-outside-run
  (do-game
    (new-game {:runner {:hand ["AirbladeX (JSRF Ed.)" (qty "Sure Gamble" 3)]}
               :corp {:hand ["Reaper Function"]}})
    (play-from-hand state :corp "Reaper Function" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "AirbladeX (JSRF Ed.)")
    (take-credits state :runner)
    (end-phase-12 state :corp)
    (click-prompt state :corp "Yes")
    (is (no-prompt? state :runner) "No damage prevention prompt outside of run")))

(deftest akamatsu-mem-chip
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game {:runner {:deck [(qty "Akamatsu Mem Chip" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")))


(deftest alarm-clock
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Alarm Clock"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Alarm Clock")
    (take-credits state :runner)
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (click-prompt state :runner "Yes")
    (is (:run @state) "Run has started")
    (run-continue state)
    (is (changed? [(:click (get-runner)) -2]
                  (click-prompt state :runner "Yes"))
        "Costs 2 clicks")
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")))

(deftest amanuensis
  (do-game
    (new-game {:runner {:hand ["Amanuensis"]
                        :deck [(qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Amanuensis")
    (is (changed? [(get-counters (get-hardware state 0) :power) 1]
                  (take-credits state :runner))
        "Amanuensis gains a power counter at end of turn")
    (take-credits state :corp)
    (remove-tag state :runner)
    (is (changed? [(get-counters (get-hardware state 0) :power) -1
                   (count (:hand (get-runner))) 2]
                  (click-prompt state :runner "Yes"))
        "Spend a power counter when removing a tag to draw 2 cards")))

(deftest amanuensis-corp-spend-tag
  (do-game
    (new-game {:runner {:hand ["Amanuensis"]
                        :deck [(qty "Sure Gamble" 2)]}
               :corp {:hand ["End of the Line"]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Amanuensis")
    (is (changed? [(get-counters (get-hardware state 0) :power) 1]
                  (take-credits state :runner))
        "Amanuensis gains a power counter at end of turn")
    (play-from-hand state :corp "End of the Line")
    (is (no-prompt? state :runner) "No runner prompt for amen")))

(deftest aniccam-trash-trash-before-and-after-install-does-not-trigger
  ;; Aniccam
  (doseq [first-side [:corp :runner]
          second-side [:corp :runner]]
    (testing (str (name first-side) " trash -> install Aniccam -> " (name second-side) " trash does not trigger Aniccam")
      (do-game
        (new-game {:runner {:hand ["Aniccam" "Sure Gamble" "Dirty Laundry"]
                            :deck ["Corroder"]}})
        (take-credits state :corp)
        (trash state first-side (find-card "Sure Gamble" (:hand (get-runner))))
        (play-from-hand state :runner "Aniccam")
        (trash state second-side (find-card "Dirty Laundry" (:hand (get-runner))))
        (is (= 0 (count (:hand (get-runner)))) "The runner has not drawn a card")))))

(deftest aniccam-aniccam-gives-1-mu
    ;; Aniccam gives 1 MU
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (is (= 5 (core/available-mu state)))))

(deftest aniccam-the-runner-draws-1-card-when-an-event-is-trashed-from-the-grip-by-the-runner
    ;; The runner draws 1 card when an event is trashed from the Grip by the Runner
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (trash-from-hand state :runner "Sure Gamble")
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-the-runner-draws-1-card-when-an-event-is-trashed-from-the-grip-by-the-corp
    ;; The runner draws 1 card when an event is trashed from the Grip by the Corp
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (trash state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-trashing-a-non-event-doesn-t-trigger-aniccam
    ;; Trashing a non-event doesn't trigger Aniccam
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Clone Chip" "Mimic" "Daily Casts"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (doseq [card (:hand (get-runner))]
        (trash state :runner card))
      (is (= 0 (count (:hand (get-runner)))) "The runner has not drawn a card")))

(deftest aniccam-trashing-an-event-along-with-some-non-events-triggers-aniccam
    ;; Trashing an event along with some non events triggers Aniccam
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Clone Chip" "Sure Gamble" "Mimic" "Daily Casts"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (doseq [card (:hand (get-runner))]
        (trash state :runner card))
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-aniccam-must-not-trigger-a-second-time-in-one-turn
    ;; Aniccam must not trigger a second time in one turn
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck [(qty "Corroder" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (trash-from-hand state :runner "Sure Gamble")
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")
      (trash-from-hand state :runner "Corroder")
      (is (= 0 (count (:hand (get-runner)))) "The runner has not drawn a second card")))

(deftest aniccam-the-effect-triggers-on-meat-damage
    ;; The effect triggers on meat damage
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (damage state :runner :meat 1)
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-the-effect-triggers-on-net-damage
    ;; The effect triggers on net damage
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (damage state :runner :net 1)
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-the-effect-triggers-on-brain-damage
    ;; The effect triggers on brain damage
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Sure Gamble"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (damage state :runner :brain 1)
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-trashing-an-event-from-r-d-triggers-aniccam
    ;; Trashing an event from R&D triggers Aniccam
    (do-game
      (new-game {:runner {:hand ["Aniccam"]
                          :deck [(qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (trash state :runner (first (:deck (get-runner))))
      (is (find-card "Sure Gamble" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-an-event-being-trashed-after-playing-it-triggers-aniccam
    ;; An event being trashed after playing it triggers Aniccam
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Easy Mark"]
                          :deck ["Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (play-from-hand state :runner "Easy Mark")
      (is (find-card "Corroder" (:hand (get-runner))) "The runner has drawn a card")))

(deftest aniccam-trashing-a-current-triggers-aniccam
    ;; Trashing a current triggers Aniccam
    (do-game
      (new-game {:runner {:hand ["Aniccam" "Hacktivist Meeting"]
                          :deck ["Corroder"]}
                 :corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Scarcity of Resources"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (play-from-hand state :runner "Hacktivist Meeting")
      (is (not (find-card "Corroder" (:hand (get-runner)))) "The runner has not drawn a card immediately after playing a current")
      (take-credits state :runner)
      (play-from-hand state :corp "Scarcity of Resources")
      (is (find-card "Corroder" (:hand (get-runner))) "The has drawn a card after their current was trashed")))

(deftest aniccam-trashing-a-card-counter-doesn-t-trigger-aniccam-5123
    ;; Trashing a card counter doesn't trigger Aniccam #5123
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["SEA Source"]}
                 :runner {:deck ["Sure Gamble"]
                          :hand ["Aniccam" "Artist Colony" "On the Lam"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Aniccam")
      (play-from-hand state :runner "Artist Colony")
      (play-from-hand state :runner "On the Lam")
      (click-card state :runner (get-resource state 0))
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (card-ability state :runner (-> (get-resource state 0) :hosted first) 0)
      (click-prompt state :runner "Done")
      (is (zero? (count-tags state)) "Runner should avoid tag")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap")
      (is (zero? (count (:hand (get-runner)))) "Runner doesn't draw from Aniccam")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game {:corp {:deck ["Shock!" "Launch Campaign"]}
               :runner {:deck ["Archives Interface" "Imp"]}})
    (take-credits state :corp)
    (core/move state :corp (find-card "Shock!" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "Launch Campaign" (:hand (get-corp))) :discard)
    (play-from-hand state :runner "Archives Interface")
    (run-empty-server state :archives)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner (find-card "Shock!" (:discard (get-corp))))
    (is (= "Shock!" (:title (first (:rfg (get-corp))))) "Shock! removed from game")
    (is (empty? (:discard (get-runner))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game {:corp {:deck [(qty "Snare!" 3)]}
               :runner {:deck [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) "Cloak"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (take-credits state :runner 3)
    ;; corp's turn. install something from HQ to trigger Astrolabe draw
    (is (= 4 (count (:hand (get-runner)))) "Started with 4 cards")
    (play-from-hand state :corp "Snare!" "New remote")
    (is (= 5 (count (:hand (get-runner)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (is (zero? (count (:discard (get-corp)))) "No cards in discard")
    (play-from-hand state :corp "Snare!" "Server 1")
    (click-prompt state :corp "OK")
    (is (= 1 (count (:discard (get-corp)))) "Wrote over old snare")
    (is (= 5 (count (:hand (get-runner)))) "Did not draw")
    (is (= 1 (count (:deck (get-runner)))) "1 card left in deck")))

(deftest autoscripter
  ;; Autoscripter - gain [Click] first time Runner installs program from Grip during their turn.
  ;; Trash if unsuccessful run
  (do-game
    (new-game {:runner {:deck ["Autoscripter" (qty "Inti" 3) "Clone Chip"]}
               :options {:start-as :runner}})
    (testing "Gaining (and not gaining) clicks"
      (play-from-hand state :runner "Inti")
      (play-from-hand state :runner "Autoscripter")
      (play-from-hand state :runner "Inti")
      (is (= 1 (:click (get-runner))) "Did not gain a click when installing program from hand a second time")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Inti")
      (is (= 4 (:click (get-runner))) "Gained 1 click when installing Program from hand")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (trash state :runner (get-program state 0))
      (card-ability state :runner (get-hardware state 1) 0)
      (click-card state :runner (first (:discard (get-runner))))
      (is (= 3 (count (get-program state))) "Three Intis installed")
      (is (= 3 (:click (get-runner))) "Did not gain a click from installing a Program from heap"))
    (testing "Trashing on unsuccessful run"
      (run-on state :hq)
      (run-jack-out state)
      (is (= "Autoscripter" (:title (last (:discard (get-runner))))) "Autoscripter was trashed after successful run"))))

[(deftest basilar-synth
  (do-game
    (new-game {:runner {:hand ["Basilar Synthgland 2KVJ" (qty "Sure Gamble" 4)]}})
    (take-credits state :corp)
    (is (= 4 (:click (get-runner))) "4 base clicks")
    (play-from-hand state :runner "Basilar Synthgland 2KVJ")
    (is (= 2 (:brain-damage (get-runner))) "2 damage taken")
    (is (= 3 (:click (get-runner))))
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 5 (:click (get-runner))) "4+1  base clicks")))]

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Blackguard"
                                 "Snitch"]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Blackguard")
      (is (= 6 (core/available-mu state)) "Runner has 6 MU")
      (play-from-hand state :runner "Snitch")
      (let [iwall (get-ice state :archives 0)]
        (run-on state :archives)
        (click-prompt state :runner "Yes")
        (is (rezzed? (refresh iwall)) "Ice Wall was rezzed"))))

(deftest blackguard-additional-cost-handling-issue-1244
    ;; Additional cost handling, issue #1244
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Archer" "Hostile Takeover"]
                        :credits 15}
                 :runner {:hand ["Blackguard" (qty "Infiltration" 2) "Hernando Cortez"]
                          :credits 100}})
      (play-and-score state "Hostile Takeover")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Archer" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Blackguard")
      (testing "Forfeiting"
        (play-from-hand state :runner "Infiltration")
        (click-prompt state :runner "Expose a card")
        (click-card state :runner "Archer")
        (click-prompt state :corp "No"))
      (testing "Additional credits"
        (play-from-hand state :runner "Hernando Cortez")
        (play-from-hand state :runner "Infiltration")
        (click-prompt state :runner "Expose a card")
        (click-card state :runner "Ice Wall")
        (click-prompt state :corp "No"))))

(deftest bmi-buffer
  (do-game
    (new-game {:runner {:hand ["Abaasy" "BMI Buffer" "Freelance Coding Contract"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "BMI Buffer")
    (play-from-hand state :runner "Freelance Coding Contract")
    (click-card state :runner (find-card "Abaasy" (:hand (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 0 (count (filter #(= (:type %) "Program") (:discard (get-runner))))) "No programs in Heap")
    (is (nil? (get-program state 0)) "Abaasy not installed")
    (let [bmi (get-hardware state 0)]
      (is (= 1 (count (:hosted bmi))) "BMI Buffer hosting Abaasy")
      (is (changed? [(:credit (get-runner)) -2]
            (card-ability state :runner bmi 0)
            (click-prompt state :runner "Abaasy"))
          "BMI Buffer requires install cost paid")
      (is (= 0 (count (:hosted (refresh bmi)))) "BMI Buffer no longer hosting Abaasy")
      (is (= "Abaasy" (:title (get-program state 0))) "Abaasy is installed"))))

(deftest bmi-buffer-2
  ;; BMI Buffer 2 installs ignoring all costs
  (do-game
    (new-game {:runner {:hand ["Abaasy" "BMI Buffer 2" "Freelance Coding Contract"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "BMI Buffer 2")
    (play-from-hand state :runner "Freelance Coding Contract")
    (click-card state :runner (find-card "Abaasy" (:hand (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 0 (count (filter #(= (:type %) "Program") (:discard (get-runner))))) "No programs in Heap")
    (is (nil? (get-program state 0)) "Abaasy not installed")
    (let [bmi (get-hardware state 0)]
      (is (= 1 (count (:hosted bmi))) "BMI Buffer 2 hosting Abaasy")
      (is (changed? [(:credit (get-runner)) 0]
            (card-ability state :runner bmi 0)
            (click-prompt state :runner "Abaasy"))
          "BMI Buffer 2 ignores install cost")
      (is (= 0 (count (:hosted (refresh bmi)))) "BMI Buffer 2 no longer hosting Abaasy")
      (is (= "Abaasy" (:title (get-program state 0))) "Abaasy is installed"))))

(deftest bookmark-click-ability
    ;; Click ability
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                 :runner {:hand ["Bookmark" "Sure Gamble" "Daily Casts" "Brain Chip"]}})
      (take-credits state :corp)
      (click-card state :corp (first (:hand (get-corp))))
      (play-from-hand state :runner "Bookmark")
      (let [bm (get-hardware state 0)]
        (card-ability state :runner bm 0)
        (click-card state :runner "Sure Gamble")
        (click-card state :runner "Daily Casts")
        (click-card state :runner "Brain Chip")
        (is (= 3 (count (:hosted (refresh bm)))) "Bookmark can host cards of any type")
        (card-ability state :runner bm 1)
        (is (not (nil? (refresh bm))) "Bookmark is still installed")
        (is (= 0 (count (:discard (get-runner)))) "Nothing moved to the heap")
        (is (= 3 (count (:hand (get-runner)))) "Bookmark moved all hosted card into the grip"))))

(deftest bookmark-trash-ability
    ;; Trash ability
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                :runner {:hand ["Bookmark" "Sure Gamble" "Daily Casts" "Brain Chip"]}})
      (take-credits state :corp)
      (click-card state :corp (first (:hand (get-corp))))
      (play-from-hand state :runner "Bookmark")
      (let [bm (get-hardware state 0)]
        (card-ability state :runner bm 0)
        (click-card state :runner "Sure Gamble")
        (click-card state :runner "Daily Casts")
        (click-card state :runner "Brain Chip")
        (is (= 3 (count (:hosted (refresh bm)))) "Bookmark can host cards of any type")
        (card-ability state :runner bm 2)
        (is (nil? (refresh bm)) "Bookmark is now trashed")
        (is (= 3 (count (:hand (get-runner)))) "Bookmark moved all hosted card into the grip")
        (is (= 1 (count (:discard (get-runner)))) "Bookmark is only card in heap"))))

(deftest boomerang
  ;; Boomerang
  (do-game
      (new-game {:runner {:deck ["Boomerang"]}
                 :corp {:deck ["Ice Wall" "Hedge Fund"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [icew (get-ice state :hq 0)
            boom (get-hardware state 0)]
        (click-card state :runner icew)
        (run-on state :hq)
        (rez state :corp icew)
        (run-continue state)
        (is (= 0 (count (:discard (get-runner)))) "Heap is empty")
        (card-ability state :runner (refresh boom) 0)
        (click-prompt state :runner "End the run")
        (is (= 1 (count (:discard (get-runner)))) "Boomerang in heap")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= 0 (count (:deck (get-runner)))) "Stack is empty")
        (click-prompt state :runner "Yes")
        (is (= 1 (count (:deck (get-runner)))) "Boomerang in stack")
        (is (= 0 (count (:discard (get-runner)))) "Heap is empty again"))))

(deftest boomerang-does-not-trigger-on-following-successful-runs
    ;; Does not trigger on following successful runs
    (do-game
      (new-game {:runner {:deck ["Boomerang"]}
                 :corp {:deck ["Ice Wall" "Hedge Fund"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [icew (get-ice state :hq 0)
            boom (get-hardware state 0)]
        (click-card state :runner icew)
        (run-on state :hq)
        (rez state :corp icew)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No")
        (run-on state :hq)
        (run-continue state)
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No prompt for shuffling Boomerang in"))))

(deftest boomerang-cannot-use-boomerang-on-other-ice
    ;; Cannot use Boomerang on other ice
    (do-game
      (new-game {:runner {:deck ["Boomerang"]}
                 :corp {:deck ["Ice Wall" "Enigma"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [icew (get-ice state :hq 0)
            enig (get-ice state :hq 1)
            boom (get-hardware state 0)]
        (click-card state :runner icew)
        (run-on state :hq)
        (rez state :corp enig)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (is (no-prompt? state :runner) "Cannot use Boomerang on other ice"))))

(deftest boomerang-assimilator-frees-target-restriction
    ;; Assimilator frees target restriction
    (do-game
      (new-game {:runner {:id "Apex: Invasive Predator"
                          :deck ["Boomerang" "Assimilator"]}
                 :corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (end-phase-12 state :runner)
      (click-card state :runner "Boomerang")
      (play-from-hand state :runner "Assimilator")
      (card-ability state :runner (get-resource state 0) 0)
      (click-card state :runner (-> (get-runner) :rig :facedown first))
      (let [icew (get-ice state :hq 0)
            boom (get-hardware state 0)]
        (run-on state :hq)
        (rez state :corp icew)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (is (not (no-prompt? state :runner)) "Can use Boomerang on ice"))))

(deftest boomerang-update-card-target-on-ice-swap
    ;; Update card-target on ice swap
    (do-game
      (new-game {:runner {:deck ["Boomerang"]}
                 :corp {:deck ["Ice Wall" "Thimblerig"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Thimblerig" "R&D")
      (take-credits state :corp)
      (let [icew (get-ice state :hq 0)
            thim (get-ice state :rd 0)]
        (rez state :corp icew)
        (rez state :corp thim)
        (play-from-hand state :runner "Boomerang")
        (click-card state :runner thim)
        (take-credits state :runner)
        (click-prompt state :corp "Yes")
        (click-card state :corp (refresh icew)))))

(deftest boomerang-update-card-target-on-ice-trash
    ;; Update card-target on ice trash
    (do-game
      (new-game {:runner {:deck ["Boomerang"]}
                 :corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (let [icew (get-ice state :hq 0)]
        (rez state :corp icew)
        (play-from-hand state :runner "Boomerang")
        (click-card state :runner icew)
        (let [boom (get-hardware state 0)]
          (trash state :runner icew)
          (is (nil? (:card-target (refresh boom))) "No more target message")
          (is (some? (get-in (refresh boom) [:special :boomerang-target])) "Still targetting a card")))))

(deftest boomerang-does-not-fire-on-crisium-runs-issue-4734
    ;; Does not fire on Crisium runs. Issue #4734
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Crisium Grid"]}
                 :runner {:deck ["Boomerang"]}})
      (play-from-hand state :corp "Crisium Grid" "HQ")
      (rez state :corp (get-content state :hq 0))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [icew (get-ice state :hq 0)
            boom (get-hardware state 0)]
        (click-card state :runner icew)
        (run-on state :hq)
        (rez state :corp icew)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (click-prompt state :runner "End the run")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No prompt for shuffling Boomerang in"))))

(deftest boomerang-no-second-prompt
    ;; Does not open a second prompt when declining the first time
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Surveyor" "Ice Wall"]}
                 :runner {:deck ["Boomerang"]}})
      (play-from-hand state :corp "Surveyor" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [surveyor (get-ice state :hq 0)
            boom (get-hardware state 0)]
        (click-card state :runner surveyor)
        (run-on state :hq)
        (rez state :corp surveyor)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (click-prompt state :runner "Trace X - End the run")
        (click-prompt state :runner "Trace X - Give the Runner 2 tags")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (is (no-prompt? state :runner) "No second prompt for shuffling Boomerang in"))))

(deftest boomerang-multiple-boomerangs-used-during-single-run-are-shuffled-correctly-issue-5112
    ;; Multiple Boomerangs used during single run are shuffled correctly. Issue #5112
    (do-game
      (new-game {:corp {:deck ["Spiderweb"]}
                 :runner {:deck [(qty "Boomerang" 2) "Paule's Café"]
                          :credits 20}})
      (play-from-hand state :corp "Spiderweb" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Paule's Café")
      (play-from-hand state :runner "Boomerang")
      (let [spiderweb (get-ice state :hq 0)
            boom (get-hardware state 0)
            pau (get-resource state 0)]
        (click-card state :runner spiderweb)
        (card-ability state :runner pau 0)
        (click-card state :runner (find-card "Boomerang" (:hand (get-runner))))
        (run-on state :hq)
        (rez state :corp spiderweb)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (click-prompt state :runner "End the run")
        (click-prompt state :runner "End the run")
        (let [boom2 (find-card "Boomerang" (:hosted (refresh pau)))]
          (card-ability state :runner pau 1)
          (click-card state :runner boom2)
          (click-card state :runner spiderweb)
          (card-ability state :runner (get-hardware state 0) 0)
          (click-prompt state :runner "End the run")
          (run-continue state)
          (run-continue state)
          (is (= 2 (count (:discard (get-runner)))) "Both Boomerangs in heap")
          (click-prompt state :runner "Yes")
          (click-prompt state :runner "Yes")
          (is (= 0 (count (:discard (get-runner)))) "Both Boomerangs shuffled back in stack")))))

(deftest boomerang-boomerang-works-even-when-target-server-changed-5130
    ;; Boomerang works even when target server changed #5130
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Surveyor" "Ice Wall"]}
                 :runner {:id "Omar Keung: Conspiracy Theorist"
                          :deck ["Boomerang"]}})
      (play-from-hand state :corp "Surveyor" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [surveyor (get-ice state :archives 0)
            boom (get-hardware state 0)
            omar (get-in @state [:runner :identity])]
        (click-card state :runner surveyor)
        (card-ability state :runner omar 0)
        (rez state :corp surveyor)
        (run-continue state)
        (card-ability state :runner (refresh boom) 0)
        (click-prompt state :runner "Trace X - End the run")
        (click-prompt state :runner "Trace X - Give the Runner 2 tags")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "HQ")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Yes")
        (is (= "Boomerang" (:title (first (:deck (get-runner)))))))))

(deftest boomerang-boomerang-heap-locked-test
    ;; Boomerang Heap Locked Test
    (do-game
      (new-game {:corp {:deck [(qty "Blacklist" 1)  (qty "Wraparound" 5) ]}
                 :runner {:deck ["Boomerang"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (play-from-hand state :corp "Blacklist" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Boomerang")
      (let [wrap (get-ice state :hq 0)
            brang (get-hardware state 0)
            blist (get-content state :remote1 0)]
        (click-card state :runner wrap)
        (run-on state :hq)
        (rez state :corp (refresh wrap))
        (rez state :corp (refresh blist))
        (run-continue state)
        (card-ability state :runner brang 0)
        (click-prompt state :runner "End the run")
        (run-continue state :movement)
        (run-continue state :success)
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "Boomerang prompt did not come up")
        (is (= 1 (count (:discard (get-runner)))) "Boomerang in heap")
        (is (last-log-contains? state "Runner accesses Wraparound from HQ.")))))

(deftest boomerang-shuffles-any-copy
  ;; shuffles any copy
  (do-game
   (new-game {:runner {:deck [(qty "Boomerang" 2) "Reboot"]}
              :corp {:deck ["Ice Wall" "Hedge Fund"]}})
   (play-from-hand state :corp "Ice Wall" "Archives")
   (take-credits state :corp)
   (play-from-hand state :runner "Boomerang")
   (trash-from-hand state :runner "Boomerang")
   (let [icew (get-ice state :archives 0)
         boom (get-hardware state 0)]
     (click-card state :runner icew)
     (play-from-hand state :runner "Reboot")
     (rez state :corp icew)
     (run-continue state)
     (card-ability state :runner (refresh boom) 0)
     (click-prompt state :runner "End the run")
     (is (= 2 (count (:discard (get-runner)))) "Two copies of Boomerang in heap")
     (run-continue-until state :success)
     (click-card state :runner (get-discarded state :runner 1))
     (click-prompt state :runner "Done")
     (is (= 0 (count (:deck (get-runner)))) "Stack is empty")
     (click-prompt state :runner "Yes")
     (is (= 1 (count (get-runner-facedown state))) "One copy of Boomerang is installed facedown")
     (is (= 1 (count (:deck (get-runner)))) "Boomerang in stack")
     (is (= 0 (count (:discard (get-runner)))) "Heap is empty again"))))

(deftest boomerang-no-prompt-if-no-copies-to-shuffle
  ;; no prompt if no copies to shuffle
  (do-game
   (new-game {:runner {:deck ["Boomerang" "Reboot"]}
              :corp {:deck ["Ice Wall" "Hedge Fund"]}})
   (play-from-hand state :corp "Ice Wall" "Archives")
   (take-credits state :corp)
   (play-from-hand state :runner "Boomerang")
   (let [icew (get-ice state :archives 0)
         boom (get-hardware state 0)]
     (click-card state :runner icew)
     (play-from-hand state :runner "Reboot")
     (rez state :corp icew)
     (run-continue state)
     (card-ability state :runner (refresh boom) 0)
     (click-prompt state :runner "End the run")
     (run-continue-until state :success)
     (click-card state :runner (get-discarded state :runner 0))
     (click-prompt state :runner "Done")
     (is (= 0 (count (:deck (get-runner)))) "Stack is empty")
     (is (no-prompt? state :runner) "The Runner should not be prompted to shuffle Boomerang")
     (is (= 1 (count (get-runner-facedown state))) "One copy of Boomerang is installed facedown")
     (is (= 0 (count (:deck (get-runner)))) "Boomerang in stack")
     (is (= 0 (count (:discard (get-runner)))) "Heap is empty again"))))

(deftest boomerang-konjin-can-be-used-on-forced-encounter-ice
  ;; Boomerang - Targeting Konjin can be used on forced encounter ice
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Konjin" "Ice Wall"]}
              :runner {:hand ["Boomerang"]}})
   (play-from-hand state :corp "Konjin" "R&D")
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Boomerang")
   (let [boomerang (get-hardware state 0)
         konjin (get-ice state :rd 0)
         iw (get-ice state :hq 0)]
     (click-card state :runner (refresh konjin))
     (rez state :corp (refresh konjin))
     (rez state :corp (refresh iw))
     (run-on state :rd)
     (run-continue state :encounter-ice)
     (click-prompt state :corp "0 [Credits]")
     (click-prompt state :runner "1 [Credits]")
     (click-card state :corp (refresh iw))
     (is (same-card? (refresh iw) (core/get-current-ice state)) "The runner should be encountering Ice Wall")
     (card-ability state :runner boomerang 0)
     (click-prompt state :runner "End the run")
     (is (:broken (first (:subroutines (refresh iw)))) "Ice Wall has been broken"))))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
    (new-game {:runner {:deck ["Box-E"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Box-E")
    (is (= 6 (core/available-mu state)))
    (is (= 7 (hand-size :runner)))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
    (new-game {:runner {:deck ["Brain Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Brain Chip")
    (change state :runner :agenda-point -2)
    (is (= 5 (hand-size :runner)) "Hand size unaffected")
    (is (= 4 (core/available-mu state)) "Memory limit unaffected")
    (change state :runner :agenda-point 4)
    (is (= 7 (hand-size :runner)) "Hand size increased by 2")
    (is (= 6 (core/available-mu state)) "Memory limit increased by 2")
    (core/move state :runner (get-hardware state 0) :discard)
    (core/fake-checkpoint state)
    (is (= 5 (hand-size :runner)) "Hand size reset")
    (is (= 4 (core/available-mu state)) "Memory limit reset")))

(deftest buffer-drive-the-player-may-decline-to-move-a-card-to-the-bottom-of-the-stack
    ;; The player may decline to move a card to the bottom of the stack
    (do-game
      (new-game {:runner {:hand ["Buffer Drive" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (trash-from-hand state :runner "Sure Gamble")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-runner)))))))

(deftest buffer-drive-the-player-may-move-one-card-trashed-from-the-grip-by-the-runner-to-the-bottom-of-the-stack
    ;; The player may move one card trashed from the Grip by the Runner to the bottom of the Stack
    (do-game
      (new-game {:runner {:hand ["Buffer Drive" "Corroder" "Yog.0" "Mimic"]
                          :deck ["Stimhack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (let [[target] (:hand (get-runner))]
        (doseq [card (:hand (get-runner))]
          (trash state :runner card))
        (click-prompt state :runner (:title target))
        (is (= 2 (count (:deck (get-runner)))))
        (is (= 2 (count (:discard (get-runner)))))
        (is (find-card (:title target) (:deck (get-runner))))
        (is (not (find-card (:title target) (:discard (get-runner))))))))

(deftest buffer-drive-the-player-may-move-one-card-trashed-from-the-grip-by-the-corp-to-the-bottom-of-the-stack
    ;; The player may move one card trashed from the Grip by the Corp to the bottom of the Stack
    (do-game
      (new-game {:runner {:hand ["Buffer Drive" "Corroder" "Yog.0" "Mimic"]
                          :deck ["Stimhack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (let [[target] (:hand (get-runner))]
        (doseq [card (:hand (get-runner))]
          (trash state :runner card))
        (click-prompt state :runner (:title target))
        (is (= 2 (count (:deck (get-runner)))))
        (is (= 2 (count (:discard (get-runner)))))
        (is (find-card (:title target) (:deck (get-runner))))
        (is (not (find-card (:title target) (:discard (get-runner))))))))

(deftest buffer-drive-the-player-may-move-one-card-trashed-from-the-heap-to-the-bottom-of-the-stack
    ;; The player may move one card trashed from the Heap to the bottom of the Stack
    (do-game
      (new-game {:runner {:hand ["Buffer Drive"]
                          :deck ["Stimhack" "Corroder" "Yog.0" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (let [[target] (take 3 (:deck (get-runner)))]
        (doseq [card (take 3 (:deck (get-runner)))]
          (trash state :runner card))
        (click-prompt state :runner (:title target))
        (is (= 2 (count (:deck (get-runner)))))
        (is (= 2 (count (:discard (get-runner)))))
        (is (find-card (:title target) (:deck (get-runner))))
        (is (not (find-card (:title target) (:discard (get-runner))))))))

(deftest buffer-drive-the-player-may-not-trigger-when-blacklist-is-rezzed
    ;; The player may not trigger when Blacklist is rezzed
    (do-game
      (new-game {:corp {:deck [(qty "Blacklist" 1)  (qty "Wraparound" 5) ]}
                 :runner {:hand ["Buffer Drive"]
                          :deck ["Stimhack" "Corroder" "Yog.0" "Mimic"]
                          :discard ["Stimhack"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (let [bufferdrive (get-hardware state 0)]
        (card-ability state :runner bufferdrive 0)
        (is (no-prompt? state :runner) "Buffer Drive Prompt did not come up"))))

(deftest buffer-drive-after-a-runner-effect-trashes-a-card-a-corp-effect-must-not-cause-buffer-drive-to-trigger-again
    ;; After a runner effect trashes a card, a corp effect must not cause Buffer Drive to trigger again
    (do-game
      (new-game {:runner {:hand ["Buffer Drive" "Corroder" "Yog.0" "Mimic"]
                          :deck ["Stimhack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (trash-from-hand state :runner "Corroder")
      (click-prompt state :runner "Corroder")
      (trash-from-hand state :runner "Yog.0")
      (is (no-prompt? state :runner))))

(deftest buffer-drive-trashing-a-corp-card-must-not-trigger-buffer-drive
    ;; Trashing a corp card must not trigger Buffer Drive
    (do-game
      (new-game {:runner {:hand ["Buffer Drive"]}
                 :corp {:deck [(qty "Aggressive Secretary" 5)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (no-prompt? state :runner))))

(deftest buffer-drive-the-player-may-not-move-a-card-trashed-while-installed-to-the-bottom-of-the-stack
    ;; The player may not move a card trashed while installed to the bottom of the Stack
    (do-game
      (new-game {:runner {:hand ["Buffer Drive" "Spy Camera"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (play-from-hand state :runner "Spy Camera")
      (card-ability state :runner (get-hardware state 1) 1) ; pop spy camera
      (click-prompt state :runner "OK")
      (is (no-prompt? state :runner))
      (is (= 1 (count (:discard (get-runner)))))))

(deftest buffer-drive-buffer-drive-must-not-trigger-a-second-time-in-one-turn
    ;; Buffer Drive must not trigger a second time in one turn
    (do-game
      (new-game {:runner {:hand ["Buffer Drive" "Corroder" "Yog.0" "Mimic"]
                          :deck ["Stimhack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (trash-from-hand state :runner "Corroder")
      (click-prompt state :runner "No action")
      (trash-from-hand state :runner "Yog.0")
      (is (no-prompt? state :runner))
      (is (= 1 (count (:deck (get-runner)))))
      (is (= 2 (count (:discard (get-runner)))))))

(deftest buffer-drive-buffer-drive-must-not-trigger-on-the-second-trash-of-the-turn-if-it-was-installed-after-the-first-trash
    ;; Buffer Drive must not trigger on the second trash of the turn if it was installed after the first trash
    (do-game
      (new-game {:runner {:hand [(qty "Buffer Drive" 3)]}})
      (take-credits state :corp)
      (trash-from-hand state :runner "Buffer Drive")
      (play-from-hand state :runner "Buffer Drive")
      (trash-from-hand state :runner "Buffer Drive")
      (is (no-prompt? state :runner))))

(deftest buffer-drive-the-effect-triggers-on-meat-damage
    ;; The effect triggers on meat damage
    (do-game
      (new-game {:runner {:hand [(qty "Buffer Drive" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (damage state :runner :meat 1)
      (is (not (no-prompt? state :runner)))))

(deftest buffer-drive-the-effect-triggers-on-net-damage
    ;; The effect triggers on net damage
    (do-game
      (new-game {:runner {:hand [(qty "Buffer Drive" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (damage state :runner :net 1)
      (is (not (no-prompt? state :runner)))))

(deftest buffer-drive-the-effect-triggers-on-brain-damage
    ;; The effect triggers on brain damage
    (do-game
      (new-game {:runner {:hand [(qty "Buffer Drive" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (damage state :runner :brain 1)
      (is (not (no-prompt? state :runner)))))

(deftest buffer-drive-the-player-may-remove-buffer-drive-from-the-game-to-move-any-card-in-the-heap-to-the-top-of-the-stack
    ;; The player may remove Buffer Drive from the game to move any card in the Heap to the top of the Stack
    (do-game
      (new-game {:runner {:hand ["Buffer Drive"]
                          :discard ["Sure Gamble" "Stimhack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (card-ability state :runner (get-hardware state 0) 0) ; pop buffer drive
      (let [sure-gamble (find-card "Sure Gamble" (:discard (get-runner)))]
        (click-card state :runner sure-gamble)
        (is (= "Sure Gamble" (:title (first (:deck (get-runner)))))))
      (is (= 0 (count (:hardware (:rig (get-runner))))))
      (is (= 1 (count (:rfg (get-runner)))))
      (is (= 1 (count (:discard (get-runner)))))))

(deftest buffer-drive-the-player-may-cancel-the-remove-from-the-game-effect-of-buffer-drive
    ;; The player may cancel the remove from the game effect of Buffer Drive
    (do-game
      (new-game {:runner {:hand ["Buffer Drive"]
                          :discard ["Sure Gamble" "Stimhack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (card-ability state :runner (get-hardware state 0) 0) ; pop buffer drive
      (click-prompt state :runner "Done")
      (is (no-prompt? state :runner))
      (is (= 1 (count (:hardware (:rig (get-runner))))))
      (is (= 0 (count (:rfg (get-runner)))))
      (is (= 2 (count (:discard (get-runner)))))))

(deftest buffer-drive-corp-trash-install-runner-trash-should-not-cause-buffer-drive-to-trigger
    ;; Corp trash -> install -> Runner trash should not cause Buffer Drive to trigger
    (do-game
      (new-game {:runner {:hand [(qty "Buffer Drive" 5)]}})
      (take-credits state :corp)
      (trash state :corp (first (:hand (get-runner))))
      (play-from-hand state :runner "Buffer Drive")
      (trash state :runner (first (:hand (get-runner))))
      (is (= 2 (count (:discard (get-runner)))))
      (is (no-prompt? state :runner))))

(deftest buffer-drive-runner-trash-install-corp-trash-should-not-cause-buffer-drive-to-trigger
    ;; Runner trash -> install -> Corp trash should not cause Buffer Drive to trigger
    (do-game
      (new-game {:runner {:hand [(qty "Buffer Drive" 5)]}})
      (take-credits state :corp)
      (trash state :runner (first (:hand (get-runner))))
      (play-from-hand state :runner "Buffer Drive")
      (trash state :corp (first (:hand (get-runner))))
      (is (= 2 (count (:discard (get-runner)))))
      (is (no-prompt? state :runner))))

(deftest buffer-drive-doesn-t-activate-on-trashed-corp-card-issue-4908
    ;; Doesn't activate on trashed corp card. Issue #4908
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Archer" "Hostile Takeover"]}
                 :runner {:hand ["Hippo" "Odore" "Buffer Drive"]
                          :credits 50}})
      (play-and-score state "Hostile Takeover")
      (play-from-hand state :corp "Archer" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Odore")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Buffer Drive")
      (click-credit state :runner)
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0) {:expect-rez false})
      (click-card state :corp "Hostile Takeover")
      (run-continue state)
      (card-ability state :runner (get-program state 0) 2)
      (card-ability state :runner (get-program state 0) 2)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "Gain 2 [Credits]")
      (click-prompt state :runner "Trash a program")
      (click-prompt state :runner "Trash a program")
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (no-prompt? state :runner))))

(deftest buffer-drive-trashing-a-played-event-doesn-t-trigger-it-issue-4922
    ;; Trashing a played event doesn't trigger it. Issue #4922
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:hand ["Patchwork" "Buffer Drive"
                                 "Sure Gamble" "Easy Mark"]
                          :credits 20}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (play-from-hand state :runner "Patchwork")
      (play-from-hand state :runner "Sure Gamble")
      (click-card state :runner "Patchwork")
      (click-card state :runner "Easy Mark")
      (is (prompt-is-card? state :runner (get-hardware state 0))
          "Buffer Drive prompt is open")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner has no open prompt")
      (is (not (prompt-is-card? state :runner (get-hardware state 0)))
          "Buffer Drive doesn't open a prompt")))

(deftest buffer-drive-the-effect-triggers-on-nested-interactions
    ;; The effect triggers on nested interactions
    (before-each
      [state (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                                 :hand ["Buffer Drive" "I've Had Worse"]}})
       _ (do (take-credits state :corp)
             (play-from-hand state :runner "Buffer Drive")
             (damage state :runner :meat 1))]
      (testing "Buttons are displayed correctly"
        (do-game state
          (is (= ["Buffer Drive" "I've Had Worse"] (sort (prompt-titles :runner))))))
      (testing "Choosing I've Had Worse"
        (do-game state
          (is (changed? [(count (:hand (get-runner))) 3]
                (click-prompt state :runner "I've Had Worse"))
              "Runner draws 3 cards")
          (is (find-card "I've Had Worse" (:discard (get-runner))))
          (is (= "Choose 1 trashed card to add to the bottom of the stack" (:msg (prompt-map :runner))))
          (is (changed? [(count (:deck (get-runner))) 1]
                (click-prompt state :runner "I've Had Worse"))
              "Runner draws 2 cards, adds 1 card to deck")
          (is (find-card "I've Had Worse" (:deck (get-runner))))
          (is (find-card "Buffer Drive" (get-hardware state)))))
      (testing "Choosing Buffer Drive"
        (do-game state
          (is (changed? [(count (:hand (get-runner))) 0]
                (click-prompt state :runner "Buffer Drive"))
              "Runner draws 0 cards")
          (is (find-card "I've Had Worse" (:discard (get-runner))))
          (is (= "Choose 1 trashed card to add to the bottom of the stack" (:msg (prompt-map :runner))))
          (is (changed? [(count (:deck (get-runner))) 1]
                (click-prompt state :runner "I've Had Worse"))
              "Runner draws 2 cards, adds 1 card to deck")
          (is (find-card "I've Had Worse" (:deck (get-runner))))
          (is (find-card "Buffer Drive" (get-hardware state)))))))

(deftest capstone
  ;; Capstone
  (do-game
    (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                        :hand ["Capstone" (qty "Corroder" 3) (qty "Cache" 2) "Patchwork"]
                        :credits 100}})
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (play-from-hand state :runner "Capstone")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Cache")
    (let [capstone (get-hardware state 0)]
      (card-ability state :runner capstone 0)
      (is (= 4 (count (:hand (get-runner)))) "4 cards in hand before using Capstone")
      (dotimes [n 4]
        (click-card state :runner (nth (:hand (get-runner)) n))))
    (is (= 3 (count (:hand (get-runner)))) "3 cards in hand after using Capstone")))

(deftest capybara-no-ice
    ;; No ice
    (do-game
      (new-game {:runner {:deck ["Capybara"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Capybara")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Capybara installed")
      (is (no-prompt? state :runner) "No prompt")
      (is (empty? (:rfg (get-runner))) "Capybara not RFGed")
      (is (not-empty (get-hardware state)) "Capybara still installed")))

(deftest capybara-single-ice
    ;; Single ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Capybara" "Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Capybara")
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (not-empty (get-hardware state)) "Capybara installed")
      (is (= 1 (count (get-ice state :hq))) "Ice Wall installed")
      (click-prompt state :runner "Yes")
      (is (not (rezzed? (get-ice state :hq 0))) "Ice Wall derezzed")
      (is (= 1 (count (:rfg (get-runner)))) "Capybara RFGed")
      (is (empty? (get-hardware state)) "Capybara removed")))

(deftest carnivore
  ;; Carnivore
  (do-game
      (new-game {:corp {:hand ["Anansi"]}
                 :runner {:hand ["Carnivore" "Sure Gamble" "Aumakua"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Carnivore")
      (is (= 5 (core/available-mu state)) "Gain 1 memory")
      (run-empty-server state "HQ")
      (click-prompt state :runner "[Carnivore] Trash 2 cards from your hand: Trash card")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (click-card state :runner (find-card "Aumakua" (:hand (get-runner))))
      (is (= 1 (count (:discard (get-corp)))))
      (is (zero? (count (:hand (get-runner)))))))

(deftest cataloguer
  (do-game
    (new-game {:corp {:deck ["Whitespace" "Hedge Fund" "Spin Doctor" "Offworld Office"]}
               :runner {:hand ["Cataloguer"]}})
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (is (zero? (count (:hand (get-corp)))))
    (is (= 4 (count (:deck (get-corp)))))
    (play-from-hand state :runner "Cataloguer")

    (is (changed? [(get-counters (get-hardware state 0) :power) 0]
                  (card-ability state :runner (get-hardware state 0) 0))
        "No successful run on R&D this turn, can not use Cataloguer ability yet")

    (run-empty-server state "R&D")

    (is (changed? [(get-counters (get-hardware state 0) :power) -1]
                  (click-prompt state :runner "Cataloguer")))

    (click-prompt state :runner (find-card "Spin Doctor" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Hedge Fund" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Offworld Office" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Whitespace" (:deck (get-corp))))
    ;; try starting over
    (click-prompt state :runner "Start over")
    (click-prompt state :runner (find-card "Whitespace" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Hedge Fund" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Spin Doctor" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Offworld Office" (:deck (get-corp))))
    (click-prompt state :runner "Done")

    (card-ability state :runner (get-hardware state 0) 0)
    (click-prompt state :runner "Steal")

    (is (nil? (get-hardware state 0)) "Cataloguer trashed after running out of power counters")))

(deftest chop-bot-3000
  ;; Chop Bot 3000 - when your turn begins trash 1 card, then draw or remove tag
  (do-game
    (new-game {:runner {:deck ["Sure Gamble"]
                        :hand ["Chop Bot 3000" (qty "Spy Camera" 4)]}})
    (core/gain state :runner :tag 2)
    (take-credits state :corp)
    (play-from-hand state :runner "Chop Bot 3000")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (nil? (:runner-phase-12 @state)) "Does not trigger with no other cards installed")
    (play-from-hand state :runner "Spy Camera")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (true? (:runner-phase-12 @state)) "Does trigger when one other card is installed")
    (play-from-hand state :runner "Spy Camera")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (true? (:runner-phase-12 @state)) "Does trigger when two other cards are installed")
    (is (= 2 (count-tags state)) "Runner has 2 tags")
    (let [chop-bot (get-hardware state 0)]
      (is (empty? (:discard (get-runner))) "No cards in trash")
      (card-ability state :runner chop-bot 0)
      (click-card state :runner (find-card "Spy Camera" (get-hardware state)))
      (click-prompt state :runner "Remove 1 tag")
      (is (find-card "Spy Camera" (:discard (get-runner))) "Spy Camera trashed")
      (is (= 1 (count-tags state)) "Runner lost 1 tag")
      (play-from-hand state :runner "Spy Camera")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner chop-bot 0)
      (click-card state :runner (find-card "Spy Camera" (get-hardware state)))
      (is (changed? [(count (:hand (get-runner))) 1]
            (click-prompt state :runner "Draw 1 card"))
          "Draws 1 card")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (true? (:runner-phase-12 @state)))
      (card-ability state :runner chop-bot 0)
      (click-card state :runner (find-card "Spy Camera" (get-hardware state)))
      (is (zero? (count (:deck (get-runner)))))
      (click-prompt state :runner "Draw 1 card"))))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (do-game
      (new-game {:runner {:deck ["Datasucker" (qty "Clone Chip" 2)]}})
      (take-credits state :corp)
      (trash-from-hand state :runner "Datasucker")
      (play-from-hand state :runner "Clone Chip")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (is (not (no-prompt? state :runner)) "Clone Chip Prompt came up")
        (click-card state :runner "Datasucker")
        (let [ds (get-program state 0)]
          (is (not (nil? ds)))
          (is (= (:title ds) "Datasucker"))))))

(deftest clone-chip-don-t-show-invalid-choices
    ;; don't show invalid choices
    (do-game
      (new-game {:runner {:deck ["Clone Chip"]
                          :discard ["Inti" "Magnum Opus"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Magnum Opus" (:discard (get-runner))))
        (is (nil? (get-program state 0)) "No program was installed")
        (click-card state :runner (find-card "Inti" (:discard (get-runner))))
        (let [inti (get-program state 0)]
          (is (not (nil? inti)) "Program was installed")
          (is (= (:title inti) "Inti") "Program is Inti")
          (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left")))))

(deftest clone-chip-clone-chip-heap-locked-test
    ;; Clone Chip Heap Locked Test
    (do-game
      (new-game {:corp {:deck [(qty "Blacklist" 1)  (qty "Wraparound" 5) ]}
                 :runner {:deck ["Datasucker" (qty "Clone Chip" 2)]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (take-credits state :corp)
      (trash-from-hand state :runner "Datasucker")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (is (= 1 (count (:discard (get-runner)))) "Datasucker in heap")
      (play-from-hand state :runner "Clone Chip")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (is (no-prompt? state :runner) "Clone Chip Prompt did not came up")
        (is (= 1 (count (:discard (get-runner)))) "Datasucker in still in heap"))))


(deftest comet
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game {:runner {:deck [(qty "Comet" 3) (qty "Easy Mark" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Comet")
    (let [comet (get-hardware state 0)]
      (play-from-hand state :runner "Easy Mark")
      (is (true? (:comet-event (get-card state comet)))) ; Comet ability enabled
      (card-ability state :runner comet 0)
      (is (= (:cid comet) (-> (prompt-map :runner) :card :cid)))
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 7 (:credit (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (nil? (:comet-event (get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of a piece of ice until end of turn
  (do-game
    (new-game {:corp {:deck ["Quandary"]}
               :runner {:deck ["Cortez Chip"]}})
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Cortez Chip")
    (let [quan (get-ice state :rd 0)
          cortez (get-hardware state 0)]
      (card-ability state :runner cortez 0)
      (click-card state :runner quan)
      (is (= 1 (count (:discard (get-runner)))) "Cortez Chip trashed")
      (rez state :corp quan)
      (is (= 4 (:credit (get-corp))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cyberdelia
  ;; Cyberdelia
  (do-game
      (new-game {:runner {:deck ["Corroder" "Cyberdelia"]}
                 :corp {:deck ["Ice Wall"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Cyberdelia")
      (let [corr (get-program state 0)
            icew (get-ice state :hq 0)]
        (run-on state :hq)
        (rez state :corp (refresh icew))
        (run-continue state)
        (card-ability state :runner (refresh corr) 0)
        (is (changed? [(:credit (get-runner)) 0]
              (click-prompt state :runner "End the run"))
            "Spent 1, paid 1"))))

(deftest cyberfeeder-pay-credits-prompt-on-install
    ;; Pay-credits prompt on install
    (do-game
      (new-game {:runner {:deck ["Cyberfeeder" "Clot" "Equivocation"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cyberfeeder")
      (play-from-hand state :runner "Equivocation")
      (is (= 1 (:credit (get-runner))) "No pay-credits prompt if non-virus program installed")
      (is (no-prompt? state :runner) "No pay-credits prompt if non-virus program installed")
      (play-from-hand state :runner "Clot")
      (let [cyb (get-hardware state 0)]
        (is (changed? [(:credit (get-runner)) -1]
              (click-card state :runner cyb))
            "Used 1 credit from Cyberfeeder"))))

(deftest cyberfeeder-pay-credits-prompt-on-using-icebreaker
    ;; Pay-credits prompt on using icebreaker
    (do-game
      (new-game {:runner {:deck ["Cyberfeeder" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cyberfeeder")
      (play-from-hand state :runner "Corroder")
      (let [cyb (get-hardware state 0)
            co (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner co 1)
              (click-card state :runner cyb))
            "Used 1 credit from Cyberfeeder"))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game {:runner {:deck [(qty "CyberSolutions Mem Chip" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "CyberSolutions Mem Chip")
    (is (= 6 (core/available-mu state)) "Gain 2 memory")))

(deftest cybsoft-macrodrive-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Cybsoft MacroDrive" "Equivocation"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cybsoft MacroDrive")
      (play-from-hand state :runner "Equivocation")
      (let [cyb (get-hardware state 0)]
        (is (changed? [(:credit (get-runner)) -1]
              (click-card state :runner cyb))
            "Used 1 credit from Cybsoft MacroDrive"))))

(deftest daredevil
  ;; Daredevil
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
               :runner {:deck ["Daredevil" (qty "Sure Gamble" 3) (qty "Easy Mark" 2)]}})
    (starting-hand state :runner ["Daredevil"])
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Daredevil")
    (is (= 6 (core/available-mu state)) "Gained 2 MU")
    (run-on state "HQ")
    (is (empty? (:hand (get-runner))) "No cards drawn")
    (run-jack-out state)
    (run-on state "Archives")
    (run-continue state)
    (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")
    (run-jack-out state)
    (run-on state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "No cards drawn")))

(deftest deep-red
  ;; Deep Red
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Ice Wall"]
                      :credits 100}
               :runner {:hand ["Deep Red" "Rook"]
                        :credit 100}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (rez state :corp (get-ice state :rd 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Deep Red")
    (is (changed? [(:click (get-runner)) -1]
          (play-from-hand state :runner "Rook")
          (click-prompt state :runner "Yes")
          (click-card state :runner "Ice Wall"))
        "Only spend 1 click")
    (is (= ["Rook"] (map get-title (:hosted (get-ice state :rd 0)))))))

(deftest demolisher
  ;; Demolisher
  (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Demolisher"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Demolisher")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 1 (:credit (get-runner))) "Trashed for 3c and gained 1c")))

(deftest demolisher-trash-with-imp
    ;; Trash with Imp
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Imp" "Demolisher"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Imp")
      (play-from-hand state :runner "Demolisher")
      (let [credits (:credit (get-runner))]
        (run-empty-server state :hq)
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (is (= (:credit (get-runner)) (+ 1 credits)) "Demolisher earns a credit when trashing with Imp"))))

(deftest demolisher-gain-one-credit-when-trashing-multiple
  ;; gain one credit when trashing multiple
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                     :hand ["Tithe"]}
              :runner {:deck [(qty "Sure Gamble" 5)]
                       :hand ["Demolisher" "Persephone" "Sure Gamble"]
                       :credits 10}})
   (play-from-hand state :corp "Tithe" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Demolisher")
   (play-from-hand state :runner "Persephone")
   (run-on state "HQ")
   (rez state :corp (get-ice state :hq 0))
   (run-continue state :encounter-ice)
   (fire-subs state (get-ice state :hq 0))
   (run-continue state :movement)
   (is (changed? [(:credit (get-runner)) 1]
         (click-prompt state :runner "Yes"))
       "Demolisher provides only 1 credit though 2 cards were trashed")
   (is (= 2 (count (:discard (get-corp)))))))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game {:runner {:deck [(qty "Desperado" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (is (= 3 (:credit (get-runner))) "Got 1c for successful run on Desperado")))

(deftest devil-charm
  ;; Devil Charm
  (do-game
      (new-game {:runner {:deck ["Devil Charm"]}
                 :corp {:deck ["Enigma"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Devil Charm")
      (run-on state :hq)
      (let [enig (get-ice state :hq 0)]
        (rez state :corp (refresh enig))
        (run-continue state)
        (is (= 2 (get-strength (refresh enig))) "Enigma starts at 2 strength")
        (click-prompt state :runner "Yes")
        (is (= -4 (get-strength (refresh enig))) "Enigma now has -4 strength for the remainder of the run")
        (is (find-card "Devil Charm" (:rfg (get-runner))) "Devil Charm is removed from the game")
        (run-continue state :movement)
        (run-jack-out state)
        (is (= 2 (get-strength (refresh enig))) "Enigma is back at 2 strength"))))

(deftest dinosaurus-hosting-a-breaker-with-strength-based-on-unused-mu-should-calculate-correctly
    ;; Hosting a breaker with strength based on unused MU should calculate correctly
    (do-game
      (new-game {:runner {:deck ["Adept" "Dinosaurus"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dinosaurus")
      (play-from-hand state :runner "Adept")
      (is (= 2 (core/available-mu state)) "2 MU used")
      (let [dino (get-hardware state 0)
            adpt (get-program state 0)]
        (is (= 4 (get-strength (refresh adpt))) "Adept at 4 strength individually")
        (card-ability state :runner dino 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh dino)))]
          (is (= 4 (core/available-mu state)) "0 MU used")
          (is (= 8 (get-strength (refresh hosted-adpt))) "Adept at 8 strength hosted")))))

(deftest dinosaurus-boost-strength-of-hosted-icebreaker-keep-mu-the-same-when-hosting-or-trashing-hosted-breaker
    ;; Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker
    (do-game
      (new-game {:runner {:deck ["Dinosaurus" "Battering Ram"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dinosaurus")
      (let [dino (get-hardware state 0)]
        (card-ability state :runner dino 0)
        (click-card state :runner (find-card "Battering Ram" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (zero? (:credit (get-runner))))
        (is (= 4 (core/available-mu state)) "Battering Ram 2 MU not deducted from available MU")
        (let [ram (first (:hosted (refresh dino)))]
          (is (= 5 (get-strength (refresh ram)))
              "Dinosaurus giving +2 strength to Battering Ram")
          ;; Trash Battering Ram
          (core/move state :runner (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
          (is (= 4 (core/available-mu state))
              "Battering Ram 2 MU not added to available MU when Battering Ram was trashed")))))

(deftest docklands-pass-corp-access-extra-card-on-hq-run
    ;; Corp access extra card on HQ run
    (do-game
      (new-game {:runner {:hand ["Docklands Pass"]}
                 :corp {:hand [(qty "Vanilla" 4)]
                        :deck [(qty "Vanilla" 10)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Docklands Pass")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner done with run after 2 accesses")
      (is (not (:run @state)) "2 access run over")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Second run is over after 1 access")
      (is (not (:run @state)) "1 access run over")))

(deftest docklands-pass-no-bonus-access-when-playing-docklands-pass-after-first-run
    ;; No bonus access when playing Docklands Pass after first run
    (do-game
      (new-game {:runner {:hand ["Docklands Pass"]}
                 :corp {:hand [(qty "Vanilla" 4)]
                        :deck [(qty "Vanilla" 10)]}})
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Runner done with run after 1 accesses")
      (is (not (:run @state)) "1 access run over")
      (play-from-hand state :runner "Docklands Pass")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No bonus access on second run")
      (is (not (:run @state)) "1 access run over")))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
      (new-game {:runner {:deck ["Doppelgänger"]}})
      (core/gain state :corp :bad-publicity 1)
      (take-credits state :corp)
      (play-from-hand state :runner "Doppelgänger")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (:run-credit (get-runner))) "Runner lost BP credits")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "R&D")
      (is (:run @state) "New run started")
      (is (= [:rd] (:server (:run @state))) "Running on R&D")
      (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))

(deftest doppelganger-makers-eye-interaction
    ;; Makers eye interaction
    (do-game
      (new-game {:corp {:deck [(qty "Quandary" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:hand ["Doppelgänger" "The Maker's Eye"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Doppelgänger")
      (play-from-hand state :runner "The Maker's Eye")
      (run-continue state)
      (is (accessing state "Quandary"))
      (click-prompt state :runner "No action")
      (is (accessing state "Quandary"))
      (click-prompt state :runner "No action")
      (is (accessing state "Quandary"))
      (click-prompt state :runner "No action")
      (is (not (:run @state)))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "R&D")
      (is (:run @state) "New run started")
      (run-continue state)
      (is (= [:rd] (:server (:run @state))) "Running on R&D")
      (is (accessing state "Quandary"))
      (click-prompt state :runner "No action")
      (is (not (:run @state)))))

(deftest ^:kaocha/pending doppelganger-inside-job
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Doppelgänger" "Inside Job"]
                        :credits 10}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Doppelgänger")
    (play-from-hand state :runner "Inside Job")
    (click-prompt state :runner "HQ")
    (run-continue state :movement)
    (run-continue state)
    (is (not (:run @state)) "Between the two runs")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "HQ")
    (is (:run @state) "New run started")
    (is (= [:hq] (:server (:run @state))) "Running on R&D")
    (run-continue state)))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game {:corp {:deck ["Snare!"]}
               :runner {:deck ["Dorm Computer"]}})
    (play-from-hand state :corp "Snare!" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-hardware state 0)]
      (is (changed? [(count-tags state) 0
                     (get-counters (refresh dorm) :power) -1]
            (card-ability state :runner dorm 0)
            (click-prompt state :runner "Server 1")
            (run-continue state)
            (is (= "Snare!" (:title (:card (prompt-map :corp)))))
            (is (= :waiting (prompt-type :runner)) "Runner has prompt to wait for Snare!")
            (click-prompt state :corp "Yes"))
          "No tags, spent a power counter"))))

(deftest dyson-fractal-generator-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Dyson Fractal Generator" "Blackstone"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Dyson Fractal Generator")
      (play-from-hand state :runner "Blackstone")
      (let [dfg (get-hardware state 0)
            bs (get-program state 0)]
        (is (changed? [(:credit (get-runner)) -2]
              (card-ability state :runner bs 1)
              (click-card state :runner dfg))
            "Used 1 credit from Dyson Fractal Generator"))))

(deftest dzmz-optimizer
  ;; DZMZ Optimizer
  (do-game
      (new-game {:corp {:hand ["Hedge Fund"]}
                 :runner {:hand ["DZMZ Optimizer" (qty "Aumakua" 2)]
                          :credits 20}})
      (take-credits state :corp)
      (play-from-hand state :runner "DZMZ Optimizer")
      (is (= 5 (core/available-mu state)) "Gain 1 memory")
      (is (changed? [(:credit (get-runner)) -2]
            (play-from-hand state :runner "Aumakua"))
          "Pays 2 credit for first install")
      (is (changed? [(:credit (get-runner)) -3]
            (play-from-hand state :runner "Aumakua"))
          "Pays 3 credit for second install")))

(deftest endurance
  ;; Endurance
  (do-game
    (new-game {:runner {:hand ["Endurance"] :credits 10}
               :corp {:hand ["Ice Wall"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Endurance")
    (let [end (get-hardware state 0)]
      (is (= 3 (get-counters (refresh end) :power)) "starts with 3 counters")
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner (refresh end) 0)
      (click-prompt state :runner "End the run")
      (is (= 1 (get-counters (refresh end) :power)) "spend 2 counters to break")
      (run-continue state)
      (run-continue state)
      (is (= 2 (get-counters (refresh end) :power)) "gained 1 counter from a successful run"))))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game {:corp {:deck ["Data Mine"
                             "Cerebral Overwriter"
                             "Mushin No Shin"]}
               :runner {:deck [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Mushin No Shin")
    (click-card state :corp (find-card "Cerebral Overwriter" (:hand (get-corp))))
    (play-from-hand state :corp "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (is (= 3 (get-counters (refresh co) :advancement)) "3 advancements on Overwriter")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Feedback Filter")
      (is (= 7 (:credit (get-runner))))
      (let [ff (get-hardware state 0)]
        (run-on state "Server 1")
        (rez state :corp dm)
        (run-continue state)
        (card-subroutine state :corp dm 0)
        (card-ability state :runner ff 0)
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (is (= 4 (:credit (get-runner))))
        (run-continue state)
        (click-prompt state :corp "Yes") ; pay 3 to fire Overwriter
        (card-ability state :runner ff 1)
        (click-prompt state :runner "Done")
        (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-runner))) "2 of the 3 core damage prevented")
        (is (= 2 (count (:hand (get-runner)))))
        (is (empty? (get-hardware state)) "Feedback Filter trashed")))))

(deftest flame-out-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:runner {:deck ["Flame-out" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (click-card state :runner (find-card "Mimic" (:hand (get-runner))))
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (card-ability state :runner fo 0)
        (is (= 3 (:credit (get-runner))) "Runner gains 1 credit")
        (is (= 8 (get-counters (refresh fo) :credit)) "Took 1 credit from Flame-out")
        (take-credits state :runner)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed")
        (is (= 1 (count (:discard (get-runner)))) "Mimic in trash"))))

(deftest flame-out-corp-turn-usage
    ;; Corp turn usage
    (do-game
      (new-game {:runner {:deck ["Flame-out" "Mimic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (click-card state :runner (find-card "Mimic" (:hand (get-runner))))
        (take-credits state :runner)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (card-ability state :runner fo 1)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 11 (:credit (get-runner))) "Runner gains 9 credit")
        (is (zero? (get-counters (refresh fo) :credit)) "Took all credits from Flame-out")
        (take-credits state :corp)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed"))))

(deftest flame-out-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Owl"]}
                 :runner {:deck ["Flame-out" "Mimic"]}})
      (play-from-hand state :corp "Owl" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (click-card state :runner "Mimic")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (run-on state "HQ")
        (rez state :corp (get-ice state :hq 0))
        (run-continue state)
        (card-ability state :runner (first (:hosted (refresh fo))) 0)
        (click-prompt state :runner "Add installed program to the top of the stack")
        (click-card state :runner fo)
        (is (= 2 (:credit (get-runner))) "Runner has not paid any credits from their credit pool")
        (take-credits state :runner)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed"))))

(deftest flame-out-pump-abilities-don-t-disappear-when-card-is-hosted-4770
    ;; Pump abilities don't disappear when card is hosted #4770
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Owl"]}
                 :runner {:hand ["Flame-out" "Cybertrooper Talut" "Mimic"]
                          :credits 20}})
      (play-from-hand state :corp "Owl" "HQ")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Cybertrooper Talut")
      (play-from-hand state :runner "Flame-out")
      (play-from-hand state :runner "Mimic")
      (let [mimic-strength (get-strength (get-program state 0))
            fo (get-hardware state 0)]
        (card-ability state :runner fo 3)
        (click-card state :runner "Mimic")
        (is (= mimic-strength (get-strength (first (:hosted (get-hardware state 0)))))))))

(deftest flip-switch-trace-reaction-ability
    ;; Trace reaction ability
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["SEA Source" "IP Block"]}
                 :runner {:hand ["Flip Switch"]}})
      (play-from-hand state :corp "IP Block" "HQ")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (play-from-hand state :runner "Flip Switch")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (is (prompt-is-type? state :runner :waiting) "Runner shouldn't get to use Flip Switch on Corp's turn")
      (is (= 3 (:base (prompt-map :corp))) "Base trace should be base 3")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner should gain a tag from not beating trace")
      (take-credits state :corp)
      (let [ip (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp ip)
        (run-continue state)
        (card-subroutine state :corp ip 0))
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Flip Switch")
      (click-prompt state :runner "Yes")
      (is (zero? (:base (prompt-map :corp))) "Base trace should now be 0")
      (is (find-card "Flip Switch" (:discard (get-runner))) "Flip Switch should be in Heap")
      (click-prompt state :corp "0")
      (click-prompt state :runner "3")
      (is (= 1 (count-tags state)) "Runner should not gain a tag from beating trace")))

(deftest flip-switch-jack-out-ability
    ;; Jack out ability
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Flip Switch"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flip Switch")
      (let [flip (get-hardware state 0)]
        (card-ability state :runner (get-hardware state 0) 0)
        (is (refresh flip) "Flip Switch hasn't been trashed")
        (run-on state "HQ")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (= "Runner jacks out." (-> @state :log last :text)))
        (is (nil? (refresh flip)) "Flip Switch has been trashed")
        (is (find-card "Flip Switch" (:discard (get-runner)))))))

(deftest flip-switch-tag-losing-ability
    ;; Tag losing ability
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Flip Switch"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flip Switch")
      (is (zero? (count-tags state)) "Runner starts with 0 tags")
      (let [flip (get-hardware state 0)]
        (card-ability state :runner flip 1)
        (is (refresh flip) "Flip Switch hasn't been trashed")
        (gain-tags state :runner 1)
        (is (= 1 (count-tags state)) "Runner starts with 0 tags")
        (card-ability state :runner flip 1)
        (is (zero? (count-tags state)) "Runner has lost 1 tag")
        (is (nil? (refresh flip)) "Flip Switch has been trashed")
        (is (find-card "Flip Switch" (:discard (get-runner)))))))

(deftest flip-switch-trace-reaction-ability-issue-4746
    ;; Trace reaction ability. Issue #4746
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["SEA Source" "IP Block"]}
                 :runner {:id "Armand \"Geist\" Walker: Tech Lord"
                          :deck [(qty "Sure Gamble" 5)]
                          :hand ["Flip Switch" "Tech Trader"]}})
      (play-from-hand state :corp "IP Block" "HQ")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (play-from-hand state :runner "Tech Trader")
      (play-from-hand state :runner "Flip Switch")
      (take-credits state :runner)
      (take-credits state :corp)
      (let [ip (get-ice state :hq 0)]
        (run-on state "HQ")
        (rez state :corp ip)
        (run-continue state)
        (card-subroutine state :corp ip 0))
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Flip Switch")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Armand \"Geist\" Walker: Tech Lord")
      (is (= 0 (:base (prompt-map :corp))) "Base trace should now be 0")))

(deftest friday-chip
  ;; Friday Chip - gain counters for trashing cards, move a counter on turn start
  (do-game
    (new-game {:corp {:deck ["Adonis Campaign" "Hedge Fund"]}
               :runner {:deck ["Friday Chip" "Aumakua"]}})
    (play-from-hand state :corp "Adonis Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 20)
    (play-from-hand state :runner "Friday Chip")
    (play-from-hand state :runner "Aumakua")
    (let [fc (get-hardware state 0)
          aum (get-program state 0)]
      (is (zero? (get-counters fc :virus)) "Friday Chip starts with 0 counters")
      (is (zero? (get-counters aum :virus)) "Auakua starts with 0 counters")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 3 [Credits] to trash") ; trash Adonis Campaing
      (click-prompt state :runner "Yes") ; gain virus counter
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip gains a counter on trash")
      (is (zero? (get-counters (refresh aum) :virus)) "Aumakua doesn't gain a counter")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip doesn't gain a counter on non-trash")
      (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gains a counter on non-trash")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-card state :runner aum)
      (is (= 2 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
      (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip lost 1 counter"))))

(deftest gachapon
  ;; Gachapon
  (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Clone Chip" "DDoS" "Equivocation" "Falsified Credentials" "Gachapon"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "DDoS" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Equivocation" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Falsified Credentials" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B C D E F
      (play-from-hand state :runner "Gachapon")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (last-log-contains? state "Au Revoir, Bankroll, Clone Chip, DDoS, Equivocation, and Falsified Credentials") "Shown correct six cards")
      (click-prompt state :runner "OK")
      (is (not (no-prompt? state :corp)) "Corp has waiting prompt")
      (is (= 1 (count (:discard (get-runner)))) "Gachapon in heap")
      (is (= 0 (count (:deck (get-runner)))) "0 cards in deck")
      (is (= 6 (count (:set-aside (get-runner)))) "6 cards set-aside")
      (is (changed? [(:credit (get-runner)) -1]
            (click-prompt state :runner "DDoS"))
          "Paid 1c to install DDoS")
      (is (= 5 (count (:set-aside (get-runner)))) "5 cards remain set-aside")
      (click-prompt state :runner "Au Revoir")
      (click-prompt state :runner "Clone Chip")
      (click-prompt state :runner "Equivocation")
      (click-prompt state :runner "Start over")
      (click-prompt state :runner "Au Revoir")
      (click-prompt state :runner "Bankroll")
      (click-prompt state :runner "Clone Chip")
      (is (= 1 (count (:discard (get-runner)))) "Still just Gachapon in heap")
      (is (= 0 (count (:rfg (get-runner)))) "No cards removed from game")
      (click-prompt state :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Still just Gachapon in heap")
      (is (= 3 (count (:deck (get-runner)))) "3 cards remain in deck")
      (is (= 2 (count (:rfg (get-runner)))) "Removed 2 cards from game")
      (is (no-prompt? state :runner) "No more prompts")
      (is (no-prompt? state :corp) "Waiting prompt cleared")))

(deftest gachapon-shuffling-with-less-than-3-cards-set-aside
    ;; Shuffling with less than 3 cards set aside
    (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Clone Chip" "Gachapon"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B C
      (play-from-hand state :runner "Gachapon")
      (card-ability state :runner (get-hardware state 0) 0)
      (click-prompt state :runner "OK")
      (is (not (no-prompt? state :corp)) "Corp has waiting prompt")
      (is (changed? [(:credit (get-runner)) 0]
            (click-prompt state :runner "Bankroll"))
          "Paid 0c to install Bankroll")
      (click-prompt state :runner "Au Revoir")
      (click-prompt state :runner "Clone Chip")
      (click-prompt state :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Just Gachapon in heap")
      (is (= 2 (count (:deck (get-runner)))) "2 cards remain in deck")
      (is (= 0 (count (:rfg (get-runner)))) "Removed no cards from game")
      (is (no-prompt? state :runner) "No more prompts")
      (is (no-prompt? state :corp) "Waiting prompt cleared")))

(deftest gachapon-no-programs-or-virtual-resources-are-revealed-issue-4826
    ;; No programs or virtual resources are revealed. Issue #4826
    (do-game
      (new-game {:runner {:hand ["Acacia" "Blackmail" "Capstone" "Daredevil" "Easy Mark" "Frame Job" "Gachapon"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Acacia" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Blackmail" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Capstone" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Daredevil" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Frame Job" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B C
      (play-from-hand state :runner "Gachapon")
      (card-ability state :runner (get-hardware state 0) 0)
      (click-prompt state :runner "OK")
      (is (= ["Done"] (prompt-buttons :runner)) "Runner can't choose ineligable cards")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "Acacia")
      (click-prompt state :runner "Blackmail")
      (click-prompt state :runner "Capstone")
      (click-prompt state :runner "Done")
      (is (= 3 (count (:deck (get-runner)))) "3 card back in deck")
      (is (= 3 (count (:rfg (get-runner)))) "3 card removed from the game")))

(deftest gachapon-gachapon-credits-for-installs-issue-4888
    ;; Gachapon + Credits for Installs. Issue #4888
    (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Clone Chip" "DDoS" "Equivocation" "Falsified Credentials" "Gachapon" "Paladin Poemu"]
                          :credits 1}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "DDoS" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Equivocation" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Falsified Credentials" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B C D E F
      (play-from-hand state :runner "Paladin Poemu")
      (let [pp (get-resource state 0)]
        (core/add-counter state :runner (refresh pp) :credit 2)
        (play-from-hand state :runner "Gachapon")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (last-log-contains? state "Au Revoir, Bankroll, Clone Chip, DDoS, Equivocation, and Falsified Credentials") "Shown correct six cards")
        (click-prompt state :runner "OK")
        (is (not (no-prompt? state :corp)) "Corp has waiting prompt")
        (is (= 1 (count (:discard (get-runner)))) "Gachapon in heap")
        (is (= 6 (count (:set-aside (get-runner)))) "6 cards in deck")
        (click-prompt state :runner "DDoS")
        (click-card state :runner pp)
        (is (= 0 (:credit (get-runner))) "DDoS installed with 2c discount using only Paladin Poemu credits")
        (is (= "DDoS" (:title (get-resource state 1))) "DDoS is installed"))))

(deftest gachapon-interaction-with-reaver-5061
    ;; Interaction with Reaver #5061
    (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Clone Chip" "DDoS" "Equivocation" "Falsified Credentials" "Golden" "Gachapon" "Reaver"]
                          :credits 10}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "DDoS" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Equivocation" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Falsified Credentials" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Golden" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B C D E F G
      (play-from-hand state :runner "Reaver")
      (play-from-hand state :runner "Gachapon")
      (is (zero? (count (:hand (get-runner)))))
      (card-ability state :runner (get-hardware state 0) 0)
      (is (= 1 (count (:hand (get-runner)))))
      (is (= "Au Revoir" (:title (first (:hand (get-runner))))) "Runner drew from Reaver")
      (is (last-log-contains? state "Bankroll, Clone Chip, DDoS, Equivocation, Falsified Credentials, and Golden") "Shown correct six cards")
      (click-prompt state :runner "OK")
      (is (prompt-is-type? state :corp :waiting))
      (is (= 1 (count (:discard (get-runner)))) "Gachapon in heap")
      (is (= 6 (count (:set-aside (get-runner)))) "6 cards in deck")
      (click-prompt state :runner "DDoS")
      (is (= "DDoS" (:title (get-resource state 0))) "DDoS is installed")))

(deftest gebrselassie
  ;; Gebrselassie
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Fire Wall" 2)]
                      :credits 20}
               :runner {:hand ["Corroder" "Bukhgalter" "Gebrselassie"]
                        :credits 20}})
    (play-from-hand state :corp "Fire Wall" "HQ")
    (play-from-hand state :corp "Fire Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :hq 1))
    (take-credits state :corp)
    (core/gain state :runner :click 5)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Bukhgalter")
    (play-from-hand state :runner "Gebrselassie")
    (let [cor (get-program state 0)
          bukh (get-program state 1)
          geb (get-hardware state 0)]
      (card-ability state :runner geb 0)
      (click-card state :runner cor)
      (run-on state :hq)
      (is (= 2 (get-strength (refresh cor))) "Corroder starts with 2 strength")
      (is (= 1 (get-strength (refresh bukh))) "Bukhgalter starts with 1 strength")
      (run-continue state)
      (card-ability state :runner cor 1)
      (card-ability state :runner cor 1)
      (card-ability state :runner cor 1)
      (is (= 5 (get-strength (refresh cor))) "Corroder has 5 strength")
      (card-ability state :runner bukh 1)
      (is (= 2 (get-strength (refresh bukh))) "Bukhgalter has 2 strength")
      (card-ability state :runner cor 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (is (= 5 (get-strength (refresh cor))) "Corroder still has 5 strength")
      (is (= 1 (get-strength (refresh bukh))) "Bukhgalter has reset to 1")
      (run-jack-out state)
      (is (= 5 (get-strength (refresh cor))) "Corroder still has 5 strength"))))

(deftest ghosttongue
  (do-game
    (new-game {:runner {:hand [(qty "Sure Gamble" 2) "Ghosttongue"] :credits 6}})
    (take-credits state :corp)
    (play-from-hand state :runner "Ghosttongue")
    (is (= 1 (:brain-damage (get-runner))) "1 brain from install")
    (is (changed? [(:credit (get-runner)) 5]
          (play-from-hand state :runner "Sure Gamble"))
        "net +5c for sure gamble")))

(deftest gpi-net-tap
  (do-game
   (new-game {:runner {:hand ["GPI Net Tap"]}
              :corp {:hand ["Ice Wall"]}})
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "GPI Net Tap")
   (let [gpi (get-hardware state 0)
         iw (get-ice state :hq 0)]
     ;; expose and jack out
     (run-on state :hq)
     (card-ability state :runner gpi 0)
     (is (last-log-contains? state "exposes Ice Wall") "Expose approached ice")
     (is (= "Jack out?" (:msg (prompt-map :runner))) "Runner offered to jack out")
     (click-prompt state :runner "Yes")
     (is (nil? (get-run)) "Run has ended")
     ;; expose and continue
     (run-on state :hq)
     (card-ability state :runner gpi 0)
     (click-prompt state :runner "No")
     (rez state :corp iw)
     (card-ability state :runner gpi 0)
     (is (not (last-log-contains? state "exposes Ice Wall")) "Cannot use ability since ice is rezzed")
     (is (no-prompt? state :runner)))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus programs
  (do-game
    (new-game {:runner {:deck ["Grimoire" "Imp"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Grimoire")
    (is (= 6 (core/available-mu state)) "Gained 2 MU")
    (play-from-hand state :runner "Imp")
    (let [imp (get-program state 0)]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest heartbeat
  ;; Heartbeat - +1 MU, trash installed card to prevent 1 damage
  (do-game
    (new-game {:runner {:id "Apex: Invasive Predator"
                        :deck [(qty "Heartbeat" 2) (qty "Sure Gamble" 2) "Cache"]}})
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
    (play-from-hand state :runner "Heartbeat")
    (is (= 5 (core/available-mu state)) "Gained 1 MU")
    (play-from-hand state :runner "Cache")
    (let [hb (get-hardware state 0)
          cache (get-program state 0)
          hbdown (get-runner-facedown state 0)]
      (damage state :corp :net 1)
      (is (= (:msg (prompt-map :runner))
             "Prevent 1 net damage?")
          "Damage prevention message correct.")
      (card-ability state :runner hb 0)
      (click-card state :runner cache)
      (is (= 1 (count (:discard (get-runner)))) "Prevented 1 net damage")
      (is (= 2 (count (:hand (get-runner)))))
      (is (second-last-log-contains? state "Runner trashes 1 installed card \\(Cache\\) to use Heartbeat to prevent 1 damage\\."))
      (damage state :corp :net 3)
      (is (= (:msg (prompt-map :runner))
             "Prevent any of the 3 net damage?")
          "Damage prevention message correct.")
      (card-ability state :runner hb 0)
      (click-card state :runner hbdown)
      (is (= (:msg (prompt-map :runner))
             "Prevent any of the 3 net damage? (1/3 prevented)")
          "Damage prevention message correct.")
      (click-prompt state :runner "Done")
      (is (= 4 (count (:discard (get-runner)))) "Prevented 1 of 3 net damage; used facedown card")
      (is (last-n-log-contains? state 2 "Runner trashes 1 installed card \\(a facedown card\\) to use Heartbeat to prevent 1 damage\\.")))))

(deftest hermes
    (do-game
      (new-game {:corp {:deck ["Project Atlas" "Hostile Takeover" "PAD Campaign"]}
                 :runner {:hand ["Hermes"]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Hermes")
      (take-credits state :runner)
      (score-agenda state :corp (get-content state :remote2 0))
      (is (changed? [(count (:hand (get-corp))) 1]
            (click-card state :runner (get-content state :remote3 0)))
          "PAD Campaign returned to hand")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (is (changed? [(count (:hand (get-corp))) 1]
            (click-card state :runner (get-content state :remote4 0)))
          "PAD Campaign returned to hand")))

(deftest hermes-public-agenda
  (do-game
   (new-game {:runner {:hand ["Hermes"]}
              :corp {:hand ["Ice Wall" "Oaktown Renovation" "Oaktown Renovation"]}})
   (play-from-hand state :corp "Oaktown Renovation" "New remote")
   (play-from-hand state :corp "Oaktown Renovation" "New remote")
   (play-from-hand state :corp "Ice Wall" "Server 1")
   (take-credits state :corp)
   (play-from-hand state :runner "Hermes")
   (run-empty-server state :remote2)
   (click-prompt state :runner "Steal")
   (click-card state :runner (get-content state :remote1 0))
   (is (= 0 (count (:hand (get-corp)))) "Hermes can not bounce Public agenda")
   (click-card state :runner (get-ice state :remote1 0))
   (is (= 1 (count (:hand (get-corp)))) "Hermes can bounce facedown ice")))

(deftest hijacked-router-run-on-archives
    ;; Run on Archives
    (do-game
      (new-game {:runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp ends turn with 8 credits")
      (play-from-hand state :runner "Hijacked Router")
      (run-empty-server state :archives)
      (is (not-empty (get-hardware state)) "Hijacked Router installed")
      (is (= "Hijacked Router" (-> (prompt-map :runner) :card :title)) "Prompt for using Hijacked Router")
      (click-prompt state :runner "Yes")
      (is (empty? (get-hardware state)) "Hijacked Router is not installed")
      (is (find-card "Hijacked Router" (:discard (get-runner))) "Hijacked Router was trashed")
      (is (= 5 (:credit (get-corp))) "Corp lost 3 credits")
      (is (not (:run @state)) "Run is finished")))

(deftest hijacked-router-run-on-hq
    ;; Run on HQ
    (do-game
      (new-game {:runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp ends turn with 8 credits")
      (play-from-hand state :runner "Hijacked Router")
      (run-empty-server state :hq)
      (is (not-empty (get-hardware state)) "Hijacked Router installed")
      (is (= "Hedge Fund" (-> (prompt-map :runner) :card :title)) "No prompt to use Hijacked Router")
      (is (not-empty (get-hardware state)) "Hijacked Router is installed")
      (is (not (find-card "Hijacked Router" (:discard (get-runner)))) "Hijacked Router was not trashed")
      (is (= 8 (:credit (get-corp))) "Corp has not lost 3 credits")))

(deftest hijacked-router-credit-loss-on-server-creation
    ;; Credit loss on server creation
    (do-game
      (new-game {:corp {:deck ["Elective Upgrade"]}
                 :runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hijacked Router")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts turn with 8 credits")
      (play-from-hand state :corp "Elective Upgrade" "New remote")
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credit from server creation")))

(deftest hippo-no-ice
    ;; No ice
    (do-game
      (new-game {:runner {:deck ["Hippo"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (no-prompt? state :runner) "No prompt")
      (is (empty? (:rfg (get-runner))) "Hippo not RFGed")
      (is (not-empty (get-hardware state)) "Hippo still installed")))

(deftest hippo-single-ice
    ;; Single ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Corroder" "Hippo"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 1 (count (get-ice state :hq))) "Ice Wall installed")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (empty? (get-ice state :hq)) "Ice Wall removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall trashed")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed")))

(deftest hippo-multiple-ice
    ;; Multiple ice
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Enigma"]}
                 :runner {:deck ["Corroder" "Hippo"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 2 (count (get-ice state :hq))) "2 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall outermost")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma innermost")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (is (= 1 (count (get-ice state :hq))) "Ice removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice trashed")
      (is (= "Ice Wall" (:title (first (:discard (get-corp))))) "Ice Wall in trash")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma still innermost")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed")))

(deftest hippo-doesn-t-fire-on-partial-break
    ;; Doesn't fire on partial break
    (do-game
      (new-game {:corp {:deck ["Battlement"]}
                 :runner {:deck ["Corroder" "Hippo"]
                          :credits 10}})
      (play-from-hand state :corp "Battlement" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 1 (count (get-ice state :hq))) "Battlement installed")
      (card-ability state :runner (get-program state 0) 1)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Done")
      (run-continue state)
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (no-prompt? state :runner) "no prompt")))

(deftest hippo-can-t-be-used-after-first-ice-issue-4792
    ;; Can't be used after first ice. Issue #4792
    (do-game
      (new-game {:corp {:hand [(qty "Ice Wall" 2)]}
                 :runner {:hand ["Corroder" "Hippo"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (get-ice state :hq 1) "Ice Wall installed")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "No")
      (is (get-ice state :hq 1) "Ice Wall is not removed")
      (run-continue-until state :approach-ice)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (is (no-prompt? state :runner) "No Hippo prompt on later ice")))

(deftest hippo-interaction-with-nfr-4782
    ;; Interaction with Nfr #4782
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Nfr" "Hippo"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Nfr")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Yes")
      (run-continue state)
      (is (= 1 (get-counters (get-program state 0) :power)) "Nfr gains 1 counter")))

(deftest hippo-can-t-be-used-after-first-ice-on-another-server-issue-4970
    ;; Can't be used after first ice on another server. Issue #4970
    (do-game
      (new-game {:corp {:hand ["Ice Wall" "Vanilla"]}
                 :runner {:hand ["Corroder" "Hippo"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Vanilla" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (play-from-hand state :runner "Corroder")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (get-ice state :hq 0) "Ice Wall installed")
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "No")
      (is (get-ice state :hq 0) "Ice Wall is not removed")
      (run-continue state)
      (run-continue state)
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (is (no-prompt? state :runner) "No Hippo prompt on later ice")))

(deftest hippocampic-mechanocytes
  ;; Hippocampic Mechanocytes
  (do-game
    (new-game {:runner {:hand ["Hippocampic Mechanocytes" (qty "Stoneship Chart Room" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Hippocampic Mechanocytes")
    (is (= 1 (count (:discard (get-runner)))) "Suffered 1 meat damage")
    (let [hm (get-hardware state 0)]
      (is (= 2 (get-counters (refresh hm) :power)))
      (is (= 7 (hand-size :runner)))
      (play-from-hand state :runner "Stoneship Chart Room")
      (card-ability state :runner (get-resource state 0) 1)
      (click-card state :runner hm)
      (is (= 8 (hand-size :runner)))
      (core/add-counter state :runner (refresh hm) :power -3)
      (core/update-hand-size state :runner)
      (is (= 5 (hand-size :runner))))))

(deftest jeitinho
  (do-game
    (new-game {:runner {:hand [(qty "Jeitinho" 3)]}
               :corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hedge Fund"]}})
    (dotimes [n 3]
      (take-credits state :corp)
      (play-from-hand state :runner "Jeitinho")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (is (= (+ 1 n) (count (:scored (get-runner)))) "Jeitinho moved to score area")
      (is (zero? (:agenda-point (get-runner))) "Jeitinho scored for 0 agenda point"))
    (is (= "assassination plot (Jeitinho)" (:reason @state)) "Win condition reports jeitinho")))

(deftest jeitinho-threat
  (do-game
    (new-game {:runner {:discard ["Jeitinho"]}
               :corp {:hand ["Authenticator" "Bellona"]}})
    (play-from-hand state :corp "Authenticator" "Archives")
    (take-credits state :corp)
    (run-on state "Archives")
    (rez state :corp (get-ice state :archives 0))
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (no-prompt? state :runner) "No prompt when under threat level")
    (run-jack-out state)
    (take-credits state :runner)
    (play-and-score state "Bellona")
    (take-credits state :corp)
    (run-on state "Archives")
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (changed? [(count (:discard (get-runner))) -1
                   (count (get-hardware state)) 1]
                  (click-prompt state :runner "Yes"))
        "Jeitinho was installed from the heap")))

(deftest keiko
  ;; Keiko
  (do-game
      (new-game {:runner {:deck ["Mystic Maemi" "Keiko" "Sure Gamble"]}})
      (take-credits state :corp)
      (is (changed? [(:credit (get-runner)) -2]
            (play-from-hand state :runner "Keiko"))
          "Got 1c back from installing Keiko")
      (is (changed? [(:credit (get-runner)) -1]
            (play-from-hand state :runner "Mystic Maemi"))
          "Only triggers once per turn")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (changed? [(:credit (get-runner)) 6]
            (play-from-hand state :runner "Sure Gamble")
            (click-card state :runner (get-resource state 0)))
          "Paid 4 for Sure Gamble. Got 9 from Sure Gamble and 1 from Keiko")))

(deftest keiko-does-not-fire-on-second-trigger
    ;; Does not fire on second trigger
    (do-game
      (new-game {:runner {:deck ["Mystic Maemi" "Keiko" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Mystic Maemi")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (changed? [(:credit (get-runner)) 5]
            (play-from-hand state :runner "Sure Gamble")
            (click-card state :runner (get-resource state 0)))
          "Paid 4 for Sure Gamble. Got 9 from Sure Gamble")
      (is (changed? [(:credit (get-runner)) -3]
            (play-from-hand state :runner "Keiko"))
          "Paid full 3c for Keiko")))

(deftest keiko-does-not-fire-on-non-companion-cards-issue-4705
    ;; Does not fire on non-Companion cards. Issue #4705
    (do-game
      (new-game {:runner {:hand ["Keiko" "Mystic Maemi" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Keiko")
      (is (changed? [(:credit (get-runner)) -2]
            (play-from-hand state :runner "Corroder"))
          "Triggers only on Companions")))

(deftest keiko-issue-5892-does-not-fire-a-second-time
    ;; Issue #5892: Does not fire a second time
    (testing "when a companion is installed and then credits from a companion are used"
      (do-game
        (new-game {:runner {:hand ["Keiko" "Trickster Taka"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Keiko")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (changed? [(:credit (get-runner)) 0]
              (play-from-hand state :runner "Trickster Taka"))
            "Got 1c back from installing Trickster Taka")
        (let [tt (get-resource state 0)]
          (core/add-counter state :runner (refresh tt) :credit 1)
          (run-on state "HQ")
          (is (changed? [(:credit (get-runner)) 1]
                (card-ability state :runner tt 0))
              "Only got 1c for using Trickster Taka"))))
    (testing "when credits from a companion are used and then a companion is installed"
      (do-game
        (new-game {:runner {:hand ["Keiko" "Trickster Taka" "Mystic Maemi"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Keiko")
        (play-from-hand state :runner "Trickster Taka")
        (let [tt (get-resource state 0)]
          (core/add-counter state :runner (refresh tt) :credit 1)
          (take-credits state :runner)
          (take-credits state :corp)
          (run-on state "HQ")
          (is (changed? [(:credit (get-runner)) 2]
                (card-ability state :runner tt 0))
              "Got 1c from Keiko for using Trickster Taka")
          (is (changed? [(:credit (get-runner)) -1]
                (play-from-hand state :runner "Mystic Maemi"))
              "Did not get 1c back from installing Mystic Maemi")))))

(deftest knobkierie-functionality
    ;; functionality
    (do-game
      (new-game {:runner {:deck ["Knobkierie" "Hivemind" "Eater"]}})
      (core/gain state :runner :credit 20)
      (take-credits state :corp)
      (play-from-hand state :runner "Knobkierie")
      (play-from-hand state :runner "Eater")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt if not virus program installed")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Hivemind")
      (let [hv (find-card "Hivemind" (get-program state))]
        (is (= 1 (get-counters (refresh hv) :virus)) "Hivemind starts with 1 virus counters")
        (run-empty-server state "HQ")
        (click-prompt state :runner "Yes") ; gain virus counter
        (click-card state :runner (find-card "Hivemind" (get-program state)))
        (click-prompt state :runner "No action")
        (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind gains a counter on successful run")
        (run-empty-server state "HQ")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No prompt after first run")
        (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind doesn't gain a counter after first run"))))

(deftest knobkierie-interaction-with-cordyceps-issue-4781
    ;; Interaction with Cordyceps. Issue #4781
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma"]}
                 :runner {:hand ["Knobkierie" "Cordyceps"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "R&D")
      (play-from-hand state :corp "Enigma" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Knobkierie")
      (play-from-hand state :runner "Cordyceps")
      (run-empty-server state "HQ")
      (run-continue state :success)
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner))) "Runner has simult prompt")
      (click-prompt state :runner "Knobkierie")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Cordyceps")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Enigma")
      (click-card state :runner "Ice Wall")
      (is (= "Ice Wall" (:title (get-ice state :hq 0))) "Ice Wall is now protecting HQ")
      (is (= "Enigma" (:title (get-ice state :rd 0))) "Enigma is now protecting R&D")
      (is (no-prompt? state :runner) "No prompt if not virus program installed")))

(deftest lilypad
  (do-game
    (new-game {:runner {:hand ["LilyPAD" "Marjanah" "Refractor"]
                        :deck [(qty "Sure Gamble" 5)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "LilyPAD")
    (is (no-prompt? state :runner))
    (play-from-hand state :runner "Marjanah")
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-runner)))) "drew a card")
    (play-from-hand state :runner "Refractor")
    (is (no-prompt? state :runner) "No more prompt for draw")))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an icebreaker upon install
  (do-game
    (new-game {:runner {:deck [(qty "LLDS Processor" 2) "Inti" "Passport"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "LLDS Processor")
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "LLDS Processor")
    (play-from-hand state :runner "Passport")
    (let [inti (get-program state 0)
          pass (get-program state 1)]
      (is (= 2 (get-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (get-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :runner)
      (is (= 1 (get-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (get-strength (refresh pass))) "Strength reduced to default"))))

(deftest lockpick-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Lockpick" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Lockpick")
      (play-from-hand state :runner "Refractor")
      (let [lp (get-hardware state 0)
            refr (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner refr 1)
              (click-card state :runner lp))
            "Used 1 credit from Lockpick"))))

(deftest lucky-charm-requires-hq-run
  (do-game
    (new-game {:runner {:hand ["Lucky Charm"]}
               :corp {:hand ["Vanilla"]}})
    (play-from-hand state :corp "Vanilla" "R&D")
    (take-credits state :corp)
    (let [vanilla (get-ice state :rd 0)]
      (rez state :corp (refresh vanilla))
      (play-from-hand state :runner "Lucky Charm")
      (run-on state "R&D")
      (run-continue state)
      (card-subroutine state :corp (refresh vanilla) 0)
      (is (= nil (:run @state)) "no run")
      (is (no-prompt? state :runner) "no charm prompt")
      (run-empty-server state "HQ")
      (is (no-prompt? state :runner))
      (run-on state "R&D")
      (run-continue state :encounter-ice)
      (card-subroutine state :corp (refresh vanilla) 0)
      (is (:run @state) "Run not ended yet")
      (is (not (no-prompt? state :runner)) "Runner prompted to ETR"))))

(deftest lucky-charm-no-interference-with-runs-ending-successfully-or-by-jacking-out-and-batty-normal-etr-border-control-interaction
    ;; No interference with runs ending successfully or by jacking out, and Batty/normal ETR/Border Control interaction
    (do-game
     (new-game {:runner {:deck [(qty "Lucky Charm" 3)]}
                :corp {:deck ["Ice Wall" "Border Control" "Marcus Batty"]}})
     (play-from-hand state :corp "Border Control" "New remote")
     (play-from-hand state :corp "Ice Wall" "Server 1")
     (play-from-hand state :corp "Marcus Batty" "Server 1")
     (core/gain state :corp :credit 20)
     (take-credits state :corp)
     (core/gain state :runner :click 100)
     (play-from-hand state :runner "Lucky Charm")
     (run-empty-server state "HQ")
     (is (no-prompt? state :runner) "No prompt messing with runner when run naturally ends successfully")
     (doseq [serv ["Archives" "HQ" "R&D"]]
       (run-on state serv)
       (is (:run @state) "Run is ongoing")
       (run-jack-out state)
       (is (no-prompt? state :runner) (str "No prompt messing with runner when jacking out from run on " serv))
       (is (not (:run @state)) "Run is finished"))
     (doseq [serv ["Archives" "HQ"]]    ; R&D, server 1 has cards
       (run-on state serv)
       (is (:run @state) "Run is ongoing")
       (run-continue state)
       (is (not (:run @state)) "Run is ended")
       (is (no-prompt? state :runner) (str "No prompt messing with runner on a successful run on " serv)))
     (run-on state "Server 1")
     (run-continue-until state :success)
     (click-prompt state :runner "No action") ;access batty
     (is (not (:run @state)) "Run is ended")
     (is (no-prompt? state :runner) "No prompt messing with runner on a successful run on remote")
     (let [bc (get-ice state :remote1 0)
           iw (get-ice state :remote1 1)
           mb (get-content state :remote1 0)]
       (rez state :corp (refresh iw))
       (rez state :corp (refresh bc))
       (rez state :corp (refresh mb))
       ;; run into ice wall, have it ETR, do not use lucky charm
       (run-on state "Server 1")
       (run-continue state)
       (card-subroutine state :corp (refresh iw) 0)
       (is (:run @state) "Run not ended yet")
       (is (not (no-prompt? state :runner)) "Runner prompted to ETR")
       (click-prompt state :runner "Done")
       (is (not (:run @state)) "Run ended yet")
       (is (no-prompt? state :runner) "Prevent prompt gone")
       ;; run into border control, have its subroutine ETR, do use lucky charm
       (run-on state "Server 1")
       (run-continue state)
       (card-subroutine state :corp (refresh bc) 1)
       (is (:run @state) "Run not ended yet")
       (is (not (no-prompt? state :runner)) "Runner prompted to ETR")
       (card-ability state :runner (get-hardware state 0) 0)
       (click-prompt state :runner "Done")
       (is (= 1 (count (:rfg (get-runner)))) "Lucky Charm RFGed")
       (is (:run @state) "Run prevented from ending")
       (is (no-prompt? state :runner) "Prevent prompt gone")
       ;; trigger border control
       (play-from-hand state :runner "Lucky Charm")
       (card-ability state :corp (refresh bc) 0)
       (is (= 1 (count (:discard (get-corp)))) "Border Control trashed")
       (is (:run @state) "Run not ended yet")
       (is (not (no-prompt? state :runner)) "Runner prompted to ETR")
       (card-ability state :runner (get-hardware state 0) 0)
       (click-prompt state :runner "Done")
       (is (= 2 (count (:rfg (get-runner)))) "2nd Lucky Charm RFGed")
       (is (:run @state) "Run prevented from ending")
       ;; win batty psi game and fire ice wall sub
       (play-from-hand state :runner "Lucky Charm")
       (card-ability state :corp mb 0)
       (click-prompt state :corp "1 [Credits]")
       (click-prompt state :runner "0 [Credits]")
       (card-subroutine state :corp (refresh iw) 0)
       (is (:run @state) "Run not ended yet")
       (is (not (no-prompt? state :runner)) "Runner prompted to ETR")
       (card-ability state :runner (get-hardware state 0) 0)
       (click-prompt state :runner "Done")
       (is (:run @state) "Run prevented from ending"))))

(deftest mache
  ;; Mâché
  (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Cache"]
                          :hand ["Mâché"]
                          :credits 15}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Mâché")
      (let [mache (get-hardware state 0)
            counters (get-counters (refresh mache) :power)
            hand (-> (get-runner) :hand count)]
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (is (= (+ counters 4) (get-counters (refresh mache) :power)) "Mache should gain 4 counters for trashing a card with a trash cost of 4")
        (card-ability state :runner mache 0)
        (is (= (inc hand) (-> (get-runner) :hand count)) "Runner should draw one card for using Mache's ability")
        (is (= 1 (get-counters (refresh mache) :power)) "Mache ability should cost 3 counters"))))

(deftest mache-fizzles-if-a-card-with-no-trash-cost-is-trashed
    ;; Fizzles if a card with no trash cost is trashed
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Cache"]
                          :hand ["Imp" "Mâché"]
                          :creditw 15}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Imp")
      (play-from-hand state :runner "Mâché")
      (let [mache (get-hardware state 0)
            counters (get-counters (refresh mache) :power)]
        (run-empty-server state :hq)
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (is (= counters (get-counters (refresh mache) :power)) "Mache should gain no counters from trashing a card with no trash cost")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (is (= counters (get-counters (refresh mache) :power)) "Mache gains no counters as it's been used this turn already"))))
(deftest mache-with-political-operative
    ;; with Political Operative
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Mâché" "Political Operative" "Cache"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (starting-hand state :runner ["Mâché" "Political Operative"])
      (play-from-hand state :runner "Mâché")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Political Operative")
      (take-credits state :runner)
      (let [pad (get-content state :remote1 0)
            mache (get-hardware state 0)
            polop (get-resource state 0)]
        (card-ability state :runner polop 0)
        (click-card state :runner (refresh pad))
        (is (zero? (get-counters (refresh mache) :power)) "Mache should gain no counters from a trash outside of an access"))))

(deftest marrow
  (do-game
    (new-game {:runner {:hand [(qty "Sure Gamble" 2) "Marrow"]}
               :corp {:hand [(qty "Hedge Fund" 2) "Hostile Takeover"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Marrow")
    (is (= 1 (:brain-damage (get-runner))) "1 from marrow install")
    (is (= 7 (hand-size :runner)) "Max hand size is 7 (5 + 3 - 1)")
    (take-credits state :runner)
    (play-and-score state "Hostile Takeover")
    (is (last-log-contains? state "uses Marrow to sabotage 1") "Sabotage happened")))

(deftest masterwork-v37
  ;; Masterwork (v37)
  (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Masterwork (v37)" "Acacia" "Capstone"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Masterwork (v37)")
      (is (= 3 (count (:hand (get-runner)))) "Runner should draw a card for playing a hardware")
      (run-on state "HQ")
      (is (= :waiting (prompt-type :corp)) "Corp should be waiting on Runner")
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "Yes")
        (is (= credits (:credit (get-runner))) "Runner shouldn't spend any credits until hardware is actually installed")
        (click-card state :runner "Acacia")
        (is (= (- credits 2) (:credit (get-runner))) "Runner should spend 1 for Masterwork and 1 for Acacia"))
      (is (no-prompt? state :corp) "Corp shouldn't be waiting anymore")
      (is (no-prompt? state :runner))
      (run-continue state)
      (click-prompt state :runner "No action")
      (run-on state "HQ")
      (is (= :waiting (prompt-type :corp)) "Corp should be waiting on Runner")
      (is (not (no-prompt? state :runner)) "Runner should get a prompt every run")))

(deftest masterwork-v37-should-only-grant-bonus-on-the-first-hardware-install-issue-4097
    ;; Should only grant bonus on the first Hardware install. Issue #4097
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Masterwork (v37)" "Acacia" (qty "Daily Casts" 5)]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Masterwork (v37)")
      (is (= 7 (count (:hand (get-runner)))) "Runner should draw a card for playing a hardware")
      (play-from-hand state :runner "Daily Casts")
      (is (= 6 (count (:hand (get-runner)))) "Runner shouldn't draw a card from installing a non-hardware after installing a single hardware")
      (play-from-hand state :runner "Acacia")
      (play-from-hand state :runner "Daily Casts")
      (is (= 4 (count (:hand (get-runner)))) "Runner shouldn't draw any more cards from installing anything")))

(deftest masterwork-v37-interactions-with-multiple-triggers-issue-4256
    ;; Interactions with multiple triggers. Issue #4256
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]}
                 :runner {:id "Armand \"Geist\" Walker: Tech Lord"
                          :deck [(qty "Clone Chip" 10)]
                          :hand ["Masterwork (v37)"
                                 "DJ Fenris"
                                 "The Class Act"
                                 "Street Peddler"]
                          :credits 100}})
      (take-credits state :corp)
      (play-from-hand state :runner "Masterwork (v37)")
      (play-from-hand state :runner "Street Peddler")
      (play-from-hand state :runner "DJ Fenris")
      (click-prompt state :runner "Hayley Kaplan: Universal Scholar")
      (play-from-hand state :runner "The Class Act")
      (take-credits state :runner)
      (is (= 5 (count (:hand (get-runner)))) "Starts with 5 cards in hand")
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 5 (count (:set-aside (get-runner)))) "Geist's draw triggers The Class Act, and street peddler has three set-aside cards")
      (is (= "The Class Act" (:title (:card (prompt-map :runner)))))
      ;; click the drawn card, which is last in the set-aside zone
      (click-card state :runner (last (:set-aside (get-runner))))
      (is (= "Street Peddler" (:title (:card (prompt-map :runner)))))
      (click-prompt state :runner "Clone Chip")
      (is (= 6 (count (:hand (get-runner)))) "Geist draw finishes")
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Masterwork (v37)")
      (is (= 7 (count (:hand (get-runner)))) "Masterwork draw")
      (is (= "Hayley Kaplan: Universal Scholar" (:title (:card (prompt-map :runner)))))
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Clone Chip" (:hand (get-runner))))
      (is (= 6 (count (:hand (get-runner)))) "Hayley install")
      (is (no-prompt? state :runner))))

(deftest masterwork-v37-interaction-with-paladin-poemu
  ;; Masterwork (v37) should be able to spend credits hosted on Paladin Poemu
  (do-game
    (new-game {:runner {:hand ["Masterwork (v37)" "Acacia" "Paladin Poemu"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Masterwork (v37)")
    (play-from-hand state :runner "Paladin Poemu")
    (let [pp (get-resource state 0)]
      (core/add-counter state :runner (refresh pp) :credit 2)
      (run-on state "HQ")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Acacia")
      (is (changed? [(get-counters (refresh pp) :credit) -1]
            (click-card state :runner pp))
          "Spent 1 credit from Paladin Poemu"))))

(deftest maui-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:deck ["Māui" "Inti"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Māui")
      (play-from-hand state :runner "Inti")
      (let [maui (get-hardware state 0)
            inti (get-program state 0)]
        (card-ability state :runner inti 1)
        (is (no-prompt? state :runner) "Not enough money to pay for Inti pump")
        (run-on state "HQ")
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner inti 1)
              (click-card state :runner maui)
              (click-card state :runner maui))
            "Used 2 credits from Maui"))))

(deftest maw
  ;; Maw - Once per turn, first time runner declines to steal or trash, trash a HQ card at random
  (do-game
      (new-game {:corp {:deck [(qty "BOOM!" 5)]}
                 :runner {:deck ["Maw"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "No HQ card in discard before Maw installed")
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "HQ card not trashed by Maw as first decline already happened")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "2nd HQ card on same turn not trashed by Maw")))

(deftest maw-check-trashed-card-is-trashed-face-up-if-it-s-the-card-that-is-accessed-issue-2695
    ;; Also checks Maw auto-trashes on Operation with no trash cost
    ;; Check trashed card is trashed face-up if it's the card that is accessed, issue #2695
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck ["Maw"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (is (zero? (count (:discard (get-corp)))) "HQ card not trashed by Maw yet")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw now")
      (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen since it was accessed")))

(deftest maw-with-hiro-in-hand-hiro-not-moved-to-runner-scored-area-on-trash-decline-2638
    ;; with Hiro in hand - Hiro not moved to runner scored area on trash decline. #2638
    (do-game
      (new-game {:corp {:deck ["Chairman Hiro"]}
                 :runner {:deck ["Maw"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "Hiro not scored")
      (is (= 1 (count (:discard (get-corp)))) "Hiro trashed by Maw")))

(deftest maw-maw-shouldn-t-trigger-on-stolen-agenda-3433
    ;; Maw shouldn't trigger on stolen agenda. #3433
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"
                               (qty "Ice Wall" 5)]}
                 :runner {:deck ["Maw"]}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :remote1)
      (click-prompt state :runner "Steal")
      (is (zero? (count (:discard (get-corp)))) "No HQ card in discard as agenda was stolen")))

(deftest maw-maw-shouldn-t-trigger-when-accessing-a-card-in-archives-3388
    ;; Maw shouldn't trigger when accessing a card in archives. #3388
    (do-game
      (new-game {:corp {:deck ["Rashida Jaheem" "Cyberdex Virus Suite" (qty "Ice Wall" 4)]}
                 :runner {:id "Alice Merchant: Clan Agitator"
                          :deck ["Maw" "Imp"]}})
      (core/move state :corp (find-card "Rashida Jaheem" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Cyberdex Virus Suite")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Imp")
      (run-empty-server state :archives)
      (click-card state :corp (find-card "Ice Wall" (:hand (get-corp)))) ;; Alice's ability
      (click-prompt state :runner "Cyberdex Virus Suite")
      (click-prompt state :corp "Yes")
      (run-empty-server state :rd)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 3 (count (:discard (get-corp)))) "Ice Wall, CVS, and Rashida")
      (is (no-prompt? state :runner) "No more prompts for runner")))

(deftest maw-maw-should-trigger-when-declining-to-steal-3388
    ;; Maw should trigger when declining to steal. #3388
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 4)]
                        :hand [(qty "Obokata Protocol" 2) "Ice Wall"]
                        :discard ["Ice Wall"]}
                 :runner {:id "Alice Merchant: Clan Agitator"
                          :deck ["Maw" "Archives Interface"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Archives Interface")
      (run-empty-server state :archives)
      (click-card state :corp (find-card "Obokata Protocol" (:hand (get-corp))))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner (find-card "Ice Wall" (:discard (get-corp))))
      (click-prompt state :runner "No action")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "Ice Wall and Obokata")))

(deftest maw-gang-sign-interaction-5021
    ;; Gang Sign interaction. #5021
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 5)]
                        :hand ["Hostile Takeover" (qty "Hedge Fund" 2)]}
                 :runner {:hand ["Maw" "Gang Sign"]
                          :credits 10}})
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Gang Sign")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "Maw forces a discard")
      (take-credits state :runner)
      (play-and-score state "Hostile Takeover")
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "Maw forces another discard")))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (do-game
      (new-game {:corp {:hand [(qty "Hedge Fund" 2) (qty "Snare!" 2)
                               "Hostile Takeover" "Scorched Earth"]}
                 :runner {:hand ["Maya" (qty "Sure Gamble" 3)]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (run-empty-server state "R&D")
      (is (accessing state "Hostile Takeover"))
      (click-prompt state :runner "Steal")
      (is (= "Move Hostile Takeover to the bottom of R&D?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (no-prompt? state :runner) "No more prompts for runner")
      (is (not (:run @state)) "Run is ended")
      (is (= "Hostile Takeover" (:title (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
      (take-credits state :runner)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck {:front true})
      (take-credits state :corp)
      (run-empty-server state "R&D")
      (click-prompt state :corp "Yes")
      (is (zero? (count (:hand (get-runner)))) "Runner took Snare! net damage")
      (is (accessing state "Snare!"))
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (click-prompt state :runner "Yes")
      (is (no-prompt? state :runner) "No more prompts for runner")
      (is (not (:run @state)) "Run is ended")
      (is (= "Snare!" (:title (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")))

(deftest maya-does-not-interrupt-multi-access
    ;; Does not interrupt multi-access
    (do-game
      (new-game {:corp {:hand [(qty "Hedge Fund" 2) "Scorched Earth" "Snare!"
                               "Beanstalk Royalties" "IPO"]}
                 :runner {:hand ["Maya" (qty "Sure Gamble" 3) "R&D Interface"]}})
      (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Maya")
      (play-from-hand state :runner "R&D Interface")
      (run-empty-server state :rd)
      (is (accessing state "Scorched Earth") "Accessing the top card of R&D")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= "Scorched Earth" (:title (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
      (is (not (no-prompt? state :runner)) "Runner has next access prompt")))

(deftest maya-triggers-only-on-rd-accesses
    ;; Maya does not trigger on accesses out of R&D
    (do-game
      (new-game {:corp {:deck ["Anansi" "Brainstorm" "Chiyashi"]}
                 :runner {:hand ["Maya" "Equivocation"]}})
      (core/move state :corp (find-card "Anansi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Anansi"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      ;; R&D is now from top to bottom: A B C
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (play-from-hand state :runner "Maya")
      (run-empty-server state :rd)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No")
      (play-from-hand state :runner "Equivocation")
      (run-empty-server state :rd)
      (click-prompt state :runner "Yes") ; Equivocation prompt
      (click-prompt state :runner "Yes") ; force the draw
      (is (find-card "Anansi" (:hand (get-corp))) "Anansi added to HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No") ; Maya prompt
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No more prompts for runner")
      (is (not (:run @state)) "Run is ended")))

(deftest mind-s-eye-interaction-with-rdi-aeneas
    ;; Interaction with RDI + Aeneas
    (do-game
      (new-game {:corp {:deck [(qty "Jackson Howard" 2)]}
                 :runner {:deck ["Mind's Eye" "R&D Interface" "Aeneas Informant"]}})
      (dotimes [_ 2]
        (core/move state :corp (find-card "Jackson Howard" (:hand (get-corp))) :deck))
      (take-credits state :corp)
      (core/gain state :runner :credit 10 :click 20)
      (play-from-hand state :runner "Mind's Eye")
      (let [eye (get-hardware state 0)]
        (is (zero? (get-counters (refresh eye) :power)) "0 counters on install")
        (dotimes [_ 3]
          (run-empty-server state :rd)
          (click-prompt state :runner "No action"))
        (is (= 3 (get-counters (refresh eye) :power)) "3 counters after 3 runs")
        (play-from-hand state :runner "R&D Interface")
        (play-from-hand state :runner "Aeneas Informant")
        (card-ability state :runner (refresh eye) 0)
        (let [num-creds (:credit (get-runner))]
          (dotimes [_ 2]
            (click-prompt state :runner "No action")
            (click-prompt state :runner "Yes")) ; Aeneas
          (is (= (+ num-creds 2) (:credit (get-runner))) "Runner has gained 2 from Aeneas")))))

(deftest mu-safecracker-no-available-stealth-credits
    ;; No available stealth credits
    (testing "Breach HQ"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand [(qty "Hedge Fund" 2)]}
                   :runner {:hand ["Mu Safecracker"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Mu Safecracker")
        (run-empty-server state "HQ")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run has ended with no prompt")))
    (testing "Breach R&D"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand [(qty "Hedge Fund" 2)]}
                   :runner {:hand ["Mu Safecracker"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Mu Safecracker")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run has ended with no prompt"))))

(deftest mu-safecracker-available-stealth-credits
    ;; Available stealth credits
    (testing "Breach HQ"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand [(qty "Hedge Fund" 2)]}
                   :runner {:hand ["Mu Safecracker" "Ghost Runner"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Ghost Runner")
        (play-from-hand state :runner "Mu Safecracker")
        (run-empty-server state "HQ")
        (is (= "Pay 1 [Credits] to access 1 additional card?"
               (:msg (prompt-map :runner)))
            "Runner has the Mu Safecracker prompt")
        (click-prompt state :runner "Yes")
        (click-card state :runner "Ghost Runner")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run has ended")))
    (testing "Breach R&D"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand [(qty "Hedge Fund" 2)]}
                   :runner {:hand ["Mu Safecracker" "Ghost Runner"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Ghost Runner")
        (play-from-hand state :runner "Mu Safecracker")
        (run-empty-server state "R&D")
        (is (= "Pay 2 [Credits] to access 1 additional card?"
               (:msg (prompt-map :runner)))
            "Runner has the Mu Safecracker prompt")
        (click-prompt state :runner "Yes")
        (click-card state :runner "Ghost Runner")
        (click-card state :runner "Ghost Runner")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (not (:run @state)) "Run has ended")))
    (testing "Issue #5083: Mu Safecracker doesn't work with Cold Read"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand [(qty "Hedge Fund" 2)]}
                   :runner {:hand ["Mu Safecracker" "Cold Read"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Mu Safecracker")
        (play-from-hand state :runner "Cold Read")
        (click-prompt state :runner "R&D")
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-card state :runner "Cold Read")
        (click-card state :runner "Cold Read")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Done")
        (is (not (:run @state)) "Run has ended"))))

(deftest net-ready-eyes
  ;; Net-Ready Eyes
  (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 3) "Net-Ready Eyes" "Peacock"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Peacock")
      (play-from-hand state :runner "Net-Ready Eyes")
      (is (= 3 (count (:discard (get-runner)))) "Took 2 damage on NRE install")
      (run-on state "HQ")
      (let [pea (get-program state 0)]
        (click-card state :runner pea)
        (is (= 3 (get-strength (refresh pea))) "Peacock strength boosted")
        (run-continue state)
        (click-prompt state :runner "No action")
        (is (= 2 (get-strength (refresh pea))) "Peacock strength back to default"))))

(deftest net-ready-eyes-do-not-display-prompt-without-an-installed-icebreaker
    ;; Do not display prompt without an installed icebreaker
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 3) (qty "Net-Ready Eyes" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Net-Ready Eyes")
      (is (= 3 (count (:discard (get-runner)))) "Took 2 damage on NRE install")
      (run-on state "HQ")
      (is (no-prompt? state :runner) "No NRE prompt")))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 3) (qty "Cache" 3)]
                          :hand ["Obelus" "Nerve Agent"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10 :click 3)
      (play-from-hand state :runner "Nerve Agent")
      (let [nerve (get-program state 0)]
        (run-empty-server state :hq)
        (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
        (click-prompt state :runner "No action")
        (play-from-hand state :runner "Obelus")
        (gain-tags state :runner 1)
        (core/fake-checkpoint state)
        (is (= 6 (hand-size :runner)) "Max hand size is 6")
        (core/lose-tags state :runner (core/make-eid state) 1)
        (core/fake-checkpoint state)
        (is (= 5 (hand-size :runner)) "Max hand size is 5")
        (run-empty-server state :hq)
        (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
        (click-prompt state :runner "1")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (empty? (:hand (get-runner))) "No cards drawn by Obelus, already had successful HQ run")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
        (click-prompt state :runner "2")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards"))))

(deftest obelus-running-and-trashing-crisium-grid-makes-run-neither-successful-unsuccessful
    ;; running and trashing Crisium Grid makes run neither successful/unsuccessful
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]
                        :hand ["Crisium Grid"]}
                 :runner {:hand ["Obelus"]
                          :deck [(qty "Sure Gamble" 3)]}})
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Crisium Grid")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (click-prompt state :runner "No action")
      (is (empty? (:hand (get-runner))) "Crisium Grid blocked successful run")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run")))

(deftest obelus-using-hades-shard-during-run-to-increase-draw
    ;; using Hades Shard during run to increase draw
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3)]}
                 :runner {:hand ["Obelus" "Hades Shard"]
                          :deck [(qty "Sure Gamble" 3) (qty "Cache" 3)]}})
      (starting-hand state :corp ["Hedge Fund" "Hedge Fund"])
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Obelus")
      (play-from-hand state :runner "Hades Shard")
      (run-empty-server state "R&D")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "No action")
      (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards")))

(deftest obelus-running-a-remote-server-first-doesn-t-block-card-draw
    ;; running a remote server first doesn't block card draw
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]
                        :hand ["Urban Renewal"]}
                 :runner {:deck [(qty "Sure Gamble" 3)]
                          :hand ["Obelus"]}})
      (play-from-hand state :corp "Urban Renewal" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run")))

(deftest obelus-works-with-paper-tripping
    ;; works with Paper Tripping
    (do-game
      (new-game {:runner {:deck ["Obelus" "Paper Tripping"]
                          :tags 3}})
      (take-credits state :corp)
      (is (= 3 (count-tags state)) "Runner starts with 3 tags")
      (play-from-hand state :runner "Obelus")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Paper Tripping")
      (is (zero? (count-tags state)) "Runner loses all tags")))

(deftest omni-drive-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:hand ["Omni-drive" "Inti"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Omni-drive")
      (let [omni (get-hardware state 0)]
          (card-ability state :runner omni 0)
          (click-card state :runner (find-card "Inti" (:hand (get-runner))))
          (let [inti (first (:hosted (refresh omni)))]
            (is (changed? [(:credit (get-runner)) -1]
                  (card-ability state :runner inti 1)
                  (click-card state :runner omni))
                "Used 1 credit from Omni-drive")))))

(deftest pan-weave-happy
  ;; PAN-Weave - 1 meat on install, once/turn siphon 1 credit on hq run
  (do-game
    (new-game {:runner {:hand [(qty "Sure Gamble" 2) "PAN-Weave"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "PAN-Weave")
    (is (= 1 (count (:discard (get-runner)))) "1 damage done")
    (is (changed? [(:credit (get-runner)) 1
           (:credit (get-corp)) -1]
          (run-on state :hq)
          (run-continue state)
          (click-prompt state :runner "No action")
          (is (no-prompt? state :runner)))
        "gained 1c from pan-weave")))

(deftest pan-weave-no-credits
  ;; PAN-Weave - 1 meat on install, once/turn siphon 1 credit on hq run
  (do-game
    (new-game {:runner {:hand [(qty "Sure Gamble" 2) "PAN-Weave"]}})
    (take-credits state :corp)
    (core/lose state :corp :credit 8)
    (is (= 0 (:credit (get-corp))))
    (play-from-hand state :runner "PAN-Weave")
    (is (= 1 (count (:discard (get-runner)))) "1 damage done")
    (is (changed? [(:credit (get-runner)) 0
           (:credit (get-corp)) 0]
          (run-on state :hq)
          (run-continue state)
          (click-prompt state :runner "No action")
          (is (no-prompt? state :runner)))
        "gained 0c from pan-weave")))

(deftest pantograph-trigger-on-steal
  ;; Pantograph - Gain 1 credit and may install a card on steal
  (do-game
   (new-game {:corp {:hand [(qty "House of Knives" 3)]}
              :runner {:hand ["Pantograph" "Bankroll"]}})
   (take-credits state :corp)
   (play-from-hand state :runner "Pantograph")
   (is (= 5 (core/available-mu state)) "Gain 1 memory")
   (run-empty-server state :hq)
   (is (changed? [(:credit (get-runner)) 1]
         (click-prompt state :runner "Steal"))
       "Gain 1 credit from Pantograph")
   (click-prompt state :runner "Done")
   (run-empty-server state :hq)
   (click-prompt state :runner "Steal")
   (is (changed? [(:credit (get-runner)) -1]
         (click-card state :runner (find-card "Bankroll" (:hand (get-runner)))))
       "Pay cost of Bankroll to install from Pantograph")
   (is (get-program state 0) "Bankroll is installed")
   (run-empty-server state :hq)
   (click-prompt state :runner "Steal")
   (is (no-prompt? state :runner) "No install prompt since no cards in hand")))

(deftest pantograph-trigger-on-score
  ;; Pantograph - Gain 1 credit and may install a card on score
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                     :hand ["House of Knives"]}
              :runner {:hand ["Pantograph" "Bankroll"]}})
   (play-from-hand state :corp "House of Knives" "New remote")
   (take-credits state :corp)
   (play-from-hand state :runner "Pantograph")
   (take-credits state :runner)
   (is (changed? [(:credit (get-runner)) 1]
         (score-agenda state :corp (get-content state :remote1 0)))
       "Gain 1 credit from Pantograph")
   (is (changed? [(:credit (get-runner)) -1]
         (click-card state :runner (find-card "Bankroll" (:hand (get-runner)))))
       "Pay cost of Bankroll to install from Pantograph")
   (is (get-program state 0) "Bankroll is installed")))

(deftest paragon-vanilla-test
    ;; Vanilla test
    (do-game
     (new-game {:runner {:deck ["Paragon" "Easy Mark" "Sure Gamble"]}})
     (starting-hand state :runner ["Paragon"])
     (take-credits state :corp)
     (play-from-hand state :runner "Paragon")
     (run-empty-server state "HQ")
     (is (prompt-is-card? state :runner (get-hardware state 0)) "Prompt from Paragon")
     (click-prompt state :runner "Yes")
     (is (= (+ 5 -3 1) (:credit (get-runner))) "Gained 1 credit from Paragon")
     (is (prompt-is-card? state :runner (get-hardware state 0)) "Prompt from Paragon")
     (let [top-card (:title (first (:deck (get-runner))))]
       (click-prompt state :runner "Yes")
       (is (= top-card (:title (last (:deck (get-runner))))) "Moved top card to bottom"))
     (click-prompt state :runner "No action")
     (run-empty-server state "HQ")
     (is (not (prompt-is-card? state :runner (get-hardware state 0))) "No prompt from Paragon")))

(deftest paragon-autoresolve
    ;; Autoresolve
    (do-game
     (new-game {:runner {:deck ["Paragon" (qty "Easy Mark" 3)]}})
     (starting-hand state :runner ["Paragon"])
     (take-credits state :corp)
     (play-from-hand state :runner "Paragon")
     (letfn [(toggle-paragon [setting]
               (card-ability state :runner (get-hardware state 0) 0)
               (click-prompt state :runner setting))]
       (doseq [set-to ["Never" "Ask" "Always"]]
         (is (changed? [(:credit (get-runner)) 0]
               (toggle-paragon set-to)
               (run-empty-server state "Archives")
               ; on first loop this actually triggers paragon, but we say 'no'
               (is (no-prompt? state :runner) "No Paragon prompt"))
             "Paragon does not fire"))
       (take-credits state :runner)
       (take-credits state :corp)
       ;; paragon is now set to 'Always'
       (is (changed? [(:credit (get-runner)) 1]
             (run-empty-server state "Archives")
             ; on first loop this actually triggers paragon, but we say 'no'
             (click-prompt state :runner "Yes")
             ; prompt to add a card to bottom
             (is (no-prompt? state :runner) "No Paragon prompt"))
           "Paragon fires automatically"))))

(deftest patchwork-play-an-event
    ;; Play an event
    (do-game
      (new-game {:runner {:deck ["Patchwork" (qty "Sure Gamble" 2) "Easy Mark"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
      (play-from-hand state :runner "Sure Gamble")
      (click-card state :runner (get-hardware state 0))
      (is (empty? (:discard (get-runner))) "Easy Mark is not in heap yet")
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (not-empty (:discard (get-runner))) "Easy Mark is in heap")
      (is (= 11 (:credit (get-runner))) "Runner has only paid 3 for Sure Gamble")))

(deftest patchwork-install-a-card
    ;; Install a card
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Easy Mark" "Cyberfeeder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (play-from-hand state :runner "Cyberfeeder")
      (is (= 5 (:credit (get-runner))) "Runner has not been charged credits yet")
      (is (empty? (:discard (get-runner))) "Easy Mark is not in heap yet")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 5 (:credit (get-runner))) "Runner was charged 0 credits to play Cyberfeeder")))

(deftest patchwork-issue-4322-trashing-same-card-that-is-being-installed
    ;; Issue #4322: Trashing same card that is being installed
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Easy Mark" "Cyberfeeder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (play-from-hand state :runner "Cyberfeeder")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner "Cyberfeeder")
      (is (find-card "Cyberfeeder" (:play-area (get-runner))) "Cyberfeeder is on the table")
      (is (not (no-prompt? state :runner)) "Prompt still open")))

(deftest patchwork-used-when-runner-credit-pool-is-under-printed-cost-issue-4563
    ;; Used when runner credit pool is under printed cost. Issue #4563
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Sure Gamble" "Easy Mark"]
                          :credits 7}})
      (take-credits state :corp)
      (play-from-hand state :runner "Patchwork")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (play-from-hand state :runner "Sure Gamble")
      (click-card state :runner (get-hardware state 0))
      (is (empty? (:discard (get-runner))) "Easy Mark is not in heap yet")
      (click-card state :runner "Easy Mark")
      (is (not-empty (:discard (get-runner))) "Easy Mark is in heap")
      (is (= 9 (:credit (get-runner))) "Runner has only paid 3 for Sure Gamble")))

(deftest patchwork-career-fair-patchwork-don-t-properly-allow-playing-of-cards-only-playable-with-both-discounts-issue-4617
    ;; Career Fair + Patchwork don't properly allow playing of cards only playable with both discounts. Issue #4617
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Sure Gamble" "Career Fair" "Liberated Account"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Patchwork")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (play-from-hand state :runner "Career Fair")
      (click-card state :runner (find-card "Liberated Account" (:hand (get-runner))))
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 0 (:credit (get-runner))) "Paid 1c for Liberated Account")
      (is (get-resource state 0) "Installed Liberated Account")))

(deftest patchwork-patchwork-shouldn-t-give-credit-back-when-playing-1c-card-issue-4960
    ;; Patchwork shouldn't give credit back when playing 1c card. Issue #4960
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Sure Gamble" "Film Critic"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Patchwork")
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (play-from-hand state :runner "Film Critic")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      (is (= 1 (:credit (get-runner))) "Runner should still have 1c")
      (is (get-resource state 0) "Installed Film Critic")))

(deftest patchwork-shouldn-t-trigger-credits-spent-from-cards-effects-issue-6462
  ;; Patchwork's ability shouldn't count for "spend credits from an installed card" type effects
  (do-game
    (new-game {:runner {:deck ["Patchwork" "Paperclip" "Sure Gamble" "The Twinning"] :credits 10 }})
    (take-credits state :corp)
    (play-from-hand state :runner "The Twinning")
    (play-from-hand state :runner "Patchwork")
    (let [patchwork (get-hardware state 0)
          twinning (get-resource state 0)
          paperclip (find-card "Paperclip" (:hand (get-runner)))]
      (play-from-hand state :runner "Sure Gamble")
      (click-card state :runner patchwork)
      (click-card state :runner paperclip)
      (is (= 0 (get-counters (refresh twinning) :power)) "Twinning should not have gained a counter"))))

(deftest pennyshaver
  ;; Pennyshaver - Place credits on successful run and take credits from Pennyshaver
  (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck ["Pennyshaver"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Pennyshaver")
      (is (= 5 (core/available-mu state)) "Gain 1 memory")
      (let [pennyshaver (get-hardware state 0)]
        (is (= 0 (get-counters (refresh pennyshaver) :credit)) "0 credits on install")
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (is (= 1 (get-counters (refresh pennyshaver) :credit)) "1 credits after one run")
        (run-empty-server state :hq)
        (click-prompt state :runner "No action")
        (is (= 2 (get-counters (refresh pennyshaver) :credit)) "2 credits after second run")
        (is (changed? [(:credit (get-runner)) 3]
              (card-ability state :runner pennyshaver 0))
            "Gain 1 + 2 credit from Pennyshaver")
        (is (= 0 (get-counters (refresh pennyshaver) :credit)) "0 credits after ability trigger"))))

(deftest plascrete-carapace
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game {:corp {:deck ["Scorched Earth"]}
               :runner {:deck ["Plascrete Carapace" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Plascrete Carapace")
    (let [plas (get-hardware state 0)]
      (is (= 4 (get-counters (refresh plas) :power)) "4 counters on install")
      (take-credits state :runner)
      (gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (is (= 1 (count (:hand (get-runner)))) "All meat damage prevented")
      (is (empty? (get-hardware state)) "Plascrete depleted and trashed"))))

(deftest poison-vial
  ;; Poison Vial
  (do-game
    (new-game {:corp {:deck ["Spiderweb"]}
               :runner {:id "Quetzal: Free Spirit"
                        :deck ["Poison Vial"]}})
    (play-from-hand state :corp "Spiderweb" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Poison Vial")
    (let [pvial (get-hardware state 0)
          spiderweb (get-ice state :hq 0)
          q (get-in @state [:runner :identity])]
      (is (= 3 (get-counters pvial :power)) "3 counters on install")
      (run-on state :hq)
      (rez state :corp spiderweb)
      (run-continue state)
      (card-ability state :runner pvial 0)
      (is (no-prompt? state :runner) "Can use ability only after breaking at least 1 sub")
      (card-ability state :runner q 0)
      (click-prompt state :runner "End the run")
      (is (changed? [(get-counters (refresh pvial) :power) -1]
            (card-ability state :runner pvial 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "End the run"))
          "Spent 1 counter")
      (is (= 3 (count (filter :broken (:subroutines (refresh spiderweb))))) "Spiderweb has all of its subroutines broken")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :hq)
      (run-continue state)
      (card-ability state :runner q 0)
      (click-prompt state :runner "End the run")
      (is (changed? [(get-counters (refresh pvial) :power) -1]
            (card-ability state :runner pvial 0)
            (click-prompt state :runner "End the run")
            (click-prompt state :runner "Done"))
          "Spent 1 counter")
      (is (= 2 (count (filter :broken (:subroutines (refresh spiderweb))))) "Spiderweb has 2 out of 3 subroutines broken"))))

(deftest prepaid-voicepad-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Prepaid VoicePAD" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Prepaid VoicePAD")
      (let [ppvp (get-hardware state 0)]
        (is (changed? [(:credit (get-runner)) -1]
              (play-from-hand state :runner "Dirty Laundry")
              (click-card state :runner ppvp))
            "Used 1 credit from "))))

(deftest prognostic-q-loop
  ;; Prognostic Q-Loop
  (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Clone Chip" "Dirty Laundry" "Equivocation" "Prognostic Q-Loop"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Dirty Laundry" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Equivocation" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B C D E
      (play-from-hand state :runner "Prognostic Q-Loop")
      (run-on state :hq)
      (click-prompt state :runner "Yes")
      (is (= "The top 2 cards of the stack are Au Revoir and Bankroll" (:msg (prompt-map :runner))))
      (click-prompt state :runner "OK")
      (card-ability state :runner (get-hardware state 0) 1)
      (click-prompt state :runner "Yes")
      (is (= "Au Revoir" (:title (get-program state 0))) "Installed Au Revoir")
      (card-ability state :runner (get-hardware state 0) 1)
      (is (no-prompt? state :runner) "Can use ability only once per turn")
      (run-jack-out state)
      (run-on state :hq)
      (is (no-prompt? state :runner) "Only trigger on the first run of the turn")))

(deftest prognostic-q-loop-does-not-reveal-if-first-run-has-been-before-install
    ;; Does not reveal if first run has been before install
    (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Prognostic Q-Loop"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B
      (run-empty-server state :archives)
      (play-from-hand state :runner "Prognostic Q-Loop")
      (run-on state :hq)
      (is (no-prompt? state :runner) "Does not trigger on second run even if installed later")))

(deftest prognostic-q-loop-auto-resolve
    ;; Auto-resolve
    (do-game
      (new-game {:runner {:hand ["Au Revoir" "Bankroll" "Prognostic Q-Loop"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom: A B
      (play-from-hand state :runner "Prognostic Q-Loop")
      (letfn [(toggle-q-loop [setting]
                (card-ability state :runner (get-hardware state 0) 0)
                (click-prompt state :runner setting)
                (is (no-prompt? state :runner) "Prompt closed"))]
        (doseq [set-to ["Never" "Ask" "Always"]]
          (toggle-q-loop set-to)
          (run-on state "Archives")
          (case set-to
            "Never"
            (is (no-prompt? state :runner) "Does not show prompt")
            "Always"
            (do (last-log-contains? state "Runner uses Prognostic Q-Loop to look at the top 2 cards of the stack.")
                (click-prompt state :runner "OK")
                (is (no-prompt? state :runner) "Look prompt closed"))
            "Ask"
            (do (is (not (no-prompt? state :runner)) "Does show trigger prompt")
                (click-prompt state :runner "Yes")
                (last-log-contains? state "Runner uses Prognostic Q-Loop to look at the top 2 cards of the stack.")
                (is (not (no-prompt? state :runner)) "Does show look prompt")
                (click-prompt state :runner "OK")
                (is (no-prompt? state :runner) "Look prompt closed")))
          (run-jack-out state)
          (take-credits state :runner)
          (take-credits state :corp)))))

(deftest prognostic-q-loop-printing-the-revealed-card
    ;; Printing the revealed card
    (testing "when the revealed card is a hardware"
      (do-game
        (new-game {:runner {:deck ["Au Revoir"]
                            :hand ["Prognostic Q-Loop"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Prognostic Q-Loop")
        (card-ability state :runner (get-hardware state 0) 1)
        (is (last-log-contains? state "reveal Au Revoir from the top of the stack") "Correctly prints the revealed card")))
    (testing "when the revealed card is not a hardware"
      (do-game
        (new-game {:runner {:deck ["Sure Gamble"]
                            :hand ["Prognostic Q-Loop"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Prognostic Q-Loop")
        (card-ability state :runner (get-hardware state 0) 1)
        (is (last-log-contains? state "reveal Sure Gamble from the top of the stack") "Correctly prints the revealed card"))))

(deftest prognostic-q-loop-doesn-t-fire-with-an-empty-deck
    ;; Doesn't fire with an empty deck
    (do-game
      (new-game {:runner {:hand ["Prognostic Q-Loop"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Prognostic Q-Loop")
      (card-ability state :runner (get-hardware state 0) 1)
      (is (last-log-contains? state "Runner spends [Click] and pays 1 [Credits] to install Prognostic Q-Loop.")
          "Shouldn't print anything to log as the stack is empty")))

(deftest prognostic-q-loop-orders-correctly-with-other-on-run-triggers-when-firing-first-issue-4973
    ;; Orders correctly with other on run triggers when firing first. Issue #4973
    (do-game
      (new-game {:runner {:hand ["Prognostic Q-Loop", "Masterwork (v37)", "Spy Camera", "Au Revoir", "Bankroll"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom A B
      (play-from-hand state :runner "Prognostic Q-Loop")
      (play-from-hand state :runner "Masterwork (v37)")
      (run-on state :hq)
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Prognostic Q-Loop")
      (click-prompt state :runner "Yes")
      (is (= "The top 2 cards of the stack are Au Revoir and Bankroll" (:msg (prompt-map :runner))))))

(deftest prognostic-q-loop-are-the-correct-cards-shown-if-another-start-of-run-trigger-draws-a-card-issue-4973
    ;; Are the correct cards shown if another start of run trigger draws a card. Issue #4973
    (do-game
      (new-game {:runner {:hand ["Prognostic Q-Loop", "Masterwork (v37)", "Spy Camera", "Au Revoir", "Bankroll", "Clone Chip"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      ; Deck is now top to bottom A B C
      (play-from-hand state :runner "Prognostic Q-Loop")
      (play-from-hand state :runner "Masterwork (v37)")
      (take-credits state :runner)
      (take-credits state :corp)
      ; Reset runner first-hardware-install
      (run-on state :hq)
      (is (= "Choose a trigger to resolve" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Masterwork (v37)")
      (click-prompt state :runner "Yes")
      (click-card state :runner "Spy Camera")
      (is (= 1 (count (:hand (get-runner)))) "Runner should draw a card for playing a hardware")
      (is (= "Look at top 2 cards of the stack?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      ; Au Revoir drawn by Masterwork off it's own install, Q Loop prompt shows accurate info
      (is (= "The top 2 cards of the stack are Bankroll and Clone Chip" (:msg (prompt-map :runner))))))

(deftest prognostic-q-loop-works-with-paladin-poemu-5304
    ;; Works with Paladin Poemu #5304
    (do-game
      (new-game {:runner {:hand ["Prognostic Q-Loop" "Paladin Poemu" "Spy Camera" "Au Revoir" "Bankroll" "Clone Chip"]
                          :credits 10}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Au Revoir" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Clone Chip" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "Prognostic Q-Loop")
      (play-from-hand state :runner "Paladin Poemu")
      (take-credits state :runner)
      (take-credits state :corp)
      (card-ability state :runner (get-hardware state 0) 1)
      (click-prompt state :runner "Yes")
      (is (prompt-is-type? state :runner :select))
      (is (= "Choose a credit providing card (0 of 1 [Credits])" (:msg (prompt-map :runner)))
          "Credit selection prompt is opened")))

(deftest public-terminal-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Public Terminal" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Public Terminal")
      (let [pt (get-hardware state 0)]
        (is (changed? [(:credit (get-runner)) -1]
              (play-from-hand state :runner "Dirty Laundry")
              (click-card state :runner pt))
            "Used 1 credit from Public Terminal"))))

(deftest quianju-pt-activate-start-of-turn
  (do-game
    (new-game {:runner {:hand ["Qianju PT"] :credits 15}
               :corp {:hand ["Public Trail" "Public Trail"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Qianju PT")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [pt (get-hardware state 0)]
      (card-ability state :runner pt 0)
      (end-phase-12 state :runner)
      (is (= 3 (:click (get-runner))) "Spent 1 click on Qianju PT")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (play-from-hand state :corp "Public Trail")
      (click-prompt state :runner "Take 1 tag")
      (is (= 0 (count-tags state)) "Avoided a tag")
      (play-from-hand state :corp "Public Trail")
      (click-prompt state :runner "Take 1 tag")
      (is (= 1 (count-tags state)) "Took a tag (QPT expired)"))))

(deftest q-coherence-chip
  ;; Q-Coherence Chip
  (do-game
      (new-game {:runner {:deck [(qty "Q-Coherence Chip" 3)]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Q-Coherence Chip")
        (is (= 5 (core/available-mu state)) "Gain 1 memory"))
  (testing "Basic trash test"
    (do-game
      (new-game {:runner {:deck [(qty "Self-modifying Code" 3) "Q-Coherence Chip"]}})
      (starting-hand state :runner ["Self-modifying Code" "Q-Coherence Chip"])
      (take-credits state :corp)
      (play-from-hand state :runner "Q-Coherence Chip")
      (play-from-hand state :runner "Self-modifying Code")
      (let [smc1 (get-program state 0)
            qchip (get-hardware state 0)]
        (card-ability state :runner smc1 0)
        (click-prompt state :runner "Self-modifying Code")
        (is (= 3 (:credit (get-runner))) "Paid 2 for SMC, 0 for install - 3 credits left")
        (is (empty? (:hand (get-runner))) "Runner hand should be empty")
        (is (nil? (refresh qchip)) "Q chip should be trashed")))))

(deftest q-coherence-chip-program-trashed-from-hand-shouldn-t-trash-chip
    ;; program trashed from hand shouldn't trash chip
    (do-game
      (new-game {:corp {:deck [(qty "Breached Dome" 10)]}
                 :runner {:deck ["Self-modifying Code" "Q-Coherence Chip"]}})
      (starting-hand state :runner ["Self-modifying Code" "Q-Coherence Chip"])
      (starting-hand state :corp ["Breached Dome"])
      (play-from-hand state :corp "Breached Dome" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Q-Coherence Chip")
      (let [qchip (get-hardware state 0)]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (refresh qchip) "Q chip should NOT be trashed"))))

(deftest q-coherence-chip-program-milled-from-stack-shouldn-t-trash-chip
    ;; program milled from stack shouldn't trash chip
    (do-game
      (new-game {:corp {:deck [(qty "Breached Dome" 10)]}
                 :runner {:deck ["Self-modifying Code", (qty "Q-Coherence Chip" 2)]}})
      (starting-hand state :runner ["Q-Coherence Chip" "Q-Coherence Chip"])
      (starting-hand state :corp ["Breached Dome"])
      (play-from-hand state :corp "Breached Dome" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Q-Coherence Chip")
      (let [qchip (get-hardware state 0)]
        (run-empty-server state "Server 1")
        (click-prompt state :runner "No action")
        (is (refresh qchip) "Q chip should NOT be trashed"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game {:runner {:deck ["Sure Gamble" (qty "Rabbit Hole" 3)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Rabbit Hole")
    (is (= 1 (get-link state)))
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Yes")
    (is (= 3 (get-link state)))
    (is (= 3 (count (get-hardware state))))
    (is (= 2 (:click (get-runner))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-runner))) "Paid 2c for each of 3 copies")))

(deftest ramujan-reliant-550-bmi
  ;; Prevent up to X net or brain damage.
  (do-game
      (new-game {:corp {:deck ["Data Mine" "Snare!"]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand [(qty "Ramujan-reliant 550 BMI" 4) "Sure Gamble"]}})
      (play-from-hand state :corp "Data Mine" "New remote")
      (play-from-hand state :corp "Snare!" "Server 1")
      (let [dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)
              rr2 (get-hardware state 1)]
          (run-on state "Server 1")
          (rez state :corp dm)
          (run-continue state)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (click-prompt state :runner "1")
          (is (last-n-log-contains? state 1 "Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 2 (count (:hand (get-runner)))) "1 net damage prevented")
          (run-continue state)
          (click-prompt state :corp "No")
          (click-prompt state :runner "No action")
          (take-credits state :runner)
          (take-credits state :corp)
          (play-from-hand state :runner "Ramujan-reliant 550 BMI")
          (run-empty-server state "Server 1")
          (click-prompt state :corp "Yes")
          (card-ability state :runner rr2 0)
          (click-prompt state :runner "3")
          (is (second-last-log-contains? state "Sure Gamble, Sure Gamble, and Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 1 (count (:hand (get-runner)))) "3 net damage prevented")))))

(deftest ramujan-reliant-550-bmi-prevent-up-to-x-net-or-brain-damage-empty-stack
    ;; Prevent up to X net or brain damage. Empty stack
    (do-game
      (new-game {:corp {:deck ["Data Mine"]}
                 :runner {:deck ["Ramujan-reliant 550 BMI" "Sure Gamble"]}})
      (play-from-hand state :corp "Data Mine" "New remote")
      (let [dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)]
          (run-on state "Server 1")
          (rez state :corp dm)
          (run-continue state)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (click-prompt state :runner "Done")
          (is (zero? (count (:hand (get-runner)))) "Not enough cards in Stack for Ramujan to work")))))

(deftest recon-drone
  ;; trash and pay X to prevent that much damage from a card you are accessing
  (do-game
    (new-game {:corp {:deck ["Snare!" "House of Knives"
                             "Prisec" "Cerebral Overwriter"]}
               :runner {:deck [(qty "Recon Drone" 10)]}})
    (core/gain state :corp :click 10)
    (core/gain state :corp :credit 100)
    (play-from-hand state :corp "House of Knives" "New remote")
    (play-from-hand state :corp "Snare!" "New remote")
    (play-from-hand state :corp "Prisec" "New remote")
    (play-from-hand state :corp "Cerebral Overwriter" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (advance state (get-content state :remote4 0))
    (take-credits state :corp)
    (core/gain state :runner :click 100)
    (core/gain state :runner :credit 100)
    (draw state :runner)
    (draw state :runner)
    (draw state :runner)
    (draw state :runner)
    (play-from-hand state :runner "Recon Drone")
    (play-from-hand state :runner "Recon Drone")
    (play-from-hand state :runner "Recon Drone")
    (play-from-hand state :runner "Recon Drone")
    (let [rd1 (get-hardware state 0)
          rd2 (get-hardware state 1)
          rd3 (get-hardware state 2)
          rd4 (get-hardware state 3)
          hok (get-scored state :corp 0)]
      (run-empty-server state "Server 2")
      (is (= :waiting (prompt-type :runner)) "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd1 0)
      (click-prompt state :runner "3")
      (click-prompt state :runner "No action")
      (is (= 5 (count (:hand (get-runner)))) "Runner took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (prompt-type :runner)) "Runner has prompt to wait for Snare!")
      (card-ability state :corp hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :runner rd2 0)
      (is (nil? (:number (:choices (prompt-map :runner)))) "No choice to prevent damage from HOK")
      (is (= 4 (count (:hand (get-runner)))) "Runner took 1 net damage from HOK")
      (click-prompt state :corp "No")
      (click-prompt state :runner "No action")
      (core/lose state :runner :credit :all)
      ; can only stop 1 damage due to credits
      (core/gain state :runner :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (prompt-type :runner)) "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd2 0)
      (is (= 1 (:credit (get-runner))) "Runner has 1 credit")
      (is (= 1 (:number (:choices (prompt-map :runner)))) "Recon Drone choice limited to runner credits")
      (click-prompt state :runner "1")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 2 net damage from Snare!")
      (core/gain state :runner :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (prompt-type :runner)) "Runner has prompt to wait for Prisec")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd3 0)
      (is (= 100 (:credit (get-runner))) "Runner has 100 credits")
      (is (= 100 (:number (:choices (prompt-map :runner)))) "Recon Drone choice is not limited to 1 meat")
      (click-prompt state :runner "1")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (prompt-type :runner)) "Runner has prompt to wait for Cerebral Overwriter")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd4 0)
      (click-prompt state :runner "1")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no core damage"))))

(deftest record-reconstructor
  ;; Record Reconstructor
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hedge Fund"]
                      :discard ["PAD Campaign"]}
               :runner {:hand ["Record Reconstructor"]}})
    (take-credits state :corp)
    (run-empty-server state :archives)
    (play-from-hand state :runner "Record Reconstructor")
    (is (= "Hedge Fund" (-> (get-corp) :deck first :title)) "Hedge Fund is on top of R&D")
    (run-empty-server state :archives)
    (click-prompt state :runner "Record Reconstructor")
    (click-prompt state :runner "PAD Campaign")
    (is (= "PAD Campaign" (-> (get-corp) :deck first :title)) "PAD Campaign is now first card in R&D")))

(deftest replicator-interaction-with-bazaar-issue-1511
    ;; interaction with Bazaar. Issue #1511
    (do-game
      (new-game {:runner {:deck ["Replicator" "Bazaar" (qty "Spy Camera" 6)]}})
      (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-runner) :rig :hardware)))))]
        (take-credits state :corp)
        (starting-hand state :runner ["Replicator" "Bazaar" "Spy Camera"])
        (play-from-hand state :runner "Replicator")
        (play-from-hand state :runner "Bazaar")
        (play-from-hand state :runner "Spy Camera") ; 1 installed
        (is (count-spy 1) "1 Spy Cameras installed")
        (click-prompt state :runner "Yes") ; for now, choosing Replicator then shows its optional Yes/No
        (click-prompt state :runner "Yes") ; Bazaar triggers, 2 installed
        (is (count-spy 2) "2 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 3 installed
        (is (count-spy 3) "3 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 4 installed
        (is (count-spy 4) "4 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 5 installed
        (is (count-spy 5) "5 Spy Cameras installed")
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Yes")  ; 6 installed
        (is (count-spy 6) "6 Spy Cameras installed"))))

(deftest respirocytes-should-draw-multiple-cards-when-multiple-respirocytes-are-in-play
    ;; Should draw multiple cards when multiple respirocytes are in play
    (do-game
      (new-game {:runner {:deck [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]
                          :hand ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"]}})
      (take-credits state :corp)
      (dotimes [_ 2]
        (play-from-hand state :runner "Respirocytes"))
      (is (= 2 (count (:discard (get-runner)))) "2 damage done")
      (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")))

(deftest respirocytes-should-not-trigger-after-being-trashed-issue-3699
    ;; Respirocytes should not trigger after being trashed (issue #3699)
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 20)]
                          :hand ["Respirocytes" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Respirocytes")
      (is (= 1 (-> (get-runner) :discard count)) "Took 1 damage from Respirocytes")
      (is (= 1 (-> (get-runner) :hand count)) "Drew 1 from Respirocytes")
      (let [respirocytes (get-hardware state 0)]
        (is (= 1 (get-counters (refresh respirocytes) :power)) "Respirocytes drew once")
        (take-credits state :runner)
        (take-credits state :corp)
        (dotimes [_ 2]
          (play-from-hand state :runner "Sure Gamble")
          (is (= 1 (-> (get-runner) :hand count)) "Drew 1 from Respirocytes")
          (take-credits state :runner)
          (take-credits state :corp))
        (is (= 1 (-> (get-runner) :hand count)) "1 card in hand")
        (is (zero? (-> (get-runner) :rig :hardware count)) "Respirocytes expired")
        (play-from-hand state :runner "Sure Gamble")
        (is (zero? (-> (get-runner) :hand count))
            "Respirocytes did not trigger when trashed")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (zero? (-> (get-runner) :hand count))
            "Respirocytes still does not trigger when trashed"))))

(deftest respirocytes-should-wait-to-draw-until-after-draws-are-handled-4190
    ;; Should wait to draw until after draws are handled. #4190
    (do-game
      (new-game {:runner {:hand [(qty "Respirocytes" 4) "The Class Act"
                                 "Acacia" "Bankroll" "Cache"]
                          :credits 10}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Acacia" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Bankroll" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Cache" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "The Class Act")
      (play-from-hand state :runner "Respirocytes")
      (play-from-hand state :runner "Respirocytes")
      (is (= 2 (count (:discard (get-runner)))) "2 damage done")
      (is (= ["Acacia" "Bankroll"] (map :title (:set-aside (get-runner)))) "First Respirocytes triggers The Class Act")
      (is (= 2 (count (:set-aside (get-runner)))) "Runner hasn't drawn anything yet")
      (click-card state :runner "Acacia")
      (is (= ["Bankroll" "Cache"] (->> (get-runner) :hand (map :title)))
          "Acacia is on the bottom of the deck so Runner drew Cache")))

(deftest rubicon-switch
  ;; Rubicon Switch
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Pachinko"]}
               :runner {:deck ["Rubicon Switch"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Pachinko" "R&D")
    (let [iw (get-ice state :hq 0)
          pach (get-ice state :rd 0)]
      (rez state :corp iw)
      (take-credits state :corp)
      (play-from-hand state :runner "Rubicon Switch")
      (rez state :corp pach)
      (let [rs (get-hardware state 0)]
        (card-ability state :runner rs 0)
        (click-prompt state :runner "1")
        (click-card state :runner "Ice Wall")
        (is (rezzed? (refresh iw)) "Ice Wall rezzed last turn can't be targeted")
        (click-card state :runner "Pachinko")
        (is (not (rezzed? (refresh pach))) "Pachinko derezzed")
        (is (= 2 (:click (get-runner))) "Spent 1 click")
        (is (= 1 (:credit (get-runner))) "Spent 1c")))))

(deftest security-nexus
  ;; Security Nexus
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Security Nexus"]
                        :credits 100}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Security Nexus")
    (let [iw (get-ice state :rd 0)]
      (run-on state "R&D")
      (rez state :corp iw)
      (run-continue state)
      (is (zero? (count-tags state)) "Runner should have no tags to start")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Run should end from losing Security Nexus trace")
      (is (= 1 (count-tags state)) "Runner should take 1 tag from losing Security Nexus trace")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state "R&D")
      (run-continue state)
      (is (= :encounter-ice (:phase (:run @state))) "Run should be in encounter phase")
      (click-prompt state :runner "Yes")
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
      (is (= :movement (:phase (:run @state))) "Security Nexus should bypass Ice Wall")
      (is (:run @state) "Run should still be going on from winning Security Nexus trace")
      (is (= 1 (count-tags state)) "Runner should still only have 1 tag"))))

(deftest severnius-stim-implant
  ;; Severnius Stim Implant
  (do-game
    (new-game {:corp {:deck ["Hedge Fund" "Hostile Takeover"]
                      :hand [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Severnius Stim Implant" "Sure Gamble" "Lucky Find" "Easy Mark"]}})
    (core/move state :corp (find-card "Hedge Fund" (:deck (get-corp))) :deck)
    (core/move state :corp (find-card "Hostile Takeover" (:deck (get-corp))) :deck)
    (take-credits state :corp)
    (play-from-hand state :runner "Severnius Stim Implant")
    (card-ability state :runner (get-hardware state 0) 0)
    (click-prompt state :runner "R&D")
    (click-card state :runner "Sure Gamble")
    (click-card state :runner "Lucky Find")
    (click-prompt state :runner "Done")
    (run-continue state)
    (click-prompt state :runner "No action") ; First card
    (click-prompt state :runner "Steal") ; Second card, due to additional access
    (is (nil? (:run @state)) "Run is over")))

(deftest sifr
  ;; Sifr - Once per turn drop encountered ice strength to zero
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game {:corp {:deck ["Archangel" "IP Block" "Hedge Fund"]}
               :runner {:deck ["Modded" "Clone Chip" "Şifr" "Parasite"]}})
    (core/gain state :corp :credit 100)
    (core/gain state :runner :credit 100)
    (play-from-hand state :corp "Archangel" "HQ")
    (play-from-hand state :corp "IP Block" "HQ")
    (take-credits state :corp)
    (trash-from-hand state :runner "Parasite")
    (play-from-hand state :runner "Şifr")
    (is (= 2 (count (:hand (get-runner)))) "Modded and Clone Chip in hand")
    (let [arch (get-ice state :hq 0)
          ip (get-ice state :hq 1)
          sifr (get-hardware state 0)]
      (rez state :corp arch)
      (rez state :corp ip)
      (is (= 4 (get-strength (refresh ip))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (:position (:run @state))))
      (is (not (no-prompt? state :corp)) "Şifr prompt")
      (click-prompt state :runner "Yes")
      (is (zero? (get-strength (refresh ip))))
      (run-continue-until state :encounter-ice)
      (is (= 1 (:position (:run @state))))
      (is (= 2 (count (:hand (get-runner))))) ; pre archangel
      (card-subroutine state :corp arch 0) ; fire archangel
      (is (not (no-prompt? state :corp)) "Archangel trace prompt - corp")
      (click-prompt state :corp "0")
      (is (not (no-prompt? state :runner)) "Archangel trace prompt - runner")
      (click-prompt state :runner "0")
      (click-card state :corp sifr)
      (is (= 3 (count (:hand (get-runner))))) ; sifr got lifted to hand
      (run-continue state :movement)
      (run-jack-out state)
      (is (= 4 (get-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Modded")
      (is (not (no-prompt? state :runner)) "Modded choice prompt exists")
      (click-card state :runner "Şifr")
      (is (= 4 (get-strength (refresh ip))) "IP Block back to standard strength"))))

(deftest sifr-works-with-chisel
  (do-game
    (new-game {:runner {:hand ["Şifr" "Chisel"] :credits 10}
               :corp {:hand ["Assassin"] :credits 10}})
    (play-from-hand state :corp "Assassin" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Chisel")
    (click-card state :runner "Assassin")
    (play-from-hand state :runner "Şifr")
    (run-on state :hq)
    (run-continue state)
    (click-prompt state :runner "Şifr")
    (click-prompt state :runner "Yes")
    (is (= 1 (count (:discard (get-corp)))) "Trashed Assassin")))

(deftest silencer-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Silencer" "Dagger"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Silencer")
      (play-from-hand state :runner "Dagger")
      (let [sil (get-hardware state 0)
            dag (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner dag 1)
              (click-card state :runner sil))
            "Used 1 credit from Silencer"))))

(deftest simulchip-with-a-program-already-in-the-heap
    ;; with a program already in the heap
    (testing "and a program trashed this turn"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment"]}
                   :runner {:hand ["Simulchip" "Corroder"]
                            :discard ["Mantle"]
                            :credits 3}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Simulchip")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner "Corroder")
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner (get-hardware state 0) 0)
              (click-card state :runner "Mantle"))
            "Mantle is installed for free")
        (is (get-program state 0) "Mantle is installed for free")))
    (testing "and no program trashed this turn and a card to trash as additional cost"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment"]}
                   :runner {:hand ["Simulchip" "Corroder"]
                            :discard ["Mantle"]
                            :credits 20}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Simulchip")
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner (get-hardware state 0) 0)
              ;; Issue #4889
              (is (= "Choose 1 installed program to trash" (:msg (prompt-map :runner)))
                  "Runner chooses program to trash as a cost")
              (click-card state :runner "Corroder"))
            "Corroder is installed for free")
        (is (= "Choose a target for Simulchip" (:msg (prompt-map :runner)))
            "Runner chooses ability target first")
        (click-card state :runner "Mantle")
        (is (get-program state 0) "Mantle is installed for free")
        (is (find-card "Corroder" (:discard (get-runner))) "Corroder has been trashed")))
    (testing "and no program trashed this turn and no card to trash as additional cost"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment"]}
                   :runner {:hand ["Simulchip"]
                            :discard ["Mantle"]
                            :credits 20}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Simulchip")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (no-prompt? state :runner) "No Simulchip prompt")))
    (testing "and not enough credits"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment"]}
                   :runner {:hand ["Simulchip" "Mass-Driver"]
                            :credits 13}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Mass-Driver")
        (play-from-hand state :runner "Simulchip")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner "Mass-Driver")
        (is (= 4 (:credit (get-runner))) "Need 5 credits to install Mass-Driver, only have 4")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (no-prompt? state :runner) "No Simulchip prompt")))
    (testing "Heap Locked Test"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment" "Blacklist"]}
                   :runner {:hand ["Simulchip" "Simulchip" "Corroder"]
                            :discard ["Mantle"]
                            :credits 5}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (play-from-hand state :corp "Blacklist" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Simulchip")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner "Corroder")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (not (no-prompt? state :runner)) "Simulchip prompt came up")
        (click-prompt state :runner "Done")
        (rez state :corp (refresh (get-content state :remote2 0)))
        (play-from-hand state :runner "Simulchip")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (no-prompt? state :runner) "Simulchip prompt did not come up"))))

(deftest simulchip-with-no-programs-in-the-heap
    ;; with no programs in the heap
    (testing "and a program trashed this turn"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment"]}
                   :runner {:hand ["Simulchip" "Corroder"]
                            :credits 3}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Simulchip")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay to steal")
        (click-card state :runner "Corroder")
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner (get-hardware state 0) 0)
              (click-card state :runner "Corroder"))
            "Corroder is installed for free")
        (is (get-program state 0) "Corroder is installed for free")))
    (testing "and no program trashed this turn"
      (do-game
        (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["SDS Drone Deployment"]}
                   :runner {:hand ["Simulchip" "Corroder"]
                            :credits 20}})
        (play-from-hand state :corp "SDS Drone Deployment" "New remote")
        (take-credits state :corp)
        (core/gain state :runner :click 4)
        (play-from-hand state :runner "Corroder")
        (play-from-hand state :runner "Simulchip")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (no-prompt? state :runner) "No Simulchip prompt"))))

(deftest simulchip-no-additional-cost-when-hosted-program-is-trashed-issue-4897
    ;; No additional cost when hosted program is trashed. Issue #4897
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Simulchip" "Chisel"]
                          :credits 20}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [iw (get-ice state :hq 0)]
        (take-credits state :corp)
        (core/gain state :runner :click 5)
        (play-from-hand state :runner "Chisel")
        (click-card state :runner "Ice Wall")
        (run-on state "HQ")
        (rez state :corp (refresh iw))
        (run-continue-until state :movement)
        (run-jack-out state)
        (run-on state "HQ")
        (run-continue-until state :movement)
        (run-jack-out state)
        (play-from-hand state :runner "Simulchip")
        (card-ability state :runner (get-hardware state 0) 0)
        (is (= "Choose a target for Simulchip" (:msg (prompt-map :runner)))
            "Runner has ability target prompt"))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
      (new-game {:corp {:deck ["Caduceus"]}
                 :runner {:deck ["Spinal Modem" "Sure Gamble"]}})
      (play-from-hand state :corp "Caduceus" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Spinal Modem")
      (let [cad (get-ice state :hq 0)
            sm (get-hardware state 0)]
        (is (= 5 (core/available-mu state)))
        (is (= 2 (get-counters (refresh sm) :recurring)))
        (run-on state :hq)
        (rez state :corp cad)
        (run-continue state)
        (card-subroutine state :corp cad 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (:brain-damage (get-runner))) "Took 1 core damage")
        (is (= 1 (count (:discard (get-runner)))))
        (is (= 4 (hand-size :runner)) "Reduced hand size"))))

(deftest solidarity-badge-draw
  (do-game
    (new-game {:corp {:hand ["Rashida Jaheem"]}
               :runner {:hand ["Solidarity Badge"]
                        :deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Solidarity Badge")
    (run-empty-server state "HQ")
    (is (changed? [(get-counters (get-hardware state 0) :power) 1]
          (click-prompt state :runner "Pay 1 [Credits] to trash"))
        "added a counter to Solidarity Badge")
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "Draw 1 card")
    (is (= 1 (count (:hand (get-runner)))))))

(deftest solidarity-badge-tag
  (do-game
    (new-game {:corp {:hand ["Rashida Jaheem"]}
               :runner {:hand ["Solidarity Badge"]
                        :deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Solidarity Badge")
    (run-empty-server state "HQ")
    (is (changed? [(get-counters (get-hardware state 0) :power) 1]
          (click-prompt state :runner "Pay 1 [Credits] to trash"))
        "added a counter to Solidarity Badge")
    (core/gain state :runner :tag 1)
    (take-credits state :runner)
    (take-credits state :corp)
    (click-prompt state :runner "Remove 1 tag")
    (is (zero? (count-tags state)))))

(deftest solidarity-badge-first-trash-only
  (do-game
    (new-game {:corp {:hand ["Rashida Jaheem" "Rashida Jaheem"]}
               :runner {:hand ["Solidarity Badge"]}})
    (take-credits state :corp)
    (run-empty-server state "HQ")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (play-from-hand state :runner "Solidarity Badge")
    (run-empty-server state "HQ")
    (is (changed? [(get-counters (get-hardware state 0) :power) 0]
          (click-prompt state :runner "Pay 1 [Credits] to trash"))
        "added no counter to Solidarity Badge")))

(deftest spinal-modem-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Spinal Modem" "Inti"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Spinal Modem")
      (play-from-hand state :runner "Inti")
      (let [sm (get-hardware state 0)
            inti (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner inti 1)
              (click-card state :runner sm)
              (click-card state :runner sm))
            "Used 2 credits from Spinal Modem"))))

(deftest sports-hopper
  ;; Sports Hopper
  (do-game
    (new-game {:runner {:deck [(qty "Sports Hopper" 3) (qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Sports Hopper"])
    (take-credits state :corp)
    (play-from-hand state :runner "Sports Hopper")
    (is (= 1 (get-link state)) "Gained 1 link")
    (card-ability state :runner (get-hardware state 0) 0)
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (zero? (get-link state)) "Lost link")))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game {:runner {:deck [(qty "Spy Camera" 6) "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron" "Kati Jones"]}})
    (starting-hand state :runner ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-runner)))))
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    (core/gain state :runner :click 3)
    (dotimes [_ 6] (play-from-hand state :runner "Spy Camera"))
    (let [spy (get-hardware state 5)]
      ;; look at top 6 cards
      (card-ability state :runner spy 0)
      (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Kati Jones" (:deck (get-runner))))
      ;; try starting over
      (click-prompt state :runner "Start over")
      (click-prompt state :runner (find-card "Kati Jones" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
      (click-prompt state :runner "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Desperado" (:title (second (:deck (get-runner))))))
      (is (= "Diesel" (:title (second (rest (:deck (get-runner)))))))
      (is (= "Corroder" (:title (second (rest (rest (:deck (get-runner))))))))
      (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-runner)))))))))
      (is (= "Kati Jones" (:title (second (rest (rest (rest (rest (:deck (get-runner))))))))))
      ;; look at top card of R&D
      (card-ability state :runner spy 1)
      (is (= "The top card of R&D is Hedge Fund" (:msg (prompt-map :runner))))
      (is (= 1 (count (:discard (get-runner))))))))

(deftest supercorridor
  ;; Supercorridor - may gain 2c at end of turn if corp and runner have same number of credits
  (do-game
    (new-game {:runner {:deck ["Supercorridor"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Supercorridor")
    (take-credits state :runner)
    (is (= (:credit (get-runner)) 4) "Do not gain credits if differing number of credits")
    (take-credits state :corp)
    (core/gain state :runner :credit 3)
    (take-credits state :runner)
    (is (= (:credit (get-runner)) (:credit (get-corp))) "Corp and Runner have same number of credits")
    (click-prompt state :runner "No")
    (is (= (:credit (get-runner)) (:credit (get-corp))) "Number of credits did not change")
    (take-credits state :corp)
    (core/lose state :runner :credit 1)
    (take-credits state :runner)
    (is (= (:credit (get-runner)) (:credit (get-corp))) "Corp and Runner have same number of credits (2nd time)")
    (let [credits (:credit (get-runner))]
      (click-prompt state :runner "Yes")
      (is (= (:credit (get-runner)) (+ 2 credits)) "Runner gained 2 credits"))))

(deftest swift
  (letfn [(laundry-archives [state]
            (play-from-hand state :runner "Dirty Laundry")
            (click-prompt state :runner "Archives")
            (run-continue state))]
    (testing "Installing Swift gives the runner +1[mu]"
      (do-game
        (new-game {:runner {:hand ["Swift"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Swift")
        (is (= 5 (core/available-mu state))) "The runner has 5[mu]"))
    (testing "Playing a run event must gain the runner a click"
      (do-game
        (new-game {:runner {:hand ["Swift" "Dirty Laundry"]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Swift")
        (laundry-archives state)
        (is (= 3 (:click (get-runner))) "Gain a click after playing the run event")))
    (testing "Playing a second run event must not gain the runner a click"
      (do-game
        (new-game {:runner {:hand ["Swift", (qty "Dirty Laundry" 2)]}})
        (take-credits state :corp)
        (play-from-hand state :runner "Swift")
        (laundry-archives state)
        (is (= 3 (:click (get-runner))) "Gain a click after playing the first run event")
        (laundry-archives state)
        (is (= 2 (:click (get-runner))) "Don't gain a click after playing the second run event")))
    (testing "Playing a run event, installing Swift, then playing another run event must not gain the runner a click"
      (do-game
        (new-game {:runner {:hand ["Swift", (qty "Dirty Laundry" 2)]}})
        (take-credits state :corp)
        (laundry-archives state)
        (is (= 3 (:click (get-runner))) "Gain a click after playing the first run event")
        (play-from-hand state :runner "Swift")
        (laundry-archives state)
        (is (= 1 (:click (get-runner))) "Don't gain a click after playing the second run event")))))

(deftest t400-memory-diamond
  ;; T400 Memory Diamond
  (do-game
      (new-game {:runner {:hand ["T400 Memory Diamond"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "T400 Memory Diamond")
      (is (= 6 (hand-size :runner)) "Increased hand size")
      (is (= 5 (core/available-mu state)) "Gain 1 memory")))

(deftest the-gauntlet-doesn-t-give-additional-accesses-when-no-ice-are-broken
    ;; Doesn't give additional accesses when no ice are broken
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" (qty "Beanstalk Royalties" 2)]}
                 :runner {:hand ["Corroder" "The Gauntlet"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "The Gauntlet")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue-until state :success)
      (is (= 1 (:random-access-limit (core/num-cards-to-access state :runner :hq nil))) "Only access 1 card from HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Access prompts are done")
      (is (not (:run @state)) "Run has ended")))

(deftest the-gauntlet-access-additional-cards-when-breaking-ice-protecting-hq
    ;; Access additional cards when breaking ice protecting HQ
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" (qty "Beanstalk Royalties" 2)]}
                 :runner {:hand ["Corroder" "The Gauntlet"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "The Gauntlet")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue-until state :success)
      (is (= 2 (:random-access-limit (core/num-cards-to-access state :runner :hq nil))) "Access 2 cards from HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Access prompts are done")
      (is (not (:run @state)) "Run has ended")))

(deftest the-gauntlet-only-access-additional-cards-for-fully-broken-ice
    ;; Only access additional cards for fully-broken ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Battlement" (qty "Beanstalk Royalties" 2)]}
                 :runner {:hand ["Corroder" "The Gauntlet"]
                          :credits 10}})
      (play-from-hand state :corp "Battlement" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "The Gauntlet")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue-until state :approach-ice)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (click-prompt state :runner "Done")
      (run-continue-until state :success)
      (is (= 2 (:random-access-limit (core/num-cards-to-access state :runner :hq nil))) "Access 2 cards from HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Access prompts are done")
      (is (not (:run @state)) "Run has ended")))

(deftest the-gauntlet-only-access-additional-cards-when-breaking-ice-protecting-hq
    ;; Only access additional cards when breaking ice protecting HQ
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" (qty "Beanstalk Royalties" 2)]}
                 :runner {:hand ["Corroder" "The Gauntlet" "Sneakdoor Beta"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Sneakdoor Beta")
      (play-from-hand state :runner "The Gauntlet")
      (card-ability state :runner (get-program state 1) 0)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue-until state :success)
      (is (= 1 (:random-access-limit (core/num-cards-to-access state :runner :hq nil))) "Access 1 cards from HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Access prompts are done")
      (is (not (:run @state)) "Run has ended")))

(deftest the-gauntlet-access-additional-cards-when-fully-broken-ice-is-derezzed
    ;; Access additional cards when fully broken ice is derezzed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" (qty "Beanstalk Royalties" 2)]}
                 :runner {:hand ["Saker" "The Gauntlet"]
                          :credits 15}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Saker")
      (play-from-hand state :runner "The Gauntlet")
      (run-on state "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (card-ability state :runner (get-program state 0) 2)
      (is (not (rezzed? (get-ice state :hq 0))) "Ice Wall has been derezzed")
      (run-continue-until state :success)
      (is (= 2 (:random-access-limit (core/num-cards-to-access state :runner :hq nil))) "Access 2 cards from HQ")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Access prompts are done")
      (is (not (:run @state)) "Run has ended")))

(deftest the-gauntlet-don-t-access-additional-cards-when-fully-broken-ice-is-trashed
    ;; Don't access additional cards when fully broken ice is trashed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" (qty "Beanstalk Royalties" 2)]}
                 :runner {:hand ["Corroder" "The Gauntlet" "Knifed"]
                          :credits 15}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "The Gauntlet")
      (play-from-hand state :runner "Knifed")
      (click-prompt state :runner "HQ")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (is (nil? (get-ice state :hq 0)) "Ice Wall has been trashed")
      (run-continue-until state :success)
      (is (= 1 (:random-access-limit (core/num-cards-to-access state :runner :hq nil))) "Access 1 card from HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Access prompts are done")
      (is (not (:run @state)) "Run has ended")))

(deftest the-gauntlet-access-additional-cards-on-run-on-hq-not-with-gang-sign-issue-2749
    ;; Access additional cards on run on HQ, not with Gang Sign. Issue #2749
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"
                               (qty "Hedge Fund" 3)]}
                 :runner {:deck ["The Gauntlet"
                                 "Gang Sign"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "The Gauntlet")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Gang Sign should trigger, without The Gauntlet pop-up
      (let [gs (get-resource state 0)]
        (prompt-is-card? state :runner gs))
      (is (= ["No action"] (prompt-buttons :runner)))
      (click-prompt state :runner "No action")))

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an icebreaker
  (do-game
    (new-game {:runner {:deck ["The Personal Touch"
                               "Paricia"
                               "Faerie"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (play-from-hand state :runner "Faerie")
    (let [par (get-program state 0)
          fae (get-program state 1)]
      (is (= 2 (get-strength (refresh fae))))
      (play-from-hand state :runner "The Personal Touch")
      (click-card state :runner par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-icebreaker")
      (click-card state :runner fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (get-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest the-toolbox-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["The Toolbox" "Inti"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 9)
      (play-from-hand state :runner "The Toolbox")
      (play-from-hand state :runner "Inti")
      (let [tt (get-hardware state 0)
            inti (get-program state 0)]
        (is (changed? [(:credit (get-runner)) 0]
              (card-ability state :runner inti 1)
              (click-card state :runner tt)
              (click-card state :runner tt))
            "Used 2 credits from The Toolbox"))))

(deftest the-wizards-chest
  (do-game
    (new-game {:runner {:hand ["The Wizard's Chest"]
                        :discard ["Legwork" "Corroder" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Egret" "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Wizard's Chest")
    (let [chest (get-hardware state 0)]
      ;; TODO: make this a helper or something for consistently ordered starting deck
      (doseq [card-name ["Legwork" "Corroder" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Egret" "Earthrise Hotel"]]
        (core/move state :runner (find-card card-name (get-in @state [:runner :discard])) :deck))
      (card-ability state :runner chest 0)
      (is (no-prompt? state :runner) "Cannot trigger The Wizard's Chest until all centrals ran")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (card-ability state :runner (refresh chest) 0)
      (is (= ["Hardware" "Program" "Resource" "Cancel"] (prompt-buttons :runner)))
      (click-prompt state :runner "Program")
      (is (= ["Install Corroder" "Install Femme Fatale" "No install"] (prompt-buttons :runner)))
      (is (changed? [(:credit (get-runner)) 0]
                    (click-prompt state :runner "Install Femme Fatale"))
          "Install at no cost")
      (is (= "Femme Fatale" (:title (get-program state 0))) "Femme Fatale is installed")
      (is (second-last-log-contains? state (str "Runner uses The Wizard's Chest"
                                                " to reveal Legwork, Corroder, Ice Carver, Prepaid VoicePAD, Femme Fatale from the top of the stack"
                                                " and install Femme Fatale, ignoring all costs."))))))

(deftest the-wizards-chest-single-card-selection
  (do-game
    (new-game {:runner {:hand ["The Wizard's Chest"]
                        :discard ["Legwork" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Wizard's Chest")
    (let [chest (get-hardware state 0)]
      ;; TODO: make this a helper or something for consistently ordered starting deck
      (doseq [card-name ["Legwork" "Ice Carver" "Prepaid VoicePAD" "Femme Fatale" "Earthrise Hotel"]]
        (core/move state :runner (find-card card-name (get-in @state [:runner :discard])) :deck))
      (card-ability state :runner chest 0)
      (is (no-prompt? state :runner) "Cannot trigger The Wizard's Chest until all centrals ran")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (card-ability state :runner (refresh chest) 0)
      (is (= ["Hardware" "Program" "Resource" "Cancel"] (prompt-buttons :runner)))
      (click-prompt state :runner "Program")
      (is (= ["Install Femme Fatale" "No install"] (prompt-buttons :runner)))
      (is (changed? [(:credit (get-runner)) 0]
                    (click-prompt state :runner "Install Femme Fatale"))
          "Install at no cost")
      (is (= "Femme Fatale" (:title (get-program state 0))) "Femme Fatale is installed")
      (is (second-last-log-contains? state (str "Runner uses The Wizard's Chest"
                                                " to reveal Legwork, Ice Carver, Prepaid VoicePAD, Femme Fatale, Earthrise Hotel from the top of the stack"
                                                " and install Femme Fatale, ignoring all costs."))))))

(deftest time-bomb
  (do-game
    (new-game {:runner {:hand ["Time Bomb"]}
               :corp {:hand ["Hedge Fund" "IPO" "Restructure" "Beanstalk Royalties"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Time Bomb")
    (is (= 1 (count (:hand (get-runner)))) "Time Bomb not played because of no run")
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Time Bomb")
    (is (zero? (count (:hand (get-runner)))) "Able to play time bomb")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 2 (get-counters (get-hardware state 0) :power)))
    (take-credits state :runner)
    (take-credits state :corp)
    (is (= 3 (get-counters (get-hardware state 0) :power)))
    (take-credits state :runner)
    (take-credits state :corp)
    (click-card state :corp "Hedge Fund")
    (click-card state :corp "IPO")
    (click-card state :corp "Restructure")
    (is (no-prompt? state :corp))))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Corp turn against Chronos Protocol
  (do-game
    (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                      :deck ["Pup" "Viktor 1.0"
                             "Neural EMP"]}
               :runner {:deck [(qty "Titanium Ribs" 2) "Sure Gamble"
                               "Fall Guy" "Kati Jones"]}})
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Viktor 1.0" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Titanium Ribs")
    (click-card state :runner (find-card "Titanium Ribs" (:hand (get-runner))))
    (click-card state :runner (find-card "Kati Jones" (:hand (get-runner))))
    (is (no-prompt? state :runner) "Fall Guy didn't try to prevent trashing of Kati")
    (is (= 2 (count (:discard (get-runner)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)]
      (rez state :corp pup)
      (run-continue state)
      (card-subroutine state :corp pup 0)
      (click-prompt state :runner "Suffer 1 net damage")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner)))) ; Ribs takes precedence over CP on Runner turn
      (is (= 3 (count (:discard (get-runner)))) "Chose card lost from 1 net damage")
      (run-continue state :movement)
      (run-jack-out state)
      (take-credits state :runner)
      (core/move state :runner (find-card "Sure Gamble" (:discard (get-runner))) :hand)
      (core/move state :runner (find-card "Kati Jones" (:discard (get-runner))) :hand)
      (play-from-hand state :corp "Neural EMP")
      (click-prompt state :corp "Yes")
      (let [kati (find-card "Kati Jones" (:hand (get-runner)))]
        (click-prompt state :corp kati) ; Chronos Protocol takes precedence over Ribs on Corp turn
        (is (= 2 (count (:discard (get-runner)))) "Card chosen by Corp for first net damage")))))

(deftest top-hat
  ;; Top Hat
  (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Neural Net"]}
                 :runner {:deck ["Top Hat"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Dedicated Neural Net" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      (is (= (:title (nth (-> @state :corp :deck) 3)) "Dedicated Neural Net"))
      ;; R&D is now from top to bottom: A B C D
      (take-credits state :corp)
      (play-from-hand state :runner "Top Hat")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Top Hat") ;Top Hat Prompt
      (click-prompt state :runner "4") ;select ABT
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner stole DNN")))

(deftest top-hat-ash-interaction
    ;; Ash interaction
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Neural Net" "Ash 2X3ZB9CY"]}
                 :runner {:deck ["Top Hat"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Dedicated Neural Net" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      (is (= (:title (nth (-> @state :corp :deck) 3)) "Dedicated Neural Net"))
      ;; R&D is now from top to bottom: A B C D
      (play-from-hand state :corp "Ash 2X3ZB9CY" "R&D")
      (let [ash (get-content state :rd 0)]
        (rez state :corp ash)
        (take-credits state :corp)
        (core/gain state :runner :click 100)
        (core/gain state :runner :credit 100)
        (play-from-hand state :runner "Top Hat")
        (run-empty-server state "R&D")
        (click-prompt state :corp "0") ; init Ash trace
        (click-prompt state :runner "0") ; lose Ash trace
        (click-prompt state :runner "Top Hat") ; Top Hat activation
        (click-prompt state :runner "1") ; Top Hat
        (is (no-prompt? state :runner) "Can't trash Ash"))))

(deftest top-hat-mad-dash-interaction-issue-4542
    ;; Mad Dash interaction issue #4542
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Neural Net"]}
                 :runner {:deck ["Top Hat" (qty "Mad Dash" 4)]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Dedicated Neural Net" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      (is (= (:title (nth (-> @state :corp :deck) 3)) "Dedicated Neural Net"))
      ;; R&D is now from top to bottom: A B C D
      (take-credits state :corp)
      (core/gain state :runner :click 100)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Top Hat")
      ;; Not stealing agenda
      (play-from-hand state :runner "Mad Dash")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (click-prompt state :runner "Top Hat") ; Top Hat activation
      (is (= 0 (count (:discard (get-runner)))) "No damage yet")
      (click-prompt state :runner "2") ; Top Hat - accessing Brainstorm
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-runner)))) "Now the meat damage fires")
      ;; Stealing agenda
      (play-from-hand state :runner "Mad Dash")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (click-prompt state :runner "Top Hat") ; Top Hat activation
      (click-prompt state :runner "1") ; Top Hat - accessing Accelerated Beta Test
      (click-prompt state :runner "Steal")
      (is (= 3 (:agenda-point (get-runner))) "Runner got 3 points")
      (is (= 2 (count (:scored (get-runner)))) "Runner got 2 cards in score area")))

(deftest turntable
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (do-game
      (new-game {:corp {:deck ["Domestic Sleepers" "Project Vitruvius"]}
                 :runner {:deck ["Turntable"]}})
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (let [ag1 (get-content state :remote1 0)]
        (score-agenda state :corp ag1)
        (take-credits state :corp)
        (play-from-hand state :runner "Turntable")
        (is (= 3 (:credit (get-runner))))
        (let [tt (get-hardware state 0)]
          (run-empty-server state "HQ")
          (click-prompt state :runner "Steal")
          (is (zero? (:agenda-point (get-runner))) "Stole Domestic Sleepers")
          (is (prompt-is-card? state :runner tt))
          (click-prompt state :runner "Yes")
          (click-card state :runner (find-card "Project Vitruvius" (:scored (get-corp))))
          (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
          (is (zero? (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")))))

(deftest turntable-vs-mandatory-upgrades
    ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
    ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
    ;; vs Mandatory Upgrades
    (do-game
      (new-game {:corp {:deck [(qty "Mandatory Upgrades" 2) "Project Vitruvius"]}
                 :runner {:deck ["Turntable"]}})
      (score-agenda state :corp (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (let [tt (get-hardware state 0)]
        ;; steal Project Vitruvius and swap for Mandatory Upgrades
        (core/steal state :runner (core/make-eid state) (find-card "Project Vitruvius" (:hand (get-corp))))
        (is (prompt-is-card? state :runner tt))
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Mandatory Upgrades" (:scored (get-corp))))
        (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn")
        ;; steal second Mandatory Upgrades and swap for Project Vitruvius
        (core/steal state :runner (core/make-eid state) (find-card "Mandatory Upgrades" (:hand (get-corp))))
        (is (prompt-is-card? state :runner tt))
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Project Vitruvius" (:scored (get-corp))))
        (is (zero? (:click (get-corp))) "Corp doesn't gain a click on Runner's turn")
        (is (= 4 (:click-per-turn (get-corp)))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Corp HQ is filled to max hand size
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)]}
               :runner {:deck ["Vigil" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Vigil")
    (is (= 5 (core/available-mu state)))
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (is (empty? (:hand (get-runner))))
    (take-credits state :runner)
    (is (= (count (:hand (get-corp))) (hand-size :corp)) "Corp hand filled to max")
    (take-credits state :corp)
    (is (= 1 (count (:hand (get-runner)))) "Drew 1 card")
    (take-credits state :runner)
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (is (not= (count (:hand (get-corp))) (hand-size :corp)) "Corp hand below max")
    (is (= 1 (count (:hand (get-runner)))) "No card drawn")))

(deftest virtuoso
  ;; Virtuoso - mark start of turn, +1 mu, hq mark -> +1, otherwise breach hq after run
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"] :hand ["IPO" "IPO"]}
               :runner {:hand ["Virtuoso"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Virtuoso")
    (is (= 5 (core/available-mu state)))
    (is (nil? (:mark @state)) "No mark identified")
    ;; breach +1 when hq marked
    (core/set-mark state :hq)
    (run-on state :hq)
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (no-prompt? state :runner))
    ;; only works once/turn
    (run-on state :hq)
    (run-continue state)
    (click-prompt state :runner "No action")
    (is (no-prompt? state :runner)))
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"] :hand ["Rashida Jaheem"]}
               :runner {:hand ["Virtuoso"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Virtuoso")
    (is (nil? (:mark @state)) "No mark identified")
    ;; when marj rd/archives, breach hq after run ends
    (core/set-mark state :rd)
    (run-on state :rd)
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Pay 1 [Credits] to trash")
    (is (no-prompt? state :runner))))

(deftest wake-implant-v2a-jrj
  ;; WAKE Implant v2A-JRJ
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                      :hand ["Hedge Fund"]}
               :runner {:hand ["WAKE Implant v2A-JRJ" "Sure Gamble"]}})
    (take-credits state :corp)
    (core/gain state :runner :click 6)
    (play-from-hand state :runner "WAKE Implant v2A-JRJ")
    (is (= 1 (count (:discard (get-runner)))) "1 damage done")
    (let [wi (get-hardware state 0)]
      (dotimes [_ 6]
        (is (changed? [(get-counters (refresh wi) :power) 1]
              (run-empty-server state :hq)
              (click-prompt state :runner "No action"))
            "1 counter added"))
      (dotimes [c 3]
        (is (changed? [(get-counters (refresh wi) :power) (- c)]
              (run-empty-server state :rd)
              (click-prompt state :runner (str c))
              (is (= c (core/access-bonus-count state :runner :rd)) (str "Runner should access " c " additional cards"))
              (click-prompt state :runner "No action")
              (dotimes [_ c]
            (click-prompt state :runner "No action")))
            (str c " counters spent"))))))

(deftest zamba
  ;; Zamba - Whenever corp card is exposed you may gain 1 credit
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Zamba" (qty "Infiltration" 2)]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Zamba")
    (is (= 6 (core/available-mu state)) "Gain 2 memory")
    (is (= 1 (:credit (get-runner))) "At 1 credit")
    (play-from-hand state :runner "Infiltration")
    (click-prompt state :runner "Expose a card")
    (click-card state :runner (get-ice state :archives 0))
    (is (= 2 (:credit (get-runner))) "Gained 1 credit from exposing")
    (play-from-hand state :runner "Infiltration")
    (click-prompt state :runner "Expose a card")
    (click-card state :runner (get-ice state :archives 0))
    (is (= 3 (:credit (get-runner))) "Gained 1 more credit from exposing")))

(deftest zer0-basic-ability
    ;; Basic ability
    (do-game
      (new-game {:runner {:deck ["Zer0" "Corroder" (qty "Sure Gamble" 2)]}})
      (starting-hand state :runner ["Zer0" "Corroder"])
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
        (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards")
        (is (find-card "Corroder" (:discard (get-runner))) "Corroder is in heap"))))

(deftest zer0-with-titanium-ribs
    ;; With Titanium Ribs
    (do-game
      (new-game {:runner {:deck ["Zer0" "Titanium Ribs" (qty "Sure Gamble" 5)]}})
      (starting-hand state :runner ["Zer0" "Titanium Ribs" "Sure Gamble" "Sure Gamble" "Sure Gamble"])
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner (first (:hand (get-runner))))
      (click-card state :runner (second (:hand (get-runner))))
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 3 (:credit (get-runner))) "Zer0 has not yet resolved because Ribs prompt is open")
        (is (= 1 (count (:hand (get-runner)))) "Zer0 has not yet resolved because Ribs prompt is open")
        (click-card state :runner (first (:hand (get-runner))))
        (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
        (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards"))))

(deftest zer0-with-respirocytes
    ;; With Respirocytes
    (do-game
      (new-game {:runner {:deck [(qty "Clone Chip" 5)]
                          :hand ["Zer0" "Titanium Ribs" "Respirocytes"
                                 "Sure Gamble" "Easy Mark" "Zamba" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner "Sure Gamble")
      (click-card state :runner "Easy Mark")
      (play-from-hand state :runner "Respirocytes")
      (click-card state :runner "Zamba")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 3 (:credit (get-runner))) "Zer0 has not yet resolved because Ribs prompt is open")
        (is (= 1 (count (:hand (get-runner)))) "Zer0 has not yet resolved because Ribs prompt is open")
        (click-card state :runner "Corroder")
        (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
        (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards"))))
