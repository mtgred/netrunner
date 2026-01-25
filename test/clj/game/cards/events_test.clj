(ns game.cards.events-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.eid :refer :all]
   [game.test-framework :refer :all]
   [game.utils :as utils]
   [jinteki.validator :refer [legal?]]))

(deftest account-siphon-use-ability
    ;; Use ability
    (do-game
      (new-game {:runner {:deck [(qty "Account Siphon" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Account Siphon, use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Account Siphon")
      (is (= 2 (count-tags state)) "Runner took 2 tags")
      (is (= 15 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))

(deftest account-siphon-access
    ;; Access
    (do-game
      (new-game {:runner {:deck [(qty "Account Siphon" 3)]}})
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play another Siphon, do not use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Breach HQ")
      (is (zero? (count-tags state)) "Runner did not take any tags")
      (is (= 5 (:credit (get-runner))) "Runner did not gain any credits")
      (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")))

(deftest account-siphon-new-angeles-city-hall-interaction
    ;; New Angeles City Hall interaction
    (do-game
      (new-game {:runner {:deck ["Account Siphon"
                                 "New Angeles City Hall"]}})
      (core/gain state :corp :bad-publicity 1)
      (is (= 1 (count-bad-pub state)) "Corp has 1 bad publicity")
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      (play-from-hand state :runner "New Angeles City Hall")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [nach (get-resource state 0)]
        (play-run-event state "Account Siphon" :hq)
        (click-prompt state :runner "Account Siphon")
        (is (= 4 (:credit (get-runner))) "Runner still has 4 credits due to BP")
        (click-prompt state :runner "New Angeles City Hall")
        (click-prompt state :runner "Yes")
        (is (= 2 (:credit (get-runner))) "Runner has 2 credits left")
        (click-prompt state :runner "Yes"))
      (is (zero? (count-tags state)) "Runner did not take any tags")
      (is (= 10 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))

(deftest always-have-a-backup-plan-jacking-out-correctly-triggers-ahbp
    ;; Jacking out correctly triggers AHBP
    (do-game
      (new-game {:runner {:deck ["Always Have a Backup Plan"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-from-hand state :runner "Always Have a Backup Plan")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (is (= [:archives] (:server (:run @state))) "Running on Archives")
      (run-jack-out state)
      (is (not (:run @state)) "Run ended")
      (click-prompt state :runner "Yes")
      (is (= [:archives] (:server (:run @state))) "Running on Archives again")))

(deftest always-have-a-backup-plan-corp-ending-the-run-correctly-triggers-ahbp
    ;; Corp ending the run correctly triggers AHBP
    (do-game
      (new-game {:runner {:deck ["Always Have a Backup Plan"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-from-hand state :runner "Always Have a Backup Plan")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (is (= [:archives] (:server (:run @state))) "Running on Archives")
      (run-jack-out state)
      (is (not (:run @state)) "Run ended")
      (click-prompt state :runner "Yes")
      (is (= [:archives] (:server (:run @state))) "Running on Archives again")))

(deftest always-have-a-backup-plan-extra-costs-are-paid-only-on-the-first-run
    ;; Extra costs are paid only on the first run
    (do-game
      (new-game {:runner {:deck ["Always Have a Backup Plan"]}
                 :corp {:deck ["Cold Site Server"]}})
      (play-from-hand state :corp "Cold Site Server" "Archives")
      (rez state :corp (get-content state :archives 0))
      (card-ability state :corp (get-content state :archives 0) 0)
      (is (= 1 (get-counters (get-content state :archives 0) :power)))
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-from-hand state :runner "Always Have a Backup Plan")
      (is (= 3 (:credit (get-runner))))
      (is (= 3 (:click (get-runner))))
      (click-prompt state :runner "Archives")
      (run-continue state)
      (is (= [:archives] (:server (:run @state))) "Running on Archives")
      (is (= 2 (:credit (get-runner))) "Initiating run cost 1 click, 1 cred + event play cost")
      (is (= 2 (:click (get-runner))) "Initiating run cost 1 click, 1 cred + event play cost")
      (run-jack-out state)
      (is (not (:run @state)) "Run ended")
      (click-prompt state :runner "Yes")
      (is (= [:archives] (:server (:run @state))) "Running on Archives again")
      (is (= 2 (:click (get-runner))) "Initiating 2nd run free")
      (is (= 2 (:credit (get-runner))) "Initiating 2nd run free")))

(deftest always-have-a-backup-plan-bypass-last-encountered-ice-etr-ice
  ;; Always Have A Backup Plan - Bypass Last Piece of Ice Encountered - Ice ETR
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Ice Wall" "Wall of Static"]
                     :credits 10}
              :runner {:hand ["Always Have a Backup Plan"]}})
   (play-from-hand state :corp "Ice Wall" "New remote")
   (play-from-hand state :corp "Wall of Static" "Server 1")
   (take-credits state :corp)
   (let [iw (get-ice state :remote1 0)
         ws (get-ice state :remote1 1)]
     (rez state :corp iw)
     (rez state :corp ws)
     (play-from-hand state :runner "Always Have a Backup Plan")
     (click-prompt state :runner "Server 1")
     (run-continue state)
     (run-continue state :encounter-ice)
     (is (utils/same-card? (refresh ws) (core/get-current-ice state)) "Encountering Wall of Static")
     (fire-subs state (refresh ws))
     (is (nil? (:run @state)) "Run has ended")
     (click-prompt state :runner "Yes")
     (run-continue state)
     (run-continue state :movement);; bypass Wall of Static
     (run-continue state :approach-ice)
     (run-continue state :encounter-ice)
     (is (utils/same-card? (refresh iw) (core/get-current-ice state)) "Did not bypass Ice Wall")
     (fire-subs state (refresh iw))
     (is (nil? (:run @state)) "Run has ended")
     (is (no-prompt? state :runner) "Not prompted to run again"))))

(deftest always-have-a-backup-plan-bypass-last-encountered-ice-etr-not-ice
  ;; Always Have A Backup Plan - Bypass Last Piece of Ice Encountered - Non-ICE ETR
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand [(qty "Hedge Fund" 2) "Ice Wall" "Wall of Static" "Anoetic Void"]
                     :credits 10}
              :runner {:hand ["Always Have a Backup Plan"]}})
   (play-from-hand state :corp "Anoetic Void" "New remote")
   (play-from-hand state :corp "Ice Wall" "Server 1")
   (play-from-hand state :corp "Wall of Static" "Server 1")
   (take-credits state :corp)
   (let [iw (get-ice state :remote1 0)
         ws (get-ice state :remote1 1)]
     (rez state :corp iw)
     (rez state :corp ws)
     (rez state :corp (get-content state :remote1 0))
     (play-from-hand state :runner "Always Have a Backup Plan")
     (click-prompt state :runner "Server 1")
     (run-continue state)
     (run-continue state :encounter-ice)
     (is (utils/same-card? (refresh ws) (core/get-current-ice state)) "Encountering Wall of Static")
     (run-continue-until state :encounter-ice)
     (is (utils/same-card? (refresh iw) (core/get-current-ice state)) "Encountering Ice Wall")
     (run-continue-until state :success)
     (click-prompt state :corp "Yes")
     (click-card state :corp (first (:hand (get-corp))))
     (click-card state :corp (second (:hand (get-corp))))
     (is (nil? (:run @state)) "Run has ended")
     (click-prompt state :runner "Yes")
     (run-continue state)
     (run-continue state :encounter-ice)
     (is (utils/same-card? (refresh ws) (core/get-current-ice state)) "Did not bypass Wall of Static")
     (run-continue-until state :approach-ice)
     (run-continue state :movement);; bypass Ice Wall
     (run-jack-out state)
     (is (nil? (:run @state)) "Run has ended")
     (is (no-prompt? state :runner) "Not prompted to run again"))))

(deftest always-have-a-backup-plan-run-redirected-second-run-on-redirected-server
  ;; Always Have A Backup Plan - Run redirected - Second run is on the attacked server
  (do-game
   (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                     :hand ["Bullfrog"]}
              :runner {:hand ["Always Have a Backup Plan"]}})
   (play-from-hand state :corp "Bullfrog" "HQ")
   (take-credits state :corp)
   (let [bullfrog (get-ice state :hq 0)]
     (rez state :corp bullfrog)
     (play-from-hand state :runner "Always Have a Backup Plan")
     (click-prompt state :runner "HQ")
     (run-continue state)
     (run-continue state :encounter-ice)
     (is (utils/same-card? (refresh bullfrog) (core/get-current-ice state)) "Encountering Bullfrog")
     (fire-subs state (refresh bullfrog))
     (click-prompt state :corp "0 [Credits]")
     (click-prompt state :runner "1 [Credits]")
     (click-prompt state :corp "Archives")
     (run-continue state :movement)
     (run-jack-out state)
     (is (nil? (:run @state)) "Run has ended")
     (click-prompt state :runner "Yes")
     (is (= :archives (first (:server (:run @state)))) "New Run on Archives and not HQ")
     (run-continue state)
     (run-continue state :movement);; bypass Bullfrog
     (run-jack-out state)
     (is (nil? (:run @state)) "Run has ended")
     (is (no-prompt? state :runner) "Not prompted to run again"))))

(deftest amped-up
  ;; Amped Up - Gain 3 clicks and take 1 unpreventable brain damage
  (do-game
    (new-game {:runner {:deck ["Amped Up"
                               "Feedback Filter"
                               (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Amped Up")
    (is (no-prompt? state :runner)
        "Feedback Filter core damage prevention opportunity not given")
    (is (= 5 (:click (get-runner))) "Runner gained 2 clicks from Amped Up")
    (is (= 2 (count (:discard (get-runner)))) "Runner discarded 1 card from damage")
    (is (= 4 (hand-size :runner)) "Runner handsize decreased by 1")
    (is (= 1 (:brain-damage (get-runner))) "Took 1 core damage")))

(deftest another-day-another-paycheck
  ;; Another Day, Another Paycheck
  (do-game
    (new-game {:corp {:deck [(qty "Project Atlas" 3)]}
               :runner {:deck ["Street Peddler" (qty "Another Day, Another Paycheck" 2)]}})
    (starting-hand state :runner ["Street Peddler" "Another Day, Another Paycheck"])
    (play-from-hand state :corp "Project Atlas" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (is (= 5 (:credit (get-runner))) "No trace, no gain")
    (play-from-hand state :runner "Another Day, Another Paycheck")
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "0")
    (click-prompt state :runner "1")
    ;; 4 credits after trace, gain 6
    (is (= 10 (:credit (get-runner))) "Runner gained 6 credits")))

(deftest apocalypse-ensure-mu-is-correct-and-no-duplicate-cards-in-heap
    ;; Ensure MU is correct and no duplicate cards in heap
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
                 :runner {:deck ["Scheherazade" "Corroder" "Hivemind" (qty "Apocalypse" 2)]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Scheherazade")
      (play-from-hand state :runner "Corroder")
      (click-prompt state :runner "Scheherazade")
      (play-from-hand state :runner "Hivemind")
      (click-prompt state :runner "The Rig")
      (is (= 1 (core/available-mu state)) "Memory at 1 (-1 from Corroder, -2 from Hivemind)")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
      (is (zero? (count (core/all-active-installed state :runner))) "No active installed runner cards")
      (let [facedowns (filter :facedown (core/all-installed state :runner))
            scheherazade (find-card "Scheherazade" facedowns)
            corroder (find-card "Corroder" facedowns)
            hivemind (find-card "Hivemind" facedowns)]
        (is scheherazade "Scheherazade facedown")
        (is corroder "Corroder facedown")
        (is hivemind "Hivemind facedown")
        (is (= 3 (count facedowns)) "No other cards facedown")
        (is (= corroder (first (:hosted scheherazade))) "Corroder is still hosted on Scheherazade")
        (is (= 1 (get-counters hivemind :virus)) "Hivemind still has a virus counters"))
      (is (find-card "Apocalypse" (:discard (get-runner))) "Apocalypse is in the heap")
      (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap")
      (is (= 4 (core/available-mu state)) "Memory back to 4")))

(deftest apocalypse-with-full-immersion-no-duplicate-cards-in-heap-2606
    ;; with Full Immersion - no duplicate cards in heap #2606
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio" "Sandburg"
                               "Oaktown Renovation"]}
                 :runner {:deck ["Apocalypse"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (rez state :corp fir)
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Sandburg" (:hand (get-corp))))
        (card-ability state :corp fir 0)
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (take-credits state :corp)
        (run-empty-server state "Archives")
        (run-empty-server state "R&D")
        (run-empty-server state "HQ")
        (play-from-hand state :runner "Apocalypse")
        (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
        (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
        (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap"))))

(deftest apocalypse-with-hostile-infrastructure-should-take-damage-equal-to-2x-cards-on-the-table
    ;; with Hostile Infrastructure - should take damage equal to 2x cards on the table
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 2) (qty "Ice Wall" 2)]
                        :credits 100}
                 :runner {:deck ["Apocalypse" (qty "Sure Gamble" 9)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (rez state :corp (get-content state :remote4 0))
      (take-credits state :corp)
      (draw state :runner 5)
      (is (= 10 (count (:hand (get-runner)))) "Runner has 9 cards in hand")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 4 (count (:discard (get-corp)))) "4 Corp cards in Archives")
      (is (= 1 (count (:hand (get-runner)))) "Runner has one card in hand")
      (is (= 9 (count (:discard (get-runner)))) "There are 9 cards in heap")))

(deftest apocalypse-turn-runner-cards-facedown-and-reduce-memory-and-hand-size-gains
    ;; Turn Runner cards facedown and reduce memory and hand-size gains
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
                 :runner {:deck ["Logos" "Apocalypse" (qty "Origami" 2)]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Logos")
      (is (= 6 (hand-size :runner)) "Hand-size increased from Logos")
      (is (= 5 (core/available-mu state)) "Memory increased from Logos")
      (play-from-hand state :runner "Origami")
      (play-from-hand state :runner "Origami")
      (is (= 10 (hand-size :runner)) "Hand-size increased from Logos and Origami")
      (is (= 3 (core/available-mu state)) "Memory decreased from Origamis")
      (core/gain state :runner :click 3 :credit 2)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
      (let [logos (find-card "Logos" (get-in (get-runner) [:rig :facedown]))]
        (is (:facedown (refresh logos)) "Logos is facedown")
        (is (= 5 (hand-size :runner)) "Hand-size reset with Logos and Origami facedown")
        (is (= 4 (core/available-mu state)) "Memory reset with Logos and Origami facedown"))))

(deftest apocalypse-turn-runner-cards-facedown-without-firing-their-trash-effects
    ;; Turn Runner cards facedown without firing their trash effects
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
                 :runner {:deck [(qty "Tri-maf Contact" 3) (qty "Apocalypse" 3)]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Tri-maf Contact")
      (core/gain state :runner :click 2)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
      (let [tmc (get-runner-facedown state 0)]
        (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
        (is (= 3 (count (:hand (get-runner))))
            "No meat damage dealt by Tri-maf's leave play effect")
        (trash state :runner tmc)
        (is (= 3 (count (:hand (get-runner))))
            "No meat damage dealt by trashing facedown Tri-maf"))))

(deftest apocalypse-interaction-with-calvin-b4l3y-and-jinja-city-grid-issue-4201
    ;; Interaction with Calvin B4L3Y and Jinja City Grid. Issue #4201
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 5)]
                        :hand ["Calvin B4L3Y" "Jinja City Grid"]
                        :credits 10}
                 :runner {:hand ["Apocalypse"]}})
      (play-from-hand state :corp "Jinja City Grid" "New remote")
      (play-from-hand state :corp "Calvin B4L3Y" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (rez state :corp (get-content state :remote2 0))
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (run-empty-server state "HQ")
      (let [hand (count (:hand (get-corp)))]
        (play-from-hand state :runner "Apocalypse")
        (click-prompt state :corp "Yes")
        (is (= (+ 2 hand) (count (:hand (get-corp)))) "Calvin Baley draws 2 cards")
        (is (no-prompt? state :corp) "No Jinja City Grid"))))

(deftest ashen-epilogue
  (do-game
    (new-game {:runner {:hand ["Ashen Epilogue" (qty "Sure Gamble" 5)]
                        :deck [(qty "Sure Gamble" 20)]
                        :discard [(qty "Sure Gamble" 20)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Ashen Epilogue")
    (is (= 0 (count (:discard (get-runner)))) "Heap is empty")
    (is (= 5 (count (:hand (get-runner)))) "Grip is full")
    (is (= 35 (count (:deck (get-runner)))) "Stack is back")
    (is (= 6 (count (:rfg (get-runner)))))))

(deftest bahia-bands
  (do-game
      (new-game {:corp {:hand ["PAD Campaign"]}
                 :runner {:hand [(qty "Bahia Bands" 2)]
                          :deck ["Sure Gamble" "Simulchip"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Bahia Bands")
      (click-prompt state :runner "Server 1")
      (run-continue state)
      (run-continue state)
      (is (changed? [(count (:hand (get-runner))) 2]
            (click-prompt state :runner "Draw 2 cards"))
          "Drew 2 cards")
      (is (changed? [(:credit (get-runner)) 0]
            (click-prompt state :runner "Install a card from the grip, paying 1 [Credits] less")
            (click-card state :runner "Simulchip"))
          "Spent no credits to install Simulchip")
      (is (= 1 (count (get-hardware state))) "Installed Simulchip")
      (click-prompt state :runner "No action")
      ;; Second run
      (gain-tags state :runner 1)
      (play-from-hand state :runner "Bahia Bands")
      (click-prompt state :runner "Server 1")
      (run-continue state)
      (run-continue state)
      (is (changed? [(count-tags state) -1]
            (click-prompt state :runner "Remove 1 tag"))
          "Removed 1 tag")
      (click-prompt state :runner "Place 4 [Credits] for paying trash costs")
      (let [bb (-> (get-runner) :play-area first)]
        (is (= 4 (get-counters (refresh bb) :credit)) "Bahia Bands has 4 credits on it")
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (dotimes [_ 4]
          (click-card state :runner bb))
        (is (= 1 (count (:discard (get-corp))))))))

(deftest because-i-can
  ;; Because I Can
  ;; make a successful run on a remote to shuffle its contents into R&D
  (do-game
      (new-game {:corp {:hand ["Project Atlas" (qty "Shell Corporation" 2)]}
                 :runner {:hand ["Because I Can"]}})
      (play-from-hand state :corp "Shell Corporation" "New remote")
      (play-from-hand state :corp "Shell Corporation" "Server 1")
      (play-from-hand state :corp "Project Atlas" "Server 1")
      (take-credits state :corp)
      (let [n (count (get-in @state [:corp :deck]))]
        (play-from-hand state :runner "Because I Can")
        (click-prompt state :runner "Server 1")
        (run-continue state)
        (is (= 3 (count (get-in @state [:corp :servers :remote1 :content])))
            "3 cards in server 1 before successful run")
        (run-continue state)
        (click-prompt state :runner "Because I Can")
        (is (= (+ n 3) (count (get-in @state [:corp :deck]))) "3 cards were shuffled into R&D")
        (is (zero? (count (get-in @state [:corp :servers :remote1 :content]))) "No cards left in server 1"))))

(deftest because-i-can-when-redirected-to-another-server
    ;; When redirected to another server
    (do-game
      (new-game {:corp {:hand ["Sand Storm" "PAD Campaign"]}
                 :runner {:hand ["Because I Can"]}})
      (play-from-hand state :corp "Sand Storm" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (let [n (count (get-in @state [:corp :deck]))
            sand-storm (get-ice state :remote1 0)]
        (play-from-hand state :runner "Because I Can")
        (click-prompt state :runner "Server 1")
        (run-continue state)
        (rez state :corp sand-storm)
        (is (= :remote1 (first (get-in @state [:run :server]))))
        (run-continue state)
        (card-subroutine state :corp sand-storm 0)
        (click-prompt state :corp "Server 2")
        (is (= :remote2 (first (get-in @state [:run :server]))))
        (is (= 1 (count (get-in @state [:corp :servers :remote2 :content]))) "1 card in server 3 before successful run")
        (run-continue state)
        (click-prompt state :runner "Because I Can")
        (is (= (inc n) (count (get-in @state [:corp :deck]))) "1 card was shuffled into R&D")
        (is (zero? (count (get-in @state [:corp :servers :remote2 :content]))) "No cards left in server 3"))))

(deftest black-hat
  ;; Black Hat
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                        :hand ["Hedge Fund"]}
                 :runner {:deck [(qty "Black Hat" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Black Hat")
      (click-prompt state :corp "0")
      (click-prompt state :runner "4")
      (run-empty-server state :rd)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "No action")))

(deftest black-hat-kitsune-interaction
    ;; Kitsune interaction
    (do-game
      (new-game {:corp {:deck [(qty "Kitsune" 10)]}
                 :runner {:deck [(qty "Black Hat" 3)]
                          :credits 15}})
      (starting-hand state :corp ["Kitsune" "Kitsune" "Kitsune" "Kitsune" "Kitsune"])
      (play-from-hand state :corp "Kitsune" "R&D")
      (let [kitsune (get-ice state :rd 0)]
        (rez state :corp kitsune)
        (take-credits state :corp)
        (play-from-hand state :runner "Black Hat")
        (click-prompt state  :corp "0")
        (click-prompt state :runner "4")
        (run-on state :rd)
        (run-continue state)
        (card-subroutine state :corp kitsune 0)
        (click-prompt state :corp "Yes")
        (click-card state :corp (find-card "Kitsune" (:hand (get-corp))))
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action"))))

(deftest blackmail
  ;; Prevent rezzing of ice for one run
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]
                        :bad-pub 1}
                 :runner {:deck [(qty "Blackmail" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Blackmail")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (let [iwall1 (get-ice state :hq 0)
            iwall2 (get-ice state :hq 1)]
        (rez state :corp iwall1 {:expect-rez false})
        (is (not (rezzed? (refresh iwall1))) "First Ice Wall is not rezzed")
        (run-continue state)
        (rez state :corp iwall2 {:expect-rez false})
        (is (not (rezzed? (refresh iwall2))) "Second Ice Wall is not rezzed")
        (run-jack-out state)
        ;; Do another run, where the ice should rez
        (run-on state "HQ")
        (rez state :corp iwall1)
        (is (rezzed? (refresh iwall1)) "First Ice Wall is rezzed"))))

(deftest blackmail-regression-test-for-a-rezzed-tmi-breaking-game-state-on-a-blackmail-run
    ;; Regression test for a rezzed tmi breaking game state on a blackmail run
    (do-game
      (new-game {:corp {:deck [(qty "TMI" 3)]
                        :bad-pub 1}
                 :runner {:deck [(qty "Blackmail" 3)]}})
      (play-from-hand state :corp "TMI" "HQ")
      (let [tmi (get-ice state :hq 0)]
        (rez state :corp tmi)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (rezzed? (refresh tmi)) "TMI is rezzed")
        (take-credits state :corp)
        (play-from-hand state :runner "Blackmail")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (run-continue-until state :movement)
        (run-jack-out state)
        (run-on state "Archives"))))

(deftest blueberry-diesel-selecting-a-card
    ;; Selecting a card
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:hand ["Blueberry!™ Diesel" "Sure Gamble" "Easy Mark" "Daily Casts"]}})
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Daily Casts" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "Blueberry!™ Diesel")
      (is (= "Daily Casts" (-> (get-runner) :deck last :title)))
      (click-prompt state :runner "Sure Gamble")
      (is (find-card "Daily Casts" (:hand (get-runner))))))

(deftest blueberry-diesel-selecting-no-card
    ;; Selecting no card
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:hand ["Blueberry!™ Diesel" "Sure Gamble" "Easy Mark" "Daily Casts"]}})
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Daily Casts" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "Blueberry!™ Diesel")
      (is (= "Daily Casts" (-> (get-runner) :deck last :title)))
      (is (nil? (seq (:hand (get-runner)))))
      (click-prompt state :runner "No")
      (is (= ["Sure Gamble" "Easy Mark"] (mapv :title (:hand (get-runner)))))
      (is (not (find-card "Daily Casts" (:hand (get-runner)))))))

(deftest bravado
  ;; Bravado
  (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Bravado"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Bravado")
      (is (= 1 (count (prompt-buttons :runner))) "Only HQ is runnable")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) 7]
            (run-continue state))
          "Gained 6+1 credits from Bravado")))

(deftest bravado-gaining-money-based-on-distinct-pieces-of-ice
    ;; Gaining money based on distinct pieces of ice
    (do-game
      (new-game {:corp {:hand [(qty "Ice Wall" 2) "Cell Portal"]
                        :credits 8}
                 :runner {:hand ["Bravado"]}})
      (play-from-hand state :corp "Cell Portal" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (let [cp (get-ice state :hq 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Bravado")
        (click-prompt state :runner "HQ")
        (run-continue-until state :approach-ice cp)
        (rez state :corp cp)
        (run-continue state)
        (card-subroutine state :corp (refresh cp) 0)
        (click-prompt state :runner "No")
        (is (changed? [(:credit (get-runner)) 9]
              (run-continue-until state :success))
            "Gained 6+3 credits from Bravado"))))

(deftest bravado-reinstalled-ice-during-a-run-is-counted-twice
    ;; Reinstalled ice during a run is counted twice
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 10)]
                        :hand ["Cell Portal" "Architect" "Enigma"]
                        :credits 15}
                 :runner {:hand ["Bravado"]}})
      (play-from-hand state :corp "Cell Portal" "HQ")
      (play-from-hand state :corp "Architect" "HQ")
      (play-from-hand state :corp "Enigma" "HQ")
      (let [cp (get-ice state :hq 0)
            arch (get-ice state :hq 1)
            enig (get-ice state :hq 2)]
        (take-credits state :corp)
        (play-from-hand state :runner "Bravado")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (run-continue-until state :approach-ice)
        ; Overinstall Enigma
        (rez state :corp arch)
        (run-continue state)
        (card-subroutine state :corp (refresh arch) 0)
        (click-prompt state :corp "OK")
        (click-prompt state :corp "Ice Wall")
        (core/move state :corp enig :discard)
        (click-prompt state :corp "HQ")
        ; Reinstall Enigma
        (card-subroutine state :corp (refresh arch) 1)
        (click-card state :corp (find-card "Enigma" (:discard (get-corp))))
        (click-prompt state :corp "HQ")
        (run-continue-until state :approach-ice)
        (rez state :corp cp)
        (run-continue state)
        (card-subroutine state :corp (refresh cp) 0)
        (click-prompt state :runner "No")
        ;; Enigma
        (run-continue-until state :movement)
        ;; Ice Wall
        (run-continue-until state :movement)
        ;; Architect
        (run-continue-until state :movement)
        ;; Cell Portal
        (run-continue-until state :movement)
        ;; Approach Server
        (is (changed? [(:credit (get-runner)) 11]
              ; Cell Portal, Architect, Ice Wall, 2x Enigma
              (run-continue state))
            "Gained 6+5 credits from Bravado"))))

(deftest bravado-trashed-ice-using-hippo-is-not-counted
    ;; Trashed ice using Hippo is not counted
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]
                        :credits 15}
                 :runner {:hand ["Bravado", "Corroder", "Hippo"]
                          :credits 15}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Hippo")
      (let [ice-wall (get-ice state :hq 0)
            corroder (get-program state 0)]
        (play-from-hand state :runner "Bravado")
        (click-prompt state :runner "HQ")
        (run-continue state)
        ;; Rez Ice Wall
        (rez state :corp ice-wall)
        (run-continue state)
        ;; Fully break Ice Wall
        (card-ability state :runner corroder 0)
        (click-prompt state :runner "End the run")
        ;; Trash using Hippo
        (click-prompt state :runner "Yes")
        (run-continue state)
        ;; Approach Server
        (is (changed? [(:credit (get-runner)) 6]
              ;; Trashed Ice Wall does not count
              (run-continue state))
            "Gained 6 credits from Bravado"))))

(deftest bravado-self-trashing-ice-is-not-counted
    ;; Self-trashing ice (e.g. traps) is not counted
    (do-game
      (new-game {:corp {:hand ["Aimor"]}
                 :runner {:hand ["Bravado"]}})
      (play-from-hand state :corp "Aimor" "HQ")
      (take-credits state :corp)
      (let [aimor (get-ice state :hq 0)]
        (play-from-hand state :runner "Bravado")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (rez state :corp aimor)
        (run-continue state)
        (fire-subs state (refresh aimor))
        (is (find-card "Aimor" (:discard (get-corp))) "Aimor was trashed")
        (is (changed? [(:credit (get-runner)) 6]
              ;; Trashed Aimor does not count
              (run-continue state))
            "Gained 6 credits from Bravado"))))

(deftest bravado-also-gaining-credits-on-unsuccessful-runs
    ;; Also gaining credits on unsuccessful runs
    (do-game
      (new-game {:corp {:hand ["Ice Wall"]}
                 :runner {:hand ["Bravado"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Bravado")
      (is (= 1 (count (prompt-buttons :runner))) "Only HQ is runnable")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (let [icew (get-ice state :hq 0)]
        (rez state :corp icew)
        (run-continue state)
        (is (changed? [(:credit (get-runner)) 6]
              (card-subroutine state :corp (refresh icew) 0))
            "Gained 6 credits from Bravado")
        (is (not (:run @state)) "Run has ended"))))

(deftest bravado-only-show-runnable-servers-5034
    ;; Only show runnable servers. #5034
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]
                        :hand [(qty "Ice Wall" 2)]}
                 :runner {:hand [(qty "Bravado" 2)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (play-from-hand state :runner "Bravado")
      (is (= ["HQ"] (prompt-buttons :runner)) "Only the iced central server is available")
      (click-prompt state :runner "HQ")
      (run-continue-until state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Bravado")
      (is (= ["HQ" "Server 1"] (prompt-buttons :runner)) "Now Server 1 is available")))

(deftest bribery
  ;; Bribery
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand [(qty "Ice Wall" 2)]}
               :runner {:hand ["Bribery"]
                        :credits 100}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Bribery")
    (click-prompt state :runner "100")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (let [iw1 (get-ice state :hq 1)
          iw2 (get-ice state :hq 0)]
      (rez state :corp iw1 {:expect-rez false})
      (is (not (rezzed? (refresh iw1))) "Foremost Ice Wall is not rezzed")
      (rez state :corp iw2)
      (is (rezzed? (refresh iw2)) "Final Ice Wall is rezzed"))))

(deftest brute-force-hack
  ;; Brute-Force-Hack
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                      :hand ["Ice Wall" "Tollbooth"]
                      :credits 10}
               :runner {:hand [(qty "Brute-Force-Hack" 2) "Xanadu"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Tollbooth" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (let [iw (get-ice state :hq 0)
          tb (get-ice state :hq 1)]
      (rez state :corp iw)
      (rez state :corp tb)
      (play-from-hand state :runner "Brute-Force-Hack")
      (click-prompt state :runner "1")
      (click-card state :runner "Tollbooth")
      (is (rezzed? (refresh tb)) "Runner doesn't have enough money to derez Tollbooth")
      (click-card state :runner iw)
      (is (not (rezzed? (refresh iw))) "Runner can derez Ice Wall"))))

(deftest brute-force-hack-interacts-with-donut
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:credits 30
                        :hand [(qty "Brute-Force-Hack" 2) "Donut Taganes"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (let [iw (get-ice state :hq 0)]
      (rez state :corp iw)
      (play-from-hand state :runner "Donut Taganes")
      (play-from-hand state :runner "Brute-Force-Hack")
      (click-prompt state :runner "1")
      (is (no-prompt? state :runner) "X = 0, cannot pick ice wall")
      (play-from-hand state :runner "Brute-Force-Hack")
      (click-prompt state :runner "2")
      (click-card state :runner iw)
      (is (not (rezzed? (refresh iw))) "Runner can derez Ice Wall"))))

(deftest build-script
  ;; Build Script
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Build Script"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))
          hand (dec (count (:hand (get-runner))))]
      (play-from-hand state :runner "Build Script")
      (is (= (inc credits) (:credit (get-runner))) "Gained 1 credit")
      (is (= (+ 2 hand) (count (:hand (get-runner)))) "Drew 2 cards"))))

(deftest burner
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 4)]}
               :runner {:hand ["Burner"]}})
    (take-credits state :corp)
    (play-run-event state "Burner" :hq)
    (is (last-log-contains? state "reveal Hedge Fund, Hedge Fund, and Hedge Fund from HQ"))
    (is (changed? [(count (:deck (get-corp))) 2
                   (count (:hand (get-corp))) -2]
                  (click-prompt state :runner "Hedge Fund")
                  (click-prompt state :runner "Top of R&D")
                  (click-prompt state :runner "Hedge Fund")
                  (click-prompt state :runner "Bottom of R&D"))
        "2 cards added to R&D")))

(deftest by-any-means-full-test
    ;; Full test
    (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Ice Wall" "Paper Trail" "PAD Campaign"
                               "Project Junebug"]}
                 :runner {:deck ["Sure Gamble"]
                          :hand ["By Any Means" (qty "Sure Gamble" 4)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (play-from-hand state :runner "By Any Means")
      (is (= 3 (:click (get-runner))) "Card not played, priority restriction")
      (take-credits state :runner)
      (starting-hand state :corp ["Paper Trail" "Hedge Fund" "PAD Campaign" "Project Junebug"])
      (play-from-hand state :corp "Paper Trail" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Project Junebug" "New remote")
      (core/add-counter state :corp (core/make-eid state) (get-content state :remote3 0) :advancement 2)
      (take-credits state :corp)
      (core/gain state :runner :click 3)
      (draw state :runner)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
      (is (= 4 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "R&D")
      (is (= 2 (count (:discard (get-corp)))) "ice was trashed")
      (is (= 3 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "Server 1")
      (is (= 3 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (= 2 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "Server 2")
      (is (= 4 (count (:discard (get-corp)))) "Trashable was trashed")
      (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "Server 3")
      (is (= 5 (count (:discard (get-corp)))) "Ambush was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest by-any-means-vs-controlling-the-message
    ;; vs Controlling the Message
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Paper Trail"]}
                 :runner {:deck [(qty "By Any Means" 2)]}})
      (play-from-hand state :corp "Paper Trail" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "No") ;; Don't trigger CTM trace
      (is (no-prompt? state :runner) "No prompt to steal since agenda was trashed")
      (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest by-any-means-vs-loup
  ;; vs Loup
  (do-game
    (new-game {:corp {:deck ["Paper Trail"]}
               :runner {:id "René \"Loup\" Arcemont: Party Animal"
                        :hand ["By Any Means"]
                        :deck ["Strike Fund"]}})
    (play-from-hand state :corp "Paper Trail" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "By Any Means")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Yes")
    (is (= 6 (:credit (get-runner))) "gained 2c from strike + 1c from loup")
    (is (no-prompt? state :runner) "No prompt to steal since agenda was trashed")
    (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
    (is (= 2 (count (:discard (get-runner)))) "two cards trashed")
    (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest by-any-means-alongside-film-critic-should-get-the-option-to-trigger-either
    ;; alongside Film Critic: should get the option to trigger either
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
                 :runner {:deck ["By Any Means" "Film Critic" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (play-from-hand state :runner "Film Critic")
      (is (= 1 (count (:discard (get-runner)))) "By Any Means has been played")
      (run-empty-server state "HQ")
      (is (= #{"Film Critic" "By Any Means" "Done"}
             (into #{} (prompt-titles :runner)))
          "A choice of which to trigger first")
      (click-prompt state :runner "Film Critic")
      (click-prompt state :runner "No")
      (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/move state :runner (find-card "By Any Means" (:discard (get-runner))) :hand)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "HQ")
      (is (= #{"Film Critic" "By Any Means" "Done"}
             (into #{} (prompt-titles :runner)))
          "A choice of which to trigger first")
      (click-prompt state :runner "By Any Means")
      (is (no-prompt? state :runner) "By Any Means trashes with no prompt")
      (is (= 2 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest by-any-means-effect-persists-when-moved-from-discard
    ;; Effect persists when moved from discard
    (do-game
      (new-game {:corp {:id "Skorpios Defense Systems: Persuasive Power"
                        :hand ["Hedge Fund"]}
                 :runner {:deck [(qty "By Any Means" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (card-ability state :corp (get-in @state [:corp :identity]) 1)
      (click-prompt state :corp (find-card "By Any Means" (:discard (get-runner))))
      (is (= 1 (count (get-in @state [:runner :rfg]))) "By Any Means RFGed")
      (is (zero? (count (:discard (get-corp)))) "Nothing trashed yet")
      (is (= 1 (count (:hand (get-runner)))) "No damage yet")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest by-any-means-effect-does-not-persist-between-turns
    ;; Effect does not persist between turns
    (do-game
      (new-game {:runner {:deck [(qty "By Any Means" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (zero? (count (:discard (get-corp)))) "Nothing trashed yet")
      (is (= 1 (count (:hand (get-runner)))) "No damage yet")
      (run-empty-server state "HQ")
      (is (zero? (count (:discard (get-corp)))) "Nothing trashed")
      (is (= 1 (count (:hand (get-runner)))) "No damage")))

(deftest by-any-means-effect-persists-when-moved-from-discard-issue-3341
    ;; Effect persists when moved from discard. Issue #3341
    (do-game
      (new-game {:corp {:hand ["Hedge Fund"]}
                 :runner {:hand [(qty "By Any Means" 2) "Levy AR Lab Access"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (play-from-hand state :runner "Levy AR Lab Access")
      (is (= ["By Any Means" "By Any Means"] (->> (get-runner) :hand (mapv :title)))
          "By Any Means back in the grip")
      (is (zero? (count (:discard (get-corp)))) "Nothing trashed yet")
      (is (= 2 (count (:hand (get-runner)))) "No damage yet")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
      (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")))

(deftest calling-in-favors
  ;; Calling in Favors
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Calling in Favors" 3) (qty "Aeneas Informant" 5) "Algo Trading"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Calling in Favors")
      (is (= credits (:credit (get-runner))) "Don't gain credits with nothing installed"))
    (core/gain state :runner :click 10)
    (dotimes [_ 5]
      (play-from-hand state :runner "Aeneas Informant"))
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Calling in Favors")
      (is (= (+ 5 credits) (:credit (get-runner))) "Gain 5 credits from 5 connections"))
    (play-from-hand state :runner "Algo Trading")
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Calling in Favors")
      (is (= (+ 5 credits) (:credit (get-runner))) "Gain 5 credits from 5 connections and 1 non-connection resource"))))

(deftest career-fair
  ;; Career Fair
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Career Fair" 2) "Caldera" "Beach Party"]
                        :credits 0}})
    (take-credits state :corp)
    (is (zero? (:credit (get-runner))) "Start with 0 credits")
    (play-from-hand state :runner "Career Fair")
    (click-card state :runner "Caldera")
    (is (= "Caldera" (:title (get-resource state 0))) "Caldera is installed for free")
    (play-from-hand state :runner "Career Fair")
    (click-card state :runner "Beach Party")
    (is (= "Beach Party" (:title (get-resource state 1))) "Beach Party is installed for free")
    (is (zero? (:credit (get-runner))) "Discount doesn't provide money")))

(deftest careful-planning
  ;; Careful Planning - Prevent card in/protecting remote server from being rezzed this turn
  (do-game
    (new-game {:corp {:deck ["PAD Campaign" (qty "Vanilla" 2)]}
               :runner {:deck [(qty "Careful Planning" 2)]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Vanilla" "Server 1")
    (take-credits state :corp)
    (let [pad (get-content state :remote1 0)
          v1 (get-ice state :hq 0)
          v2 (get-ice state :remote1 0)]
      (play-from-hand state :runner "Careful Planning")
      (click-card state :runner v1)
      (is (not (no-prompt? state :runner)) "Can't target card in central server")
      (click-card state :runner v2)
      (is (has-icon? state (refresh v2) "CP") "Vanilla has an icon")
      (rez state :corp v2 {:expect-rez false})
      (is (not (rezzed? (refresh v2))) "Prevented remote ice from rezzing")
      (take-credits state :runner)
      (is (nil? (:icon (refresh v2))))
      (rez state :corp (refresh v2))
      (is (rezzed? (refresh v2)) "Rez prevention of ice ended")
      (take-credits state :corp)
      (play-from-hand state :runner "Careful Planning")
      (click-card state :runner pad)
      (rez state :corp pad {:expect-rez false})
      (is (has-icon? state (refresh pad) "CP") "PAD Campaign has an icon")
      (is (not (rezzed? (refresh pad))) "Prevented remote server contents from rezzing")
      (take-credits state :runner)
      (is (nil? (:icon (refresh pad))))
      (rez state :corp (refresh pad))
      (is (rezzed? (refresh pad)) "Rez prevention of asset ended"))))

(deftest carpe-diem-no-mark
  ;; Carpe Diem - identify mark. Gain 4. You may run mark.
  (do-game
    (new-game {:runner {:hand ["Carpe Diem"]}})
    (take-credits state :corp)
    (is (nil? (:mark @state)) "No mark identified")
    (is (changed? [(:credit (get-runner)) 3]
          (play-from-hand state :runner "Carpe Diem"))
        "gained 3c  net from carpe diem")
    (click-prompt state :runner "Yes")
    (is (= (first (:server (get-run))) (:mark @state)))
    (run-continue state)
    (run-jack-out state)))

(deftest carpe-diem-hq-marked
  ;; Carpe Diem - identify mark. Gain 4. You may run mark.
  (do-game
    (new-game {:runner {:hand ["Carpe Diem"]}})
    (take-credits state :corp)
    (is (nil? (:mark @state)) "No mark identified")
    (core/set-mark state :hq)
    (is (changed? [(:credit (get-runner)) 3]
          (play-from-hand state :runner "Carpe Diem"))
        "gained 3c  net from carpe diem")
    (click-prompt state :runner "Yes")
    (is (= [:hq] (:server (get-run))) "Ran the correct server")))

(deftest cbi-raid
  ;; CBI Raid - Full test
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Adonis Campaign" "Quandary"
                             "Jackson Howard" "Global Food Initiative"]}
               :runner {:deck ["CBI Raid"]}})
    (take-credits state :corp)
    (is (= 5 (count (:hand (get-corp)))))
    (play-run-event state "CBI Raid" :hq)
    (click-prompt state :corp (find-card "Caprice Nisei" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
    ;; try starting over
    (click-prompt state :corp "Start over")
    (click-prompt state :corp (find-card "Global Food Initiative" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Quandary" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Adonis Campaign" (:hand (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:hand (get-corp)))) ;this is the top card of R&D
    (click-prompt state :corp "Done")
    (is (zero? (count (:hand (get-corp)))))
    (is (= 5 (count (:deck (get-corp)))))
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest charm-offensive
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]
                      :discard ["Ice Wall"]}
               :runner {:hand ["Charm Offensive"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-run-event state "Charm Offensive" :archives)
    (click-card state :runner (get-ice state :hq 0))
    (is (not (get-ice state :hq 0)) "Ice Wall is trashed")
    (is (= 2 (count (:discard (get-corp)))) "2 cards in trash now")))

(deftest chastushka
  ;; Chastushka - run hq, replace breach with sabotage 4
  (do-game
    (new-game {:runner {:deck ["Chastushka"]}
               :corp {:hand [(qty "Hedge Fund" 5)] :deck [(qty "Hedge Fund" 2)]}})
    (take-credits state :corp)
    (play-run-event state "Chastushka" :hq)
    (is (last-log-contains? state "uses Chastushka to sabotage 4") "Sabotage happened")))

(deftest chastushka-sabotage-auto-trashes
  (do-game
    (new-game {:runner {:deck ["Chastushka"]}
               :corp {:hand ["IPO" "Beanstalk Royalties" "IQ"] :deck ["Hedge Fund"]}})
    (take-credits state :corp)
    (play-run-event state "Chastushka" :hq)
    (is (= 4 (count (:discard (get-corp)))) "trashed 4")))

(deftest chastushka-sabotage-behaves
  (do-game
    (new-game {:runner {:deck ["Chastushka"]}
               :corp {:hand ["IPO" "Beanstalk Royalties" "IQ"] :deck [(qty "Hedge Fund" 5)]}})
    (take-credits state :corp)
    (play-run-event state "Chastushka" :hq)
    (click-card state :corp "Beanstalk Royalties")
    (click-card state :corp "IPO")
    (click-prompt state :corp "Done")
    (is (= 4 (count (:discard (get-corp)))) "trashed 4")))

(deftest chrysopeoeian-skimming-reveal
  (do-game
    (new-game {:corp {:hand ["Project Atlas" "Project Beale"]}
               :runner {:hand ["Chrysopoeian Skimming"]
                        :deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Chrysopoeian Skimming")
    (click-prompt state :corp "Project Atlas")
    (is (= 4 (:click (get-runner))))
    (is (= 1 (count (:hand (get-runner)))))))

(deftest chrysopeoeian-skimming-decline
  (do-game
    (new-game {:corp {:hand ["Project Atlas" "Project Beale" "Accelerated Beta Test" "Brainstorm" "Chiyashi"]}
               :runner {:hand ["Chrysopoeian Skimming"]
                        :deck ["Sure Gamble"]}})
    (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
    (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
    (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
    (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
    ;; R&D is now from top to bottom: A B C
    (take-credits state :corp)
    (play-from-hand state :runner "Chrysopoeian Skimming")
    (click-prompt state :corp "Done")
    (is (str/includes? (:msg (prompt-map :runner)) "Accelerated Beta Test, Brainstorm, and Chiyashi"))
    (click-prompt state :runner "OK")))

(deftest chrysopeoeian-skimming-forced-decline
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck ["Hedge Fund" "IPO" "NGO Front"]}
               :runner {:hand ["Chrysopoeian Skimming"]
                        :deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Chrysopoeian Skimming")
    (click-prompt state :corp "Done")
    (click-prompt state :runner "OK")))

(deftest clean-getaway
  (do-game
    (new-game {:corp {:hand ["Project Atlas"]}
               :runner {:hand ["Clean Getaway"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Clean Getaway")
    (click-prompt state :runner "HQ")
    (is (changed? [(:credit (get-runner)) 6]
          (run-continue-until state :success))
        "Got paid on success")))

(deftest code-siphon
  ;; Code Siphon
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Ice Wall" 2)]}
               :runner {:deck [(qty "Mass-Driver" 2)]
                        :hand [(qty "Code Siphon" 2)]
                        :credits 10}})
    (take-credits state :corp)
    (play-run-event state "Code Siphon" :rd)
    (click-prompt state :runner "Code Siphon")
    (let [credits (:credit (get-runner))
          tags (count-tags state)]
      (click-prompt state :runner "Mass-Driver")
      (is (= "Mass-Driver" (:title (get-program state 0))) "Mass-Driver is installed")
      (is (= (- credits 8) (:credit (get-runner))) "Runner pays full 8 for Mass-Driver")
      (is (= (inc tags) (count-tags state)) "Gained 1 tag"))
    (take-credits state :runner)
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Code Siphon")
    (run-continue-until state :success)
    (is (= ["Code Siphon" "Breach R&D"] (prompt-titles :runner))
        "Replacement effect isn't mandatory")
    (click-prompt state :runner "Code Siphon")
    (let [credits (:credit (get-runner))]
      (click-prompt state :runner "Mass-Driver")
      (is (= "Mass-Driver" (:title (get-program state 1))) "Mass-Driver is installed")
      (is (= (- credits 2) (:credit (get-runner))) "Runner pays discounted cost for Mass-Driver"))))

(deftest cold-read
  ;; Make a run, and place 4 on this card, which you may use only during this run.
  ;; When this run ends, trash 1 program (cannot be prevented) used during this run.
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["PAD Campaign"]}
                 :runner {:deck ["Imp" (qty "Cold Read" 2)]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (play-from-hand state :runner "Cold Read")
      (let [cr (-> (get-runner) :play-area first)]
        (click-prompt state :runner "Server 1")
        (run-continue state)
        (is (= 4 (get-counters (refresh cr) :credit)) "Cold Read has 4 counters"))
      (run-continue state)
      (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
      (click-card state :runner (get-program state 0))
      (is (= 2 (count (:discard (get-runner)))) "Imp and Cold Read in discard")))

(deftest cold-read-earth-station
  ;;ensure that cold read can't pay for the earth station ability
  (do-game
    (new-game {:runner {:hand ["Cold Read" "Sure Gamble"]}
               :corp {:id "Earth Station: SEA Headquarters" :hand ["PAD Campaign"]}})
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (:flipped (get-in @state [:corp :identity])) "Earth station is on flip side")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (is (changed? [(:credit (get-runner)) -6]
          (play-from-hand state :runner "Cold Read")
          (click-prompt state :runner "Server 1"))
        "Paid for earth station")
    (is (no-prompt? state :runner) "waiting on earth station payment prompt")))

(deftest cold-read-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Cold Read" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Refractor")
      (play-from-hand state :runner "Cold Read")
      (click-prompt state :runner "Archives")
      (let [refr (get-program state 0)
            cr (first (get-in @state [:runner :play-area]))]
        (run-continue state)
        (card-ability state :runner refr 1)
        (is (changed? [(:credit (get-runner)) 0]
              (click-card state :runner cr))
            "Used 1 credit from Cold Read")
        (run-continue state)
        (click-card state :runner refr)
        (is (= 2 (count (:discard (get-runner)))) "Cold Read and Refractor in discard"))))

(deftest ^{:card-title "compile"}
  compile-test
  ;; Compile - Make a run, and install a program for free which is shuffled back into stack
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:deck ["Gordian Blade"]
                          :hand ["Compile"]
                          :credits 15}})
      (play-from-hand state :corp "Enigma" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Stack")
      (click-prompt state :runner "Gordian Blade")
      (is (:installed (get-program state 0)) "Gordian Blade should be installed")
      (let [deck (count (:deck (get-runner)))]
        (run-continue state :movement)
        (run-jack-out state)
        (is (= (inc deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
        (is (nil? (get-program state 0))))))

(deftest compile-test-can-install-from-discard-issue-4441
    ;; Can install from discard. Issue #4441
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:hand ["Compile"]
                          :discard ["Gordian Blade"]}})
      (play-from-hand state :corp "Enigma" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Heap")
      (click-prompt state :runner "Gordian Blade")
      (is (:installed (get-program state 0)) "Gordian Blade should be installed")
      (run-continue state :movement)
      (run-jack-out state)
      (is (find-card "Gordian Blade" (:deck (get-runner))) "Gordian Blade should be back in stack")
      (is (nil? (get-program state 0)))))

(deftest compile-test-with-self-modifying-code-neither-smc-nor-other-card-should-be-shuffled-back-in
    ;; with Self-modifying Code, neither SMC nor other card should be shuffled back in
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma"]}
                 :runner {:hand ["Compile" "Clone Chip"]
                          :deck [(qty "Self-modifying Code" 3)]
                          :credits 10}})
      (play-from-hand state :corp "Enigma" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Clone Chip")
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Stack")
      (click-prompt state :runner "Self-modifying Code")
      (let [smc (get-program state 0)
            clone-chip (get-hardware state 0)]
        (card-ability state :runner smc 0)
        (click-prompt state :runner (find-card "Self-modifying Code" (:deck (get-runner))))
        (card-ability state :runner clone-chip 0)
        (click-card state :runner (find-card "Self-modifying Code" (:discard (get-runner)))))
      (let [deck (count (:deck (get-runner)))]
        (run-continue state :movement)
        (run-jack-out state)
        (is (= deck (count (:deck (get-runner)))) "No card was shuffled back into the stack"))))

(deftest compile-test-vs-ending-the-run-via-corp-action-3639
    ;; vs ending the run via corp action. #3639
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Gordian Blade"]
                          :hand ["Compile"]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (let [iw (get-ice state :archives 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Compile")
        (click-prompt state :runner "Archives")
        (run-continue state)
        (rez state :corp iw)
        (run-continue state)
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Stack")
        (click-prompt state :runner "Gordian Blade")
        (is (:installed (get-program state 0)) "Gordian Blade should be installed")
        (let [deck (count (:deck (get-runner)))]
          (fire-subs state iw)
          (is (= (inc deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
          (is (nil? (get-program state 0)))))))

(deftest compile-test-vs-hosting
    ;; vs ending the run via corp action. #3639
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Gordian Blade"]
                          :hand ["NetChip" "Compile"]}})
      (play-from-hand state :corp "Ice Wall" "Archives")
      (let [iw (get-ice state :archives 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "NetChip")
        (play-from-hand state :runner "Compile")
        (click-prompt state :runner "Archives")
        (run-continue state)
        (rez state :corp iw)
        (run-continue state)
        (click-prompts state :runner "Yes" "Stack" "Gordian Blade" "NetChip")
        (is (:installed (first (:hosted (get-hardware state 0)))) "Gordian Blade should be installed")
        (let [deck (count (:deck (get-runner)))]
          (fire-subs state iw)
          (is (= (inc deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
          (is (nil? (first (:hosted (get-hardware state 0)))) "Gordian Blade should be gone")))))

(deftest compile-test-only-asks-once-per-run-issue-4749
    ;; Only asks once per run. Issue #4749
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma" "Enigma"]
                        :credits 10}
                 :runner {:deck ["Gordian Blade"]
                          :hand ["Compile"]
                          :credits 15}})
      (play-from-hand state :corp "Enigma" "Archives")
      (play-from-hand state :corp "Enigma" "Archives")
      (take-credits state :corp)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (rez state :corp (get-ice state :archives 1))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Stack")
      (click-prompt state :runner "Gordian Blade")
      (is (:installed (get-program state 0)) "Gordian Blade should be installed")
      (run-continue state)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (is (empty? (:hand (get-runner))) "No Compile prompt")))

(deftest compile-test-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Blacklist" "Enigma"]}
                 :runner {:discard ["Gordian Blade"]
                          :hand    ["Compile"]
                          :credits 15}})
      (play-from-hand state :corp "Enigma" "Archives")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (rez state :corp (get-ice state :archives 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (= "Choose where to install the program from" (:msg (prompt-map :runner))) "Stack is only option")
      (is (= 1 (-> (prompt-map :runner) :choices count)) "Runner has 1 choice")
      (is (= ["Stack"] (prompt-buttons :runner)) "Runner's only choice is Stack")))

(deftest compile-effects-ordering-1
    ;; Compile & Mayfly - should get order of choice
    (do-game
      (new-game {:corp {:deck ["Masvingo"]
                        :credits 20}
                 :runner {:hand ["Compile"]
                          :deck ["Mayfly"]
                          :credits 20}})
      (play-from-hand state :corp "Masvingo" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Stack")
      (click-prompt state :runner "Mayfly")
      (let [mayfly (get-program state 0)
            deck (count (:deck (get-runner)))]
        (auto-pump-and-break state (refresh mayfly))
        (core/process-action "continue" state :corp nil)
        (run-jack-out state)
        (click-prompt state :runner "Compile")
        (is (= (inc deck) (count (:deck (get-runner)))) "Mayfly should be back in stack"))))

(deftest compile-effects-ordering-2
    ;; Compile & Mayfly - should get order of choice
    (do-game
      (new-game {:corp {:deck ["Masvingo"]
                        :credits 20}
                 :runner {:hand ["Compile"]
                          :deck ["Mayfly"]
                          :credits 20}})
      (play-from-hand state :corp "Masvingo" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Compile")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Stack")
      (click-prompt state :runner "Mayfly")
      (let [mayfly (get-program state 0)]
        (auto-pump-and-break state (refresh mayfly))
        (core/process-action "continue" state :corp nil)
        (run-jack-out state)
        (click-prompt state :runner "Mayfly")
        (is (zero? (count (:deck (get-runner)))) "Mayfly should not be back in the stack"))))

(deftest concerto
  ;; Concerto
  (do-game
      (new-game {:corp {:deck ["Sealed Vault"]}
                 :runner {:deck ["Sure Gamble"]
                          :hand ["Concerto"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Concerto")
      (let [concerto (-> (get-runner) :play-area first)]
        (is (last-log-contains? state "reveal Sure Gamble"))
        (is (find-card "Sure Gamble" (:hand (get-runner))))
        (is (= 5 (get-counters (refresh concerto) :credit))))
      (click-prompt state :runner "HQ")
      (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Pay 8 [Credits] to trash")
      (dotimes [_ 5]
        (click-card state :runner "Concerto"))
      (is (and (zero? (count (:hand (get-corp))))
               (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Sealed Vault trashed")
      (is (= 2 (:credit (get-runner))))))

(deftest concerto-on-empty-stack
  ;; Concerto - no cards left in the stack
  (do-game
      (new-game {:runner {:hand ["Concerto"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Concerto")
      (let [concerto (-> (get-runner) :play-area first)]
        (is (not (last-log-contains? state "reveal")))
        (is (zero? (get-counters (refresh concerto) :credit))))))

(deftest contaminate
  ;; Contaminate - add 3 virus counters to an installed runner card with no virus counters
  (do-game
      (new-game {:runner {:deck ["Yusuf" "Chrome Parlor" (qty "Contaminate" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5 :click 2)
      (play-from-hand state :runner "Yusuf")
      (play-from-hand state :runner "Chrome Parlor")
      (let [yus (get-program state 0)
            cp (get-resource state 0)]
        (is (zero? (get-counters (refresh yus) :virus)) "Yusuf starts with 0 virus counters")
        (is (zero? (get-counters (refresh cp) :virus)) "Chrome Parlor starts with 0 virus counters")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh yus))
        (is (= 3 (get-counters (refresh yus) :virus)) "Yusuf has 3 counters after Contaminate")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh cp))
        (is (= 3 (get-counters (refresh cp) :virus)) "Chrome Parlor has 3 counters after Contaminate")
        (play-from-hand state :runner "Contaminate")
        (is (last-log-contains? state "do nothing") "no valid targets")
        (is (= 3 (get-counters (refresh cp) :virus)) "Yusuf isn't selectable by Contaminate"))))

(deftest contaminate-hivemind-makes-virus-programs-act-like-they-have-a-virus-counter
    ;; Hivemind makes virus programs act like they have a virus counter
    (do-game
      (new-game {:runner {:deck ["Aumakua" "Friday Chip" "Hivemind" "Contaminate"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5 :click 2)
      (play-from-hand state :runner "Aumakua")
      (play-from-hand state :runner "Hivemind")
      (play-from-hand state :runner "Friday Chip")
      (let [aum (get-program state 0)
            fc (get-hardware state 0)]
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua starts with 0 virus counters (not counting Hivemind)")
        (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip starts with 0 virus counters")
        (play-from-hand state :runner "Contaminate")
        (click-card state :runner (refresh aum))
        (click-card state :runner (refresh fc))
        (is (= 3 (get-counters (refresh fc) :virus)) "Friday Chip has 3 counters after Contaminate")
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua ends with 0 virus counters (not counting Hivemind)"))))

(deftest corporate-grant
  ;; Corporate "Grant" - First time runner installs a card, the corp loses 1 credit
  (do-game
      (new-game {:runner {:deck ["Corporate \"Grant\"" (qty "Daily Casts" 2)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Corporate \"Grant\"")
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (play-from-hand state :runner "Daily Casts")
      (is (= 7 (:credit (get-corp))) "Corp loses 1 credit")
      (play-from-hand state :runner "Daily Casts")
      (is (empty? (:hand (get-runner))) "Played all cards in hand")
      (is (= 7 (:credit (get-corp))) "Corp doesn't lose 1 credit")))

(deftest corporate-grant-with-hayley-kaplan-issue-3162
    ;; with Hayley Kaplan. Issue #3162
    (do-game
      (new-game {:runner {:id "Hayley Kaplan: Universal Scholar"
                          :deck ["Corporate \"Grant\"" (qty "Clone Chip" 2)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Corporate \"Grant\"")
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (play-from-hand state :runner "Clone Chip")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Clone Chip" (:hand (get-runner))))
      (is (= 7 (:credit (get-corp))) "Corp only loses 1 credit")))

(deftest corporate-scandal
  ;; Corporate Scandal - Corp has 1 additional bad pub even with 0
  (do-game
    (new-game {:corp {:deck ["Elizabeth Mills"]}
               :runner {:deck ["Corporate Scandal" "Activist Support"
                               "Raymond Flint" "Investigative Journalism"]}})
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 5 :click 1)
    (play-from-hand state :runner "Raymond Flint")
    (play-from-hand state :runner "Corporate Scandal")
    (is (no-prompt? state :runner) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :runner "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-resource state 1))) "IJ able to be installed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-runner))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :runner "Activist Support")
    (take-credits state :runner)
    (let [em (get-content state :remote1 0)]
      (rez state :corp em)
      (is (= 1 (count-bad-pub state)) "Corp still has BP")
      (take-credits state :corp)
      (is (zero? (get-in (get-corp) [:bad-publicity :base])) "Corp has BP, didn't take 1 from Activist Support"))))

(deftest creative-commision
  ;; Creative Commission
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 2)]}
                 :runner {:hand ["Creative Commission"]}})
      (take-credits state :corp)
      (let [clicks (:click (get-runner))]
        (is (changed? [(:credit (get-runner)) 4]
              (play-from-hand state :runner "Creative Commission"))
            "gain 4 credits from Creative Commission")
        (is (= (+ clicks -1 -1) (:click (get-runner))) "Runner plays Creative Commission and loses 1 click"))))

(deftest credit-crash-corp-pays-to-keep
    ;; Corp pays to keep
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Credit Crash"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Credit Crash")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (let [credits (:credit (get-corp))
            discard (count (:discard (get-corp)))]
        (click-prompt state :corp "Yes")
        (is (= (- credits 5) (:credit (get-corp))) "Corp paid 5 credits to save Hedge Fund")
        (is (= discard (count (:discard (get-corp)))) "Corp has no cards trashed"))
      (click-prompt state :runner "No action")))

(deftest credit-crash-corp-doesn-t-pay-to-keep
    ;; Corp doesn't pay to keep
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Credit Crash"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Credit Crash")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (let [credits (:credit (get-corp))
            discard (count (:discard (get-corp)))]
        (click-prompt state :corp "No")
        (is (= credits (:credit (get-corp))) "Corp doesn't pay 5 credits to save Hedge Fund")
        (is (= (inc discard) (count (:discard (get-corp)))) "Corp has 1 card trashed"))
      (is (no-prompt? state :runner) "Runner has no access prompts")))

(deftest credit-kiting
  ;; Credit Kiting - After successful central run lower install cost by 8 and gain a tag
  (do-game
    (new-game {:corp {:deck ["PAD Campaign" "Ice Wall"]}
               :runner {:deck ["Credit Kiting" "Femme Fatale"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Credit Kiting")
    (is (= 3 (:click (get-runner))) "Card not played, successful run on central not made")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Credit Kiting")
    (click-card state :runner (find-card "Femme Fatale" (:hand (get-runner))))
    (is (= 4 (:credit (get-runner))) "Femme Fatale only cost 1 credit")
    (testing "Femme Fatale can still target ice when installed with Credit Kiting, issue #3715"
      (let [iw (get-ice state :rd 0)]
        (click-card state :runner iw)
        (is (has-icon? state (refresh iw) "FF") "Ice Wall has an icon")))
    (is (= 1 (count-tags state)) "Runner gained a tag")))

(deftest cyber-threat-corp-rezzes-a-piece-of-ice
    ;; Corp rezzes a piece of ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Cyber Threat"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cyber Threat")
      (click-prompt state :runner "HQ")
      (click-prompt state :corp "Yes")
      (let [iw (get-ice state :hq 0)
            credits (:credit (get-corp))]
        (click-card state :corp "Ice Wall")
        (is (rezzed? (refresh iw)) "Ice Wall is rezzed")
        (is (= (dec credits) (:credit (get-corp))) "Corp paid to rez Ice Wall"))))

(deftest cyber-threat-corp-doesn-t-rez-a-piece-of-ice
    ;; Corp doesn't rez a piece of ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Cyber Threat"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Cyber Threat")
      (click-prompt state :runner "HQ")
      (click-prompt state :corp "No")
      (let [iw (get-ice state :hq 0)]
        (is (:run @state) "Run has been initiated")
        (rez state :corp iw {:expect-rez false})
        (is (not (rezzed? (refresh iw))) "Corp can't rez ice this run"))))

(deftest data-breach
  ;; Data Breach
  (do-game
      (new-game {:runner {:deck [(qty "Data Breach" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (play-run-event state "Data Breach" :rd)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (:server (:run @state))) "Second run on R&D triggered")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No prompt to run a third time")
      (is (not (:run @state)) "Run is over")
      (play-from-hand state :runner "Data Breach")
      (run-continue state)
      (run-jack-out state)
      (is (no-prompt? state :runner) "No option to run again on unsuccessful run")))

(deftest data-breach-faq-4-1-ensure-runner-gets-choice-of-activation-order
    ;; FAQ 4.1 - ensure runner gets choice of activation order
    (do-game
      (new-game {:runner {:deck ["Doppelgänger" (qty "Data Breach" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Doppelgänger")
      ;; Playing Data Breach manually to handle Successful Run events
      (play-from-hand state :runner "Data Breach")
      (is (:run @state) "There is a run happening")
      (is (= [:rd] (get-in @state [:run :server])) "Correct server is run")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Doppelgänger")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "HQ")
      (is (:run @state) "New run started")
      (is (= [:hq] (:server (:run @state))) "Running on HQ via Doppelgänger")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (:server (:run @state))) "Second Data Breach run on R&D triggered")
      (run-continue state)
      (run-continue state)
      (is (not (:run @state)) "All runs has been successful completed")))

(deftest day-job
  ;; Day Job
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Day Job"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Day Job")
      (is (= (+ credits -2 10) (:credit (get-runner))) "Runner spends 2, gains 10")
      (is (zero? (:click (get-runner))) "Runner loses 4 clicks"))))

(deftest deep-data-mining
  ;; Deep Data Mining
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hedge Fund"]}
               :runner {:hand [(qty "Deep Data Mining" 2) "Magnum Opus"]
                        :credits 15}})
    (take-credits state :corp)
    (play-run-event state "Deep Data Mining" :rd)
    (dotimes [_ 5] ; 1 normal access, 4 extra accesses from DDM
      (click-prompt state :runner "No action"))
    (play-from-hand state :runner "Magnum Opus")
    (play-run-event state "Deep Data Mining" :rd)
    (dotimes [_ 3] ; 1 normal access, 2 extra accesses from DDM because Magnum Opus takes 2 MU
      (click-prompt state :runner "No action"))
    (is (no-prompt? state :runner) "No more accesses after 3")))

(deftest deep-dive-single-card
  ;;there is only a single card to access - the runner isn't offered to spend a click.
  (do-game
    (new-game {:corp {:deck ["Fire Wall" "Brainstorm" "Chiyashi"
                             "DNA Tracker" "Excalibur" "PAD Campaign"]}
               :runner {:hand ["Deep Dive" "Deep Dive" "Deep Dive"]}})
    (draw state :corp)
    (core/move state :corp (find-card "PAD Campaign" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    ;; R&D is now: Pad Campaign
    (core/gain state :runner :click 1)
    (core/gain state :runner :credit 100)
    ;;run the three centrals
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "PAD Campaign")
    (click-prompt state :runner "No action")
    (is (no-prompt? state :runner) "Access not completed or offered second access")
    (is (no-prompt? state :corp) "Corp stuck waiting for runner")
    ;;now we check pad campaign still exists, and that we didn't spend a second click
    (is (= 1 (count (:deck (get-corp)))))
    (is (= 0 (count (:discard (get-corp)))))
    (is (= 1 (:click (get-runner))) "Shouldn't lose a click from deep dive")))

(deftest deep-dive-two-cards
  (do-game
    (new-game {:corp {:hand ["Fire Wall" "Brainstorm" "Chiyashi"
                             "DNA Tracker" "Marilyn Campaign" "PAD Campaign"]}
               :runner {:hand ["Deep Dive" "Deep Dive" "Deep Dive"]}})
    (draw state :corp)
    (core/move state :corp (find-card "PAD Campaign" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Marilyn Campaign" (:hand (get-corp))) :deck)
    (is (= (:title (nth (-> @state :corp :deck) 0)) "PAD Campaign"))
    (is (= (:title (nth (-> @state :corp :deck) 1)) "Marilyn Campaign"))
    ;;R&D is now: PAD Campaign, Marilyn Campaign
    (take-credits state :corp)
    (core/gain state :runner :click 96)
    (core/gain state :runner :credit 100)
    ;;run the three centrals
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    ;;access pad campaign, do not trash it, then refuse the second access
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "PAD Campaign")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No")
    (is (no-prompt? state :runner)
        "Runner shouldn't be given the option to access from an empty R&D")
    (is (no-prompt? state :corp)
        "Corporation player shouldn't be waiting on prompts")
    (is (= 96 (:click (get-runner))) "Shouldn't spend a click with Deep Dive")
    ;;access marilyn campaign, trash it, then access pad campaign
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "Marilyn Campaign")
    (click-prompt state :runner "Pay 3 [Credits] to trash")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "PAD Campaign")
    (click-prompt state :runner "Pay 4 [Credits] to trash")
    (is (no-prompt? state :runner)
        "Runner should not be waiting on a prompt")
    (is (no-prompt? state :corp)
        "Corporation player shouldn't be waiting on prompts (again)")
    (is (= 94 (:click (get-runner))) "Should have spend two clicks on Deep Dive")
    (is (= 0 (count (:deck (get-corp)))))
    (is (= 2 (count (:discard (get-corp)))))))

(deftest deep-dive-ikawah-cost-for-second-card
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 3) "Ikawah Project" "Project Vitruvius"]}
               :runner {:hand ["Deep Dive"]}})
    (draw state :corp)
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Ikawah Project" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Project Vitruvius" (:hand (get-corp))) :deck)
    ;;R&D is now: Hedge Fund, Ikawah Project, Project Vitruvius
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    ;;run the three centrals
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "Ikawah Project")
    (click-prompt state :runner "Pay to steal")
    (is (no-prompt? state :runner)
        "Runner shouldn't be given the option to access the second card since he spent the last click to steal Ikawah")))

(deftest deep-dive-basic-functionality
  (do-game
    (new-game {:corp {:hand ["Ansel 1.0" "Better Citizen Program" "Chiyashi"
                             "Dedicated Technician Team"
                             "Efficiency Committee" "Friends in High Places" "Gyri Labyrinth"
                             "Heimdall 1.0" "Ichi 1.0" "Janus 1.0"]}
               :runner {:hand ["Deep Dive" "Deep Dive" "Deep Dive"]}})
    (draw state :corp)
    (core/move state :corp (find-card "Ansel 1.0" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Better Citizen Program" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Dedicated Technician Team" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Efficiency Committee" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Friends in High Places" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Gyri Labyrinth" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Heimdall 1.0" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Ichi 1.0" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Janus 1.0" (:hand (get-corp))) :deck)
    (is (= (:title (nth (-> @state :corp :deck) 0)) "Ansel 1.0"))
    (is (= (:title (nth (-> @state :corp :deck) 1)) "Better Citizen Program"))
    (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
    (is (= (:title (nth (-> @state :corp :deck) 3)) "Dedicated Technician Team"))
    (is (= (:title (nth (-> @state :corp :deck) 4)) "Efficiency Committee"))
    (is (= (:title (nth (-> @state :corp :deck) 5)) "Friends in High Places"))
    (is (= (:title (nth (-> @state :corp :deck) 6)) "Gyri Labyrinth"))
    (is (= (:title (nth (-> @state :corp :deck) 7)) "Heimdall 1.0"))
    (is (= (:title (nth (-> @state :corp :deck) 8)) "Ichi 1.0"))
    (is (= (:title (nth (-> @state :corp :deck) 9)) "Janus 1.0"))
    ;;R&D is now: A B C D E F G H I J
    (take-credits state :corp)
    (core/gain state :runner :click 96)
    (core/gain state :runner :credit 100)
    ;;run the three centrals
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    ;;steal better citizen and effcom
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "Better Citizen Program")
    (click-prompt state :runner "Steal")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Efficiency Committee")
    (click-prompt state :runner "Steal")
    (is (last-log-contains? state "Corp shuffles R&D"))
    (is (no-prompt? state :runner)
        "Runner should not be waiting on a prompt")
    (is (no-prompt? state :corp)
        "Corporation player shouldn't be waiting on prompts")
    (is (= 95 (:click (get-runner))) "Should have spend two clicks on Deep Dive")
    (is (= 8 (count (:deck (get-corp)))))
    (is (= 0 (count (:discard (get-corp)))))
    (is (= 4 (:agenda-point (get-runner))) "Runner scored two agendas")))

(deftest deep-dive-bacterial-programming
  (do-game
    (new-game {:corp {:hand ["Hedge Fund" "Bacterial Programming" "Jumon"]
                      :deck [(qty "Hedge Fund" 16)]}
               :runner  {:hand ["Deep Dive"]}})
    (core/move state :corp (find-card "Bacterial Programming" (:hand (get-corp))) :deck {:front true})
    (core/move state :corp (find-card "Jumon" (:hand (get-corp))) :deck {:front true})
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck {:front true})
    (is (= (:title (nth (-> @state :corp :deck) 0)) "Hedge Fund"))
    (is (= (:title (nth (-> @state :corp :deck) 1)) "Jumon"))
    (is (= (:title (nth (-> @state :corp :deck) 2)) "Bacterial Programming"))
    (take-credits state :corp)
    (core/gain state :runner :click 96)
    (core/gain state :runner :credit 100)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    ;;steal bacterial, then later, jumon
    (play-from-hand state :runner "Deep Dive")
    (is (not (:run @state)))
    (click-prompt state :runner "OK")
    (click-prompt state :runner "Bacterial Programming")
    (click-prompt state :runner "Steal")
    (is (not (:run @state)))
    ;; resolve bacterial
    (click-prompt state :corp "Yes")
    (click-prompt state :corp "Done")
    (click-prompt state :corp "Done")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Hedge Fund")
    (click-prompt state :corp "Done")
    (is (not (:run @state)))
    ;; should be able to continue to stealing jumon
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Jumon")
    (click-prompt state :runner "Steal")
    ;; these should be failing here, but pass?
    (is (no-prompt? state :corp) "No lingering prompt from bacterial")
    (is (no-prompt? state :runner) "No lingering prompt from bacterial")
    (is (not (:run @state)))))

(deftest deep-dive-degree-mill
  (do-game
    (new-game {:corp {:hand ["Hedge Fund" "Degree Mill" "Jumon"]
                      :deck [(qty "Hedge Fund" 16)]}
               :runner  {:hand ["Deep Dive" "PAD Tap" "Daily Casts"]}})
    (core/move state :corp (find-card "Degree Mill" (:hand (get-corp))) :deck {:front true})
    (core/move state :corp (find-card "Jumon" (:hand (get-corp))) :deck {:front true})
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck {:front true})
    (is (= (:title (nth (-> @state :corp :deck) 0)) "Hedge Fund"))
    (is (= (:title (nth (-> @state :corp :deck) 1)) "Jumon"))
    (is (= (:title (nth (-> @state :corp :deck) 2)) "Degree Mill"))
    (take-credits state :corp)
    (core/gain state :runner :click 96)
    (core/gain state :runner :credit 100)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    ;;steal bacterial, then later, jumon
    (play-from-hand state :runner "PAD Tap")
    (play-from-hand state :runner "Daily Casts")
    (is (= nil (:phase (get-run))) "There is no run")
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "Degree Mill")
    (click-prompt state :runner "Pay to steal")
    (is (not (:run @state)))
    (click-card state :runner "PAD Tap")
    (click-card state :runner "Daily Casts")
    (is (not (:run @state)))
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Jumon")
    (click-prompt state :runner "Steal")
    (is (not (:run @state)))))

(deftest deep-dive-strongbox-ikawah-bellona
  (do-game
    (new-game {:corp {:hand ["Strongbox" "Ikawah Project" "Bellona" "Fire Wall"]}
               :runner {:hand ["Deep Dive" "Deep Dive" "Deep Dive"]}})
    (draw state :corp)
    (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Ikawah Project" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Bellona" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Strongbox" "R&D")
    (rez state :corp (get-content state :rd 0))
    (take-credits state :corp)
    (core/gain state :runner :click 96)
    (core/gain state :runner :credit 95)
    ;;run the three centrals
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "Card from deck")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    ;;play from hand, and attempt to steal ikawah - this should cost us 1 click and 2 credits
    (play-from-hand state :runner "Deep Dive")
    (click-prompt state :runner "OK")
    (click-prompt state :runner "Bellona")
    (click-prompt state :runner "Pay to steal")
    ;;runner should now have 95 clicks and 93 credits
    (is (= 96 (:click (get-runner))) "Should NOT have spent +1 click to steal bellona")
    (is (= 93 (:credit (get-runner))) "Should have spent +5 credits to steal bellona")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Ikawah Project")
    (click-prompt state :runner "Pay to steal")
    ;;runner should now have 94 clicks and 91 credits
    (is (= 94 (:click (get-runner))) "Should have spent +1 click to steal Ikawah")
    (is (= 91 (:credit (get-runner))) "Should have spent +2 credits to steal Ikawah")))

(deftest deja-vu
  ;; Deja Vu - recur one non-virus or two virus cards
  (do-game
    (new-game {:runner {:deck [(qty "Déjà Vu" 2)
                               "Cache"
                               "Datasucker"
                               "Dirty Laundry"]}})
    (take-credits state :corp 3) ; pass to runner's turn
    (trash-from-hand state :runner "Cache")
    (trash-from-hand state :runner "Datasucker")
    (trash-from-hand state :runner "Dirty Laundry")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in hand prior to playing Déjà Vu")
    (play-from-hand state :runner "Déjà Vu")
    (click-prompt state :runner (find-card "Dirty Laundry" (:discard (get-runner))))
    (is (no-prompt? state :runner) "Recurring a non-virus card stops Déjà Vu prompting further")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in after playing Déjà Vu")
    (play-from-hand state :runner "Déjà Vu")
    (click-prompt state :runner (find-card "Cache" (:discard (get-runner))))
    (is (not (no-prompt? state :runner)) "Recurring a virus card causes Déjà Vu to prompt for second virus to recur")
    (click-prompt state :runner (find-card "Datasucker" (:discard (get-runner))))
    (is (= 3 (count (:hand (get-runner)))) "Three cards in after playing second Déjà Vu")))

(deftest deja-vu-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Blacklist"]}
                 :runner {:deck [(qty "Déjà Vu" 2)
                                 "Cache"
                                 "Datasucker"
                                 "Dirty Laundry"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (trash-from-hand state :runner "Cache")
      (trash-from-hand state :runner "Datasucker")
      (trash-from-hand state :runner "Dirty Laundry")
      (is (= 2 (count (:hand (get-runner)))) "Two cards in hand prior to playing Déjà Vu")
      (play-from-hand state :runner "Déjà Vu")
      (is (no-prompt? state :runner) "Prompt did not come up")))

(deftest demolition-run
  ;; Demolition Run - Trash at no cost
  (do-game
    (new-game {:corp {:deck ["False Lead"
                             "Shell Corporation"
                             (qty "Hedge Fund" 3)]}
               :runner {:deck ["Demolition Run"]}})
    (core/move state :corp (find-card "False Lead" (:hand (get-corp))) :deck) ; put False Lead back in R&D
    (play-from-hand state :corp "Shell Corporation" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :corp 2) ; pass to runner's turn by taking credits
    (play-from-hand state :runner "Demolition Run")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits for the event")
    (click-prompt state :runner "R&D")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Unrezzed upgrade")
    (click-prompt state :runner "[Demolition Run] Trash card")
    (is (= 3 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
    (click-prompt state :runner "[Demolition Run] Trash card")
    (is (zero? (:agenda-point (get-runner))) "Didn't steal False Lead")
    (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
    (is (no-prompt? state :runner) "Run concluded")))

(deftest deuces-wild
  ;; Deuces Wild
  (do-game
    (new-game {:corp {:deck ["Wraparound"
                             "The Future Perfect"]}
               :runner {:deck [(qty "Deuces Wild" 2) (qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Wraparound" "New remote")
    (take-credits state :corp)
    (starting-hand state :runner ["Deuces Wild" "Deuces Wild"])
    (play-from-hand state :runner "Deuces Wild")
    (click-prompt state :runner "Gain 3 [Credits]")
    (is (= 6 (:credit (get-runner))) "Gained 1 net credit")
    (click-prompt state :runner "Draw 2 cards")
    (is (= 3 (count (:hand (get-runner)))) "Drew 2 cards")
    (is (no-prompt? state :runner) "Deuces Wild not showing a third choice option")
    (play-from-hand state :runner "Deuces Wild")
    (click-prompt state :runner "Expose 1 ice and make a run")
    (click-card state :runner (get-ice state :remote1 0))
    (click-prompt state :runner "HQ")
    (is (no-prompt? state :runner) "Deuces prompt not queued")
    (run-continue state)
    (run-continue state)
    (is (= 3 (count (:prompt (get-runner)))) "Deuces prompt not queued")
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "TFP stolen")
    (gain-tags state :runner 1)
    (is (= 1 (count-tags state)) "Runner has 1 tag")
    (click-prompt state :runner "Remove 1 tag")
    (is (zero? (count-tags state)))))

(deftest diana-s-hunt
  ;; Diana's Hunt
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}
               :runner {:hand ["Diana's Hunt" "Mass-Driver"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Diana's Hunt")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (click-prompt state :runner "Yes")
    (click-card state :runner "Mass-Driver")
    (is (= "Mass-Driver" (:title (get-program state 0))) "Mass-Driver is installed")
    (run-continue state)
    (run-continue state)
    (is (not (get-program state 0)) "Mass-Driver is uninstalled")
    (is (= "Mass-Driver" (:title (get-discarded state :runner 0))) "Mass-Driver is in the heap")))

(deftest diesel
  ;; Diesel
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Diesel"]}})
    (take-credits state :corp)
    (let [hand (count (:hand (get-runner)))]
      (play-from-hand state :runner "Diesel")
      (is (= (+ hand -1 3) (count (:hand (get-runner)))) "Runner plays Diesel and draws 3 cards"))))

(deftest direct-access-direct-access-employee-strike-interaction
    ;; Direct Access/Employee Strike interaction
    (do-game
     (new-game {:runner {:deck ["Direct Access" "Employee Strike"]
                         :id "Alice Merchant: Clan Agitator"}
                :corp {:deck [(qty "PAD Campaign" 3)]
                       :id "Jinteki: Replicating Perfection"}})
     (play-from-hand state :corp "PAD Campaign" "New remote")
     (take-credits state :corp)
     (play-from-hand state :runner "Direct Access")
     (click-prompt state :runner "Server 1")
     (run-continue state)
     (is (= :remote1 (get-in @state [:run :server 0])) "Running on remote vs RP")
     (run-continue state)
     (click-prompt state :runner "No action")
     (click-prompt state :runner "Yes")
     (is (= "Direct Access" (-> (get-runner) :deck first :title)) "Direct Access shuffled into stack")
     (run-on state "Server 1" {:wait-at-initiation true})
     (is (and (= 3 (:click (get-runner))) (not (:run @state))) "RP prevented running on remote")
     (click-draw state :runner)
     (play-from-hand state :runner "Direct Access")
     (click-prompt state :runner "Archives")
     (run-continue state)
     (run-continue state)
     (is (= "Waiting for Runner to resolve pending triggers" (:msg (prompt-map :corp))) "Corp not forced to discard for Alice")
     (click-prompt state :runner "Yes")
     (click-draw state :runner)
     (take-credits state :runner)
     (take-credits state :corp)
     (play-from-hand state :runner "Employee Strike")
     (play-from-hand state :runner "Direct Access")
     (click-prompt state :runner "Server 1")
     (run-continue state)
     (run-continue state)
     (click-prompt state :runner "No action")
     (click-prompt state :runner "No")
     (is (= "Direct Access" (-> (get-runner) :discard first :title)) "Direct Access discarded")
     (run-on state "Server 1")
     (is (:run @state) "RP blank, so did not prevent running on remote")
     (run-continue state)
     (click-prompt state :runner "No action")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (changed? [(count (:discard (get-corp))) 1]
           (run-empty-server state "Archives")
           (click-card state :corp (find-card "PAD Campaign" (:hand (get-corp)))))
         "Alice not permanently blanked")))

(deftest direct-access-blanks-ids-before-they-can-trigger-6108
    ;; Blanks IDs before they can trigger #6108
    (do-game
     (new-game {:corp {:id "Azmari EdTech: Shaping the Future"
                       :deck [(qty "Hedge Fund" 5)]
                       :hand [(qty "Hedge Fund" 5)]}
                :runner {:id "Ken \"Express\" Tenma: Disappeared Clone"
                         :hand ["Direct Access"]}})
     (take-credits state :corp)
     (click-prompt state :corp "Event")
     (let [runner-credits (- (:credit (get-runner)) 1);; subtract 1 for the cost of Direct Access
           corp-credits (:credit (get-corp))]
       (play-from-hand state :runner "Direct Access")
       (is (= runner-credits (:credit (get-runner))) "Ken did not trigger")
       (is (= corp-credits (:credit (get-corp))) "Azmari did not trigger"))))

(deftest dirty-laundry
  ;; Dirty Laundry - Gain 5 credits at the end of the run if it was successful
  (do-game
     (new-game {:runner {:deck [(qty "Dirty Laundry" 2)]}})
     (take-credits state :corp)
     (play-from-hand-with-prompts state :runner "Dirty Laundry" "Archives")
     (run-continue-until state :success)
     (is (= 8 (:credit (get-runner))) "Gained 5 credits")
     (play-from-hand state :runner "Dirty Laundry")
     (click-prompt state :runner "Archives")
     (run-continue state)
     (run-jack-out state)
     (is (= 6 (:credit (get-runner))) "Run unsuccessful; gained no credits")))

(deftest dirty-laundry-doppelganger-interaction
    ;; Doppelgänger interaction
    (do-game
     (new-game {:runner {:hand ["Doppelgänger" "Dirty Laundry"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Doppelgänger")
     (play-from-hand state :runner "Dirty Laundry")
     (let [credits (:credit (get-runner))]
       (click-prompt state :runner "Archives")
       (run-continue state)
       (run-continue state)
       (is (not (:run @state)))
       (is (= credits (:credit (get-runner))) "Runner hasn't gained DL credits yet")
       (click-prompt state :runner "Doppelgänger")
       (click-prompt state :runner "Yes")
       (click-prompt state :runner "Archives")
       (is (:run @state) "New run started")
       (run-continue state)
       (run-continue state)
       (is (not (:run @state)))
       (is (= (+ 5 credits) (:credit (get-runner))) "Runner hasn't gained DL credits yet"))))

(deftest diversion-of-funds-use-ability
    ;; Use ability
    (do-game
      (new-game {:runner {:deck [(qty "Diversion of Funds" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      (play-run-event state "Diversion of Funds" :hq)
      (click-prompt state :runner "Diversion of Funds")
      (is (= 9 (:credit (get-runner))) "Runner netted 4 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")
      (is (not (:run @state)) "Run finished")))

(deftest diversion-of-funds-access
    ;; Access
    (do-game
      (new-game {:runner {:deck [(qty "Diversion of Funds" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      (play-run-event state "Diversion of Funds" :hq)
      (click-prompt state :runner "Breach HQ")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "Prompt is closed")
      (is (= 4 (:credit (get-runner))) "Runner is down a credit")
      (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")
      (is (not (:run @state)) "Run finished")))

(deftest divide-and-conquer
  ;; Divide and Conquer
  (do-game
      (new-game {:corp {:deck ["Hostile Takeover" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer"]}})
      (starting-hand state :corp ["Hostile Takeover" "Ice Wall" "Ice Wall"])
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (play-run-event state "Divide and Conquer" :archives)
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "No action")
      (is (= 4 (-> (get-runner) :register :last-run core/total-cards-accessed))
          "Runner should access 2 cards in Archives, 1 in R&D, and 1 in HQ")))

(deftest divide-and-conquer-with-the-turning-wheel-counters
    ;; with The Turning Wheel counters
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer" "The Turning Wheel"]}})
      (starting-hand state :corp (concat ["Hostile Takeover"]
                                         (repeat 4 "Ice Wall")))
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (let [ttw (get-resource state 0)]
        (core/add-counter state :runner (core/make-eid state) ttw :power 4)
        (play-from-hand state :runner "Divide and Conquer")
        (run-continue state)
        (card-ability state :runner ttw 0)
        (card-ability state :runner ttw 1)
        (run-continue state)
        ;; HQ
        (is (= 1 (core/access-bonus-count state :runner :hq))
            "The Turning Wheel should provide 1 additional access on HQ")
        (dotimes [_ 2]
          (click-prompt state :runner (-> (prompt-map :runner) :choices first :value)))
        ;; R&D
        (is (= 1 (core/access-bonus-count state :runner :rd))
            "The Turning Wheel should provide 1 additional access on R&D")
        (dotimes [_ 2]
          (click-prompt state :runner "No action"))
        (is (no-prompt? state :runner) "No prompts after all accesses are complete")
        (is (= 6 (-> (get-runner) :register :last-run core/total-cards-accessed))
            "Runner should access 2 cards in Archives, 1 + 1 in R&D, and 1 + 1 in HQ"))))

(deftest divide-and-conquer-the-turning-wheel-gains-counters-after-using-d-c-issue-3810
    ;; The Turning Wheel gains counters after using D&C. Issue #3810
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer" "The Turning Wheel"]}})
      (starting-hand state :corp ["Ice Wall" "Ice Wall" "Ice Wall"])
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (let [ttw (get-resource state 0)
            counters (get-counters (refresh ttw) :power)]
        (play-run-event state "Divide and Conquer" :archives)
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (= counters (get-counters (refresh ttw) :power)) "Gains no counters")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= (+ 1 counters) (get-counters (refresh ttw) :power)) "Gains 1 counters"))))

(deftest divide-and-conquer-vs-nisei-mk-ii-issue-3803
    ;; vs Nisei Mk II. Issue #3803
    (do-game
      (new-game {:corp {:deck ["Nisei MK II" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer"]}})
      (starting-hand state :corp ["Nisei MK II" "Ice Wall" "Ice Wall"])
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (play-and-score state "Nisei MK II")
      (let [scored-nisei (get-scored state :corp 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Divide and Conquer")
        (run-continue state)
        (is (= :movement (:phase (:run @state))) "In Movement phase before Success")
        (card-ability state :corp (refresh scored-nisei) 0)
        (is (no-prompt? state :runner) "No access prompts for runner")
        (is (not (:run @state)) "Run ended by using Nisei counter")
        (is (zero? (-> (get-runner) :register :last-run core/total-cards-accessed))
            "Runner should access 0 cards"))))

(deftest divide-and-conquer-interaction-with-black-hat-issue-3798
    ;; interaction with Black Hat. Issue #3798
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer" "Black Hat"]
                          :credits 10}})
      (starting-hand state :corp (concat "Hostile Takeover" (repeat 5 "Ice Wall")))
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Black Hat")
      (click-prompt state :corp "0")
      (click-prompt state :runner "5")
      (play-run-event state "Divide and Conquer" :archives)
      (dotimes [_ 3]
        (click-prompt state :runner (-> (prompt-map :runner) :choices first :value)))
      (dotimes [_ 3]
        (click-prompt state :runner (-> (prompt-map :runner) :choices first :value)))
      (is (no-prompt? state :runner) "No prompts after all accesses are complete")
      (is (= 7 (-> (get-runner) :register :last-run core/total-cards-accessed)))))

(deftest divide-and-conquer-interaction-with-no-cards-in-archives-issue-4473
    ;; interaction with no cards in archives. Issue #4473
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover"]
                        :hand ["Hostile Takeover"]}
                 :runner {:hand ["Divide and Conquer"]}})
      (take-credits state :corp)
      (play-run-event state "Divide and Conquer" :archives)
      (is (not (no-prompt? state :runner)) "Even with no cards in Archives, there's a prompt for accessing R&D")
      (click-prompt state :runner "Steal")
      (is (not (no-prompt? state :runner)) "Even with no cards in Archives, there's a prompt for accessing HQ")
      (click-prompt state :runner "Steal")))

(deftest divide-and-conquer-interaction-with-crisium-grid-on-archives-issue-5315
    ;; interaction with Crisium Grid on archives. Issue #5315
    (do-game
     (new-game {:corp {:deck ["Hostile Takeover"]
                       :hand ["Hostile Takeover" "Crisium Grid"]}
                :runner {:hand ["Divide and Conquer"]}})
     (play-from-hand state :corp "Crisium Grid" "Archives")
     (rez state :corp (get-content state :archives 0))
     (take-credits state :corp)
     (play-run-event state "Divide and Conquer" :archives)
     (click-prompt state :runner "No action")
     (is (no-prompt? state :runner) "There's no prompt for accessing R&D")))

(deftest drive-by
  ;; Drive By - Expose card in remote server and trash if asset or upgrade
  (do-game
      (new-game {:corp {:deck [(qty "Eve Campaign" 2)
                               "Product Placement"
                               "Project Atlas"]}
                 :runner {:deck [(qty "Drive By" 2)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Product Placement" "HQ")
      (take-credits state :corp)
      (let [eve1 (get-content state :remote1 0)
            eve2 (get-content state :remote2 0)
            atl (get-content state :remote3 0)
            pp (get-content state :hq 0)]
        (rez state :corp eve1)
        (play-from-hand state :runner "Drive By")
        (click-card state :runner pp)
        (is (= 1 (count (get-in @state [:corp :servers :hq :content])))
            "Upgrades in root of central servers can't be targeted")
        (click-card state :runner (refresh eve1))
        (is (= 1 (count (get-in @state [:corp :servers :remote1 :content])))
            "Rezzed cards can't be targeted")
        (click-card state :runner eve2)
        (is (= 2 (:click (get-runner))) "Spent 2 clicks")
        (is (and (= 1 (count (:discard (get-corp))))
                 (= 5 (:credit (get-runner))))
            "Eve trashed at no cost")
        (is (nil? (get-in @state [:corp :servers :remote2 :content])) "Server 2 no longer exists")
        (play-from-hand state :runner "Drive By")
        (click-card state :runner atl)
        (is (zero? (:click (get-runner))) "Runner has 0 clicks left")
        (is (= 1 (count (get-in @state [:corp :servers :remote3 :content])))
            "Project Atlas not trashed from Server 3"))))

(deftest drive-by-psychic-field-trashed-after-psi-game-issue-2127
    ;; Psychic Field trashed after psi game. Issue #2127.
    (do-game
      (new-game {:corp {:deck ["Psychic Field"]}
                 :runner {:deck [(qty "Drive By" 3)]}})
      (play-from-hand state :corp "Psychic Field" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Drive By")
      (click-card state :runner (get-content state :remote1 0))
      (click-prompt state :corp "0 [Credits]")
      (click-prompt state :runner "1 [Credits]")
      (is (empty? (get-content state :remote1)) "Psychic Field trashed")))

(deftest drive-by-turn-on-reprisal-cards-issue-3755
    ;; Turn on reprisal cards. Issue #3755.
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Drive By"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Drive By")
      (click-card state :runner "PAD Campaign")
      (is (empty? (get-content state :remote1)) "PAD Campaign trashed")
      (is (get-in (get-runner) [:register :trashed-card]) "Registered as runner trashed a card")))

(deftest early-bird
  ;; Early Bird - Priority, make a run and gain a click
  (do-game
    (new-game {:runner {:deck ["Early Bird"]}})
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (play-from-hand state :runner "Early Bird")
    (is (= 3 (:click (get-runner))) "Card not played, Early Bird priority restriction")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Early Bird")
    (click-prompt state :runner "Archives")
    (is (= 4 (:click (get-runner))) "Early Bird gains click")))

(deftest easy-mark
  ;; Easy Mark
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Easy Mark"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Easy Mark")
      (is (= (+ 3 credits) (:credit (get-runner))) "Runner has gained 3 credits"))))

(deftest embezzle
  ;; Embezzle
  (do-game
      (new-game {:corp {:deck ["Ice Wall" "Archer"]}
                 :runner {:deck ["Embezzle"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-run-event state "Embezzle" :hq)
      (click-prompt state :runner "ICE")
      (is (= 2 (count (:discard (get-corp)))) "HQ card trashed")
      (is (= 12 (:credit (get-runner))))))

(deftest embezzle-check-that-trashed-cards-are-trashed-face-up
    ;; Check that trashed cards are trashed face-up
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Embezzle"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-run-event state "Embezzle" :hq)
      (click-prompt state :runner "ICE")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed")
      (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen")
      (is (= 8 (:credit (get-runner))))))

(deftest emergency-shutdown
  ;; Emergency Shutdown
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}
               :runner {:hand ["Emergency Shutdown"]}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (let [iw (get-ice state :remote1 0)]
      (rez state :corp iw)
      (is (rezzed? (refresh iw)) "Ice Wall is rezzed")
      (take-credits state :corp)
      (run-empty-server state :archives)
      (play-from-hand state :runner "Emergency Shutdown")
      (is (no-prompt? state :runner) "Runner has no derez prompt")
      (run-empty-server state :hq)
      (play-from-hand state :runner "Emergency Shutdown")
      (is (not (no-prompt? state :runner)) "Runner has a derez prompt")
      (click-card state :runner (refresh iw))
      (is (not (rezzed? (refresh iw))) "Ice Wall is derezzed"))))

(deftest emergent-creativity
  ;; Emergent Creativty - Double, discard programs/hardware from grip, install from heap
  (do-game
      (new-game {:runner {:deck ["Emergent Creativity" "Paperclip"
                                 "Heartbeat" "Gordian Blade" "Test Run"]}})
      (starting-hand state :runner ["Emergent Creativity" "Heartbeat" "Gordian Blade" "Test Run"])
      (take-credits state :corp)
      (play-from-hand state :runner "Emergent Creativity")
      (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
      (click-card state :runner (find-card "Gordian Blade" (:hand (get-runner))))
      (click-prompt state :runner "Done")
      (click-prompt state :runner (find-card "Paperclip" (:deck (get-runner))))
      (is (= 3 (:credit (get-runner))) "Offset cost of installing Paperclip")
      (is (zero? (count (:deck (get-runner)))) "Installed from heap")
      (is (= 3 (count (:discard (get-runner)))) "Discard is 3 cards - EC, Heartbeat, GB")
      (is (= 2 (:click (get-runner))) "Emergent Creativity is a Double event")))

(deftest emergent-creativity-resource-not-shown-in-prompt
    ;; Resource not shown in prompt
    (do-game
      (new-game {:runner {:deck ["Emergent Creativity" "Heartbeat" "Gordian Blade" "Liberated Account"]}})
      (starting-hand state :runner ["Emergent Creativity" "Heartbeat" "Gordian Blade" ])
      (take-credits state :corp)
      (play-from-hand state :runner "Emergent Creativity")
      (click-card state :runner "Heartbeat")
      (click-card state :runner "Gordian Blade")
      (is (= ["Cancel"] (prompt-buttons :runner)) "Liberated Account shouldn't be shown in prompt (only Cancel)")
      (click-prompt state :runner "Cancel")))

(deftest emergent-creativity-should-trash-cards-before-choosing-which-to-install-issue-4939
    ;; Should trash cards before choosing which to install. Issue #4939
    (do-game
      (new-game {:runner {:hand ["Emergent Creativity" "Buffer Drive" "Engolo"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Buffer Drive")
      (play-from-hand state :runner "Emergent Creativity")
      (is (changed? [(:credit (get-runner)) 0]
            (is (= "Emergent Creativity" (:title (:card (prompt-map :runner))))
            "Trash prompt from Emergent Creativity")
            (click-card state :runner "Engolo")
            (is (= "Buffer Drive" (:title (:card (prompt-map :runner))))
            "Buffer Drive prompt interrupts Emergent Creativity")
            (click-prompt state :runner "Engolo")
            (is (= "Emergent Creativity" (:title (:card (prompt-map :runner))))
            "Install prompt from Emergent Creativity")
            (click-prompt state :runner "Engolo")
            (is (= "Engolo" (:title (get-program state 0))) "Engolo is installed"))
          "Installing Engolo costs no credits")))

(deftest employee-strike-vs-blue-sun-suppress-step-1-2
    ;; vs Blue Sun, suppress Step 1.2
    (do-game
      (new-game {:corp {:id "Blue Sun: Powering the Future"
                        :deck ["Ice Wall"]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))

(deftest employee-strike-vs-pu-philotic
    ;; vs PU/Philotic
    (do-game
      (new-game {:corp {:id "Jinteki: Potential Unleashed"
                        :deck ["Philotic Entanglement" (qty "Braintrust" 2)]}
                 :runner {:deck [(qty "Employee Strike" 10)]}})
      (play-from-hand state :corp "Braintrust" "New remote")
      (play-from-hand state :corp "Braintrust" "New remote")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Steal")
      (run-empty-server state "Server 2")
      (click-prompt state :runner "Steal")
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (play-from-hand state :corp "Philotic Entanglement" "New remote")
      (is (changed? [(count (:deck (get-runner))) -1]
            (score-agenda state :corp (get-content state :remote3 0)))
          "PU trashes top card of the stack")
      (is (= 4 (count (:discard (get-runner))))
          "Discard is 4 cards - 2 from Philotic, 1 EStrike, 1 from PU mill")))

(deftest employee-strike-vs-pe-philotic
  ;; vs PU/Philotic
  (do-game
    (new-game {:corp {:id "Jinteki: Personal Evolution"
                      :deck ["Sting!" (qty "Braintrust" 2)]}
               :runner {:hand [(qty "Employee Strike" 5)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Employee Strike")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (take-credits state :runner)
    (play-and-score state "Sting!")
    (is (no-prompt? state :corp) "No prompt to interact because PE is dead")
    (is (= 2 (count (:discard (get-runner))))
        "Discard is 2 cards - 1 from String1, 1 EStrike")))


(deftest en-passant
  ;; En Passant
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Ice Wall" 2)]}
               :runner {:hand [(qty "En Passant" 2)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "New remote")
    (take-credits state :corp)
    (let [iw (get-ice state :hq 0)
          iw2 (get-ice state :remote1 0)]
      (run-on state :hq)
      (run-continue state)
      (run-continue state)
      (play-from-hand state :runner "En Passant")
      (click-card state :runner (refresh iw2))
      (is (refresh iw2) "Ice Wall on the remote hasn't been trashed")
      (rez state :corp iw)
      (is (refresh iw) "Ice Wall on HQ hasn't been trashed as it's rezzed")
      (derez state :corp iw)
      (click-card state :runner (refresh iw))
      (is (not (refresh iw)) "Ice Wall on HQ has been trashed"))))

(deftest encore
  ;; Encore - Run all 3 central servers successfully to take another turn.  Remove Encore from game.
  (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck ["Encore"]}})
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Encore")
      (is (= 1 (count (:rfg (get-runner)))) "Encore removed from game")
      (take-credits state :runner)
      (take-credits state :runner)
      ; only get one extra turn
      (take-credits state :runner)
      (is (= 9 (:credit (get-runner))))))

(deftest encore-2-encores-in-a-5-click-turn-results-in-2-extra-turns
    ;; 2 encores in a 5 click turn results in 2 extra turns
    (do-game
      (new-game {:corp {:deck ["Hedge Fund"]}
                 :runner {:deck [(qty "Encore" 2)]}})
      (play-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (core/gain state :runner :click 1)
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Encore")
      (play-from-hand state :runner "Encore")
      (is (= 2 (count (:rfg (get-runner)))) "2 Encores removed from game")
      (take-credits state :runner)
      (take-credits state :runner)
      ;; Two extra turns
      (take-credits state :runner)
      (is (= 13 (:credit (get-runner))))))

(deftest escher
  ;; Escher
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Enigma" "IP Block" "Data Raven"]
                      :credits 20}
               :runner {:hand ["Escher"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Enigma" "New remote")
    (play-from-hand state :corp "IP Block" "New remote")
    (play-from-hand state :corp "Data Raven" "New remote")
    (rez state :corp (get-ice state :remote1 0))
    (rez state :corp (get-ice state :remote2 0))
    (rez state :corp (get-ice state :remote3 0))
    (rez state :corp (get-ice state :remote4 0))
    (take-credits state :corp)
    (play-run-event state "Escher" :hq)
    (click-card state :runner "Ice Wall")
    (click-card state :runner "IP Block")
    (click-card state :runner "Enigma")
    (click-card state :runner "Data Raven")
    (click-prompt state :runner "Done")
    (is (= "IP Block" (:title (get-ice state :remote1 0))) "IP Block has moved")
    (is (= "Data Raven" (:title (get-ice state :remote2 0))) "Data Raven has moved")
    (is (= "Ice Wall" (:title (get-ice state :remote3 0))) "Ice Wall has moved")
    (is (= "Enigma" (:title (get-ice state :remote4 0))) "Enigma has moved")))

(deftest eureka
  ;; Eureka! - Install the program but trash the event
  (do-game
    (new-game {:runner {:deck [(qty "Eureka!" 2) "Torch" "Sure Gamble"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 1)
    (core/move state :runner (find-card "Torch" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Eureka!")
    (click-prompt state :runner "Yes")
    (is (= 3 (:credit (get-runner))))
    (is (= 1 (count (get-program state))))
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Eureka!")
    (is (zero? (:credit (get-runner))))
    (is (= 3 (count (:discard (get-runner)))))))

(deftest exclusive-party
  ;; Exclusive Party - Draw 1 card. Gain 1c for each copy in the heap.
  (do-game
    (new-game {:runner {:deck [(qty "Exclusive Party" 4)]}})
    (take-credits state :corp)
    (is (zero? (count (:discard (get-runner)))) "Heap is empty")
    (play-from-hand state :runner "Exclusive Party")
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (is (= 1 (count (:discard (get-runner)))) "One copy of EP in heap")
    (play-from-hand state :runner "Exclusive Party")
    (is (= 6 (:credit (get-runner))) "1 credit gained")
    (is (= 2 (count (:discard (get-runner)))) "Two copies of EP in heap")
    (play-from-hand state :runner "Exclusive Party")
    (is (= 8 (:credit (get-runner))) "2 credits gained")))

(deftest executive-wiretaps
  ;; Executive Wiretaps
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover" "PAD Campaign" "Ice Wall" "Hedge Fund" "Cayambe Grid"]}
               :runner {:hand ["Executive Wiretaps"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Executive Wiretaps")
    (is (last-log-contains? state (str "Runner uses Executive Wiretaps to reveal "
                                       "Cayambe Grid, Hedge Fund, Hostile Takeover, Ice Wall, and PAD Campaign from HQ.")))))

(deftest exploit
  ;; Exploit
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Enigma" "Hunter"]}
               :runner {:hand ["Exploit"]}})
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Enigma" "New remote")
    (play-from-hand state :corp "Hunter" "New remote")
    (let [iw (get-ice state :remote1 0)
          enigma (get-ice state :remote2 0)
          hunter (get-ice state :remote3 0)]
      (rez state :corp iw)
      (rez state :corp enigma)
      (rez state :corp hunter)
      (take-credits state :corp)
      (play-from-hand state :runner "Exploit")
      (is (no-prompt? state :runner) "No prompt as runner has fulfilled req yet")
      (run-empty-server state :archives)
      (run-empty-server state :rd)
      (click-prompt state :runner "No action")
      (run-empty-server state :hq)
      (play-from-hand state :runner "Exploit")
      (is (= :select (prompt-type :runner)) "Runner has Exploit select prompt")
      (click-card state :runner (refresh iw))
      (click-card state :runner (refresh enigma))
      (click-card state :runner (refresh hunter))
      (is (not (rezzed? (refresh iw))) "Ice Wall is derezzed")
      (is (not (rezzed? (refresh enigma))) "Enigma is derezzed")
      (is (not (rezzed? (refresh hunter))) "Hunter is derezzed"))))

(deftest exploratory-romp
  ;; Exploratory Romp - Remove advancements from card instead of accessing
  (do-game
      (new-game {:corp {:deck ["TGTBT"]}
                 :runner {:deck ["Exploratory Romp"]}})
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [tg (get-content state :remote1 0)]
        (advance state tg 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Exploratory Romp")
        (click-prompt state :runner "Server 1")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "2")
        (click-card state :runner (refresh tg))
        (is (zero? (count-tags state)) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed"))))

(deftest exploratory-romp-don-t-remove-more-than-the-existing-number-of-advancement-tokens
    ;; Don't remove more than the existing number of advancement tokens
    (do-game
      (new-game {:corp {:deck ["TGTBT"]}
                 :runner {:deck ["Exploratory Romp"]}})
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [tg (get-content state :remote1 0)]
        (advance state tg 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Exploratory Romp")
        (click-prompt state :runner "Server 1")
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "3")
        (click-card state :runner (refresh tg))
        (is (zero? (count-tags state)) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed"))))

(deftest express-delivery
  ;; Express Delivery
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck ["Easy Mark" "Sure Gamble" "Infiltration" "Magnum Opus"]
                        :hand ["Express Delivery"]}})
    (take-credits state :corp)
    (let [num-shuffles (count (core/turn-events state :runner :runner-shuffle-deck))]
      (play-from-hand state :runner "Express Delivery")
      (is (= 4 (count (prompt-buttons :runner))) "Runner sees 4 cards")
      (click-prompt state :runner "Magnum Opus")
      (is (= (inc num-shuffles) (count (core/turn-events state :runner :runner-shuffle-deck)))
                "Runner should shuffle the stack")
      (is (= ["Magnum Opus"] (map :title (:hand (get-runner)))) "Magnum Opus is in the hand"))))

(deftest eye-for-an-eye
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["Eye for an Eye" (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Eye for an Eye")
    (is (not (:run @state)) "Cannot play Eye for an Eye when tagged")
    (remove-tag state :runner)
    (play-run-event state "Eye for an Eye" :hq)
    (click-prompt state :runner "[Eye for an Eye] Trash 1 card from your hand: Trash card")
    (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
    (click-prompt state :runner "[Eye for an Eye] Trash 1 card from your hand: Trash card")
    (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
    (is (not (:run @state)) "Run ended")
    (is (= 1 (count-tags state)))
    (is (= 2 (count (:discard (get-corp)))))
    (is (= 3 (count (:discard (get-runner)))) "Whole world is blind")))

(deftest falsified-credentials
  ;; Falsified Credentials - Expose card in remote
  ;; server and correctly guess its type to gain 5 creds
  (do-game
      (new-game {:corp {:deck [(qty "Eve Campaign" 2)
                               (qty "Product Placement" 2)
                               "Project Atlas"]}
                 :runner {:deck [(qty "Falsified Credentials" 3)]}})
      (core/gain state :corp :click 2)
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Eve Campaign" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Product Placement" "HQ")
      (play-from-hand state :corp "Product Placement" "Server 3")
      (take-credits state :corp)
      (let [eve1 (get-content state :remote1 0)
            eve2 (get-content state :remote2 0)
            atl (get-content state :remote3 0)
            pp1 (get-content state :hq 0)
            pp2 (get-content state :remote3 1)]
        (rez state :corp eve1)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Asset")
        (click-card state :runner (refresh eve1))
        (is (= 4 (:credit (get-runner)))
            "Rezzed cards can't be targeted")
        (click-card state :runner eve2)
        (is (= 3 (:click (get-runner))) "Spent 1 click")
        (is (= 9 (:credit (get-runner))) "Gained 5 creds for guessing asset correctly")
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Upgrade")
        (click-card state :runner pp1)
        (is (= 8 (:credit (get-runner))) "Can't target cards in centrals")
        (click-card state :runner pp2)
        (is (= 13 (:credit (get-runner)))
            "Gained 5 creds for guessing upgrade correctly, even if server contains non-upgrade as well")
        (rez state :corp pp2)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (is (= 17 (:credit (get-runner)))
            "Gained 5 credits for guessing agenda correctly, even with rezzed card in server"))))

(deftest falsified-credentials-vs-zaibatsu-loyalty-if-falsified-credentials-fails-to-expose-it-grants-no-credits
    ;; vs Zaibatsu Loyalty. If Falsified Credentials fails to expose, it grants no credits.
    (do-game
      (new-game {:corp {:deck ["Zaibatsu Loyalty" "Project Atlas"]}
                 :runner {:deck [(qty "Falsified Credentials" 2)]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Zaibatsu Loyalty" "New remote")
      (take-credits state :corp)
      (let [atl (get-content state :remote1 0)
            zaibatsu (get-content state :remote2 0)]
        (rez state :corp zaibatsu)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (click-prompt state :corp "Allow 1 card to be exposed")
        (is (= 9 (:credit (get-runner))) "An unprevented expose gets credits")
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (click-prompt state :corp "1 [Credit]: Zaibatsu Loyalty")
        (is (= 8 (:credit (get-runner))) "A prevented expose does not"))))

(deftest fear-the-masses
  ;; Fear the Masses
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 50)]
                        :hand [(qty "Hedge Fund" 5)]}
                :runner {:hand [(qty "Fear the Masses" 6)]}})
      (take-credits state :corp)
      (let [discard (count (:discard (get-corp)))]
        (play-run-event state "Fear the Masses" :hq)
        (is (= (inc discard) (count (:discard (get-corp)))) "Corp trashes 1 card"))
      (let [discard (count (:discard (get-corp)))]
        (click-card state :runner (nth (:hand (get-runner)) 0))
        (click-card state :runner (nth (:hand (get-runner)) 1))
        (click-card state :runner (nth (:hand (get-runner)) 2))
        (click-card state :runner (nth (:hand (get-runner)) 3))
        (click-card state :runner (nth (:hand (get-runner)) 4))
        (is (= (+ discard 5) (count (:discard (get-corp)))) "Corp trashes 5 additional card"))))

(deftest fear-the-masses-trebuchet-firing-incorrectly-prevents-fear-the-masses-effect-from-taking-place-issue-5294
  ;; Trebuchet firing incorrectly prevents Fear The Masses effect from taking place. Issue #5294
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 50)]
                      :hand ["Trebuchet"]}
               :runner {:hand [(qty "Fear the Masses" 6)]}})
    (play-from-hand state :corp "Trebuchet" "HQ")
    (take-credits state :corp)
    (let [discard (count (:discard (get-corp)))
          treb (get-ice state :hq 0)]
      (play-from-hand state :runner "Fear the Masses")
      (run-continue state)
      (rez state :corp treb)
      (run-continue state)
      (card-subroutine state :corp treb 0)
      (card-subroutine state :corp treb 1)
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (run-continue state)
      (run-continue state)
      (is (= (inc discard) (count (:discard (get-corp)))) "Corp trashes 1 card"))
    (let [discard (count (:discard (get-corp)))]
      (click-card state :runner (nth (:hand (get-runner)) 0))
      (click-card state :runner (nth (:hand (get-runner)) 1))
      (click-card state :runner (nth (:hand (get-runner)) 2))
      (click-card state :runner (nth (:hand (get-runner)) 3))
      (click-card state :runner (nth (:hand (get-runner)) 4))
      (is (= (+ discard 5) (count (:discard (get-corp)))) "Corp trashes 5 additional card"))))

(deftest feint
  ;; Feint - bypass 2 pieces of ice on HQ, but access no cards
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Tollbooth"]
                      :credits 20}
               :runner {:deck ["Feint"]}})
    (play-from-hand state :corp "Tollbooth" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :hq 1))
    (take-credits state :corp)
    (play-from-hand state :runner "Feint")
    (run-continue state)
    (run-continue state :movement) ;; Bypass Ice Wall
    (run-continue state :approach-ice)
    (let [credits (:credit (get-runner))]
      (run-continue state :movement) ;; Bypass Tollbooth
      (is (= credits (:credit (get-runner))) "Runner doesn't lose any credits from Tollbooth"))
    (run-continue state :success)
    (click-prompt state :runner "OK")
    (is (not (:run @state)) "Run is over")))

(deftest finality
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 5)]
                      :hand [(qty "Quandary" 5)]}
               :runner {:hand ["Finality" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-run-event state "Finality" :rd)
    (is (= 1 (count (:discard (get-runner)))))
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "1st quandary")
    (click-prompt state :runner "No action")
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "2nd quandary")
    (click-prompt state :runner "No action")
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "3rd quandary")
    (click-prompt state :runner "No action")
    (is (= "You accessed Quandary." (:msg (prompt-map :runner))) "4rd quandary")
    (click-prompt state :runner "No action")
    (is (not (:run @state)))))

(deftest fisk-investment-seminar
  ;; Fisk Investment Seminar
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Fisk Investment Seminar"]}})
    (take-credits state :corp)
    (let [c-hand (count (:hand (get-corp)))
          ;; -1 because of playing FIS
          r-hand (dec (count (:hand (get-runner))))]
      (play-from-hand state :runner "Fisk Investment Seminar")
      (is (= (+ 3 c-hand) (count (:hand (get-corp)))) "Corp draws 3 cards")
      (is (= (+ 3 r-hand) (count (:hand (get-runner)))) "Runner draws 3 cards"))))

(deftest forged-activation-orders-corp-chooses-to-trash-the-ice
    ;; Corp chooses to trash the ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Forged Activation Orders"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Forged Activation Orders")
      (click-card state :runner "Ice Wall")
      (click-prompt state :corp "Trash ice protecting HQ at position 0")
      (is (= "Ice Wall" (:title (get-discarded state :corp 0))) "Ice Wall is trashed")))

(deftest forged-activation-orders-corp-chooses-to-rez-the-ice
    ;; Corp chooses to rez the ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Forged Activation Orders"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Forged Activation Orders")
      (click-card state :runner "Ice Wall")
      (click-prompt state :corp "Rez ice protecting HQ at position 0")
      (is (rezzed? (get-ice state :hq 0)) "Ice Wall is rezzed")))

(deftest forged-activation-orders-corp-cannot-rez-the-ice
    ;; Corp cannot rez the ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Chiyashi"]}
                 :runner {:hand ["Forged Activation Orders"]}})
      (play-from-hand state :corp "Chiyashi" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Forged Activation Orders")
      (click-card state :runner "Chiyashi")
      (is (= ["Trash ice protecting HQ at position 0"]
           (mapv :value (:choices (prompt-map :corp)))))))

(deftest forked
  ;; Forked
  (do-game
    (new-game {:corp {:hand ["Rototurret"]}
               :runner {:hand ["Forked" "Mimic"]}})
    (play-from-hand state :corp "Rototurret" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 2)
    (play-from-hand state :runner "Mimic")
    (play-from-hand state :runner "Forked")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "Trash a program")
    (click-prompt state :runner "End the run")
    (run-continue state)
    (run-continue state)
    (is (= 1 (count (:discard (get-corp)))) "Rototurret is trashed")))

(deftest frame-job
  ;; Frame Job
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover"]}
               :runner {:hand ["Frame Job"]}})
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Frame Job")
    (let [bp (count-bad-pub state)]
      (click-prompt state :runner "Hostile Takeover")
      (is (= (inc bp) (count-bad-pub state)) "Corp gains 1 bp")
      (is (not (get-scored state :runner 0)) "Hostile Takeover is forfeit"))))

(deftest frantic-coding
  ;; Frantic Coding - Install 1 program, other 9 cards are trashed
  (do-game
      (new-game {:runner {:hand ["Frantic Coding"]
                          :deck ["Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (is (= ["Corroder" "Magnum Opus" "Done"] (prompt-titles :runner)) "No Torch in list because can't afford")
      (is (= 2 (:credit (get-runner))))
      (is (zero? (count (:discard (get-runner)))))
      (click-prompt state :runner "Magnum Opus")
      (is (= 1 (count (get-program state))))
      (is (= 2 (:credit (get-runner))) "Magnum Opus installed for free")
      (is (= 10 (count (:discard (get-runner)))))))

(deftest frantic-coding-don-t-install-anything-all-10-cards-are-trashed
    ;; Don't install anything, all 10 cards are trashed
    (do-game
      (new-game {:runner {:deck ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]}})
      (starting-hand state :runner ["Frantic Coding"])
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (is (= ["Corroder" "Magnum Opus" "Done"] (prompt-titles :runner)) "No Torch in list because can't afford")
      (is (zero? (count (:discard (get-runner)))))
      (click-prompt state :runner "Done")
      (is (zero? (count (get-program state))))
      (is (= 11 (count (:discard (get-runner)))))))

(deftest freedom-through-equality
  ;; Move Freedom Through Equality to runner score on another steal
  ;; Check only one current used
  (do-game
    (new-game {:corp {:deck [(qty "Project Beale" 2)]}
               :runner {:deck ["Street Peddler" (qty "\"Freedom Through Equality\"" 3) "Sure Gamble"]}})
    (starting-hand state :runner ["Street Peddler"
                                  "\"Freedom Through Equality\""
                                  "\"Freedom Through Equality\""
                                  "Sure Gamble"])
    (play-from-hand state :corp "Project Beale" "New remote")
    (play-from-hand state :corp "Project Beale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Freedom Through Equality not moved from Peddler to score area")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "\"Freedom Through Equality\"")
    (play-from-hand state :runner "\"Freedom Through Equality\"")
    (run-empty-server state "Server 2")
    (click-prompt state :runner "Steal")
    (is (= 3 (count (:scored (get-runner)))) "Freedom Through Equality moved to score area")
    (is (= 5 (:agenda-point (get-runner))) "Freedom Through Equality for 1 agenda point")))

(deftest freelance-coding-contract
  ;; Freelance Coding Contract - Gain 2 credits per program trashed from Grip
  (do-game
    (new-game {:runner {:deck ["Freelance Coding Contract"
                               "Paricia" "Cloak" "Inti"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Freelance Coding Contract")
    (click-card state :runner (find-card "Cloak" (:hand (get-runner))))
    (click-card state :runner (find-card "Paricia" (:hand (get-runner))))
    (click-card state :runner (find-card "Inti" (:hand (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 3 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "3 programs in Heap")
    (is (= 11 (:credit (get-runner))) "Gained 6 credits from 3 trashed programs")))

(deftest game-day
  ;; Game Day - draw until at handsize
  (do-game
    (new-game {:runner {:deck [(qty "Game Day" 3)
                               (qty "Public Sympathy" 3)
                               (qty "Sure Gamble" 3)
                               (qty "Easy Mark" 3)]}})
    (take-credits state :corp)
    ;; move needed cards to hand -- in case they were not drawn
    (core/move state :runner (find-card "Game Day" (:deck (get-runner))) :hand)
    (core/move state :runner (find-card "Public Sympathy" (:deck (get-runner))) :hand)
    (play-from-hand state :runner "Public Sympathy")
    (is (= 7 (hand-size :runner)) "Runner hand size is 7")
    (play-from-hand state :runner "Game Day")
    (is (= 7 (count (:hand (get-runner)))) "Drew up to 7 cards")))

(deftest glut-cipher
  ;; Glut Cipher
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Fire Wall" "Wraparound"]
                      :discard ["Enigma" "Rototurret" "Beanstalk Royalties"]}
               :runner {:hand [(qty "Glut Cipher" 3)]}})
    (take-credits state :corp)
    (is (= 3 (count (:discard (get-corp)))) "There are 3 cards in Archives")
    (play-from-hand state :runner "Glut Cipher")
    (run-continue state)
    (is (:run @state) "There is a run happening")
    (run-continue state)
    (is (= 3 (count (:discard (get-corp)))) "Glut Cipher did not fire when < 5 cards")
    (is (zero? (count (filter :seen (:discard (get-corp))))) "There are no faceup cards in Archives")
    (run-empty-server state "Archives")
    (is (= 3 (count (filter :seen (:discard (get-corp))))) "There are 3 faceup cards in Archives")
    (trash-from-hand state :corp "Ice Wall")
    (trash-from-hand state :corp "Fire Wall")
    (trash-from-hand state :corp "Wraparound")
    (is (= 3 (count (filter :seen (:discard (get-corp))))) "There are 3 faceup cards in Archives")
    (is (= 6 (count (:discard (get-corp)))) "There are 6 cards in Archives")
    (play-run-event state "Glut Cipher" :archives)
    (click-card state :corp "Ice Wall")
    (click-card state :corp "Fire Wall")
    (click-card state :corp "Wraparound")
    (is (not (no-prompt? state :corp)) "There is still a prompt")
    (click-card state :corp "Enigma")
    (click-card state :corp "Rototurret")
    (is (no-prompt? state :corp) "Selecting 5 cards closed prompt")
    (let [discard (:discard (get-corp))]
      (is (find-card "Beanstalk Royalties" discard) "Beanstalk Royalties is still in Archives")
      (is (= 6 (count discard)) "There are 6 cards in Archives")
      (is (= 1 (count (filter :seen discard))) "There is 1 seen card in Archives"))
    (is (zero? (count (:hand (get-corp)))) "There are no cards in hand")))

(deftest government-investigations
  ;; Government Investigations
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Cerebral Cast"]}
               :runner {:hand ["Government Investigations" "Push Your Luck"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Government Investigations")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (play-from-hand state :corp "Cerebral Cast")
    (is (= ["Roll a d6" "0 [Credits]" "1 [Credits]"] (prompt-buttons :corp)))
    (is (= ["Roll a d6" "0 [Credits]" "1 [Credits]"] (prompt-buttons :runner)))
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (take-credits state :corp)
    (play-from-hand state :runner "Push Your Luck")
    (is (= ["0" "1" "3" "4" "5"] (prompt-buttons :runner)) "Runner can't choose 2")))

(deftest guinea-pig
  ;; Guinea Pig
  (do-game
    (new-game {:runner {:deck ["Guinea Pig" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Guinea Pig")
    (is (= 11 (:credit (get-runner))) "Gained +6 credits from playing Guinea Pig")
    (is (empty? (:hand (get-runner))) "No cards left in grip, trashed all cards due to Guinea Pig")
    (is (= 4 (count (:discard (get-runner)))) "3 cards trashed from Guinea Pig + Guinea Pig itself")))

(deftest hacktivist-meeting
  ;; Hacktivist Meeting
  ;; Trash a random card from corp hand while active
  ;; Make sure it is not active when hosted on Peddler
  (do-game
    (new-game {:corp {:deck [(qty "Jeeves Model Bioroids" 2)
                             (qty "Jackson Howard" 2)]
                      :hand ["Jeeves Model Bioroids" "Jackson Howard" "Sundew"]
                      :credits 10}
               :runner {:hand ["Street Peddler" "Hacktivist Meeting"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (take-credits state :runner)
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (play-from-hand state :corp "Sundew" "New remote")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)
          sundew (get-content state :remote3 0)]
      (rez state :corp jeeves)
      (is (zero? (count (:discard (get-corp)))) "Nothing discarded to rez Jeeves - Hacktivist not active")
      (take-credits state :corp)
      (play-from-hand state :runner "Hacktivist Meeting")
      (rez state :corp jackson)
      (is (= 1 (count (:discard (get-corp)))) "Card discarded to rez Jackson - Hacktivist active")
      (rez state :corp sundew {:expect-rez false})
      (is (not (rezzed? (refresh sundew))) "Sundew is not rezzed as corp has no cards in hand")
      (is (= "Unable to pay for Sundew." (-> @state :corp :toast first :msg)) "Corp gets the correct toast"))))

(deftest harmony-ar-therapy
  ;; Harmony AR Therapy
  (do-game
      (new-game {:runner {:hand [(qty "Find the Truth" 2) (qty "Astrolabe" 2) (qty "Bankroll" 2) (qty "Chameleon" 2) (qty "Dirty Laundry" 2) (qty "Equivocation" 2)]
                          :deck ["Harmony AR Therapy"]}})
      (take-credits state :corp)
      (dotimes [_ 12] (core/move state :runner (first (:hand (get-runner))) :discard))
      (draw state :runner 1)
      (play-from-hand state :runner "Harmony AR Therapy")
      (is (= 6 (count (prompt-buttons :runner))) "Cards are shown distinctly")
      (click-prompt state :runner "Dirty Laundry")
      (click-prompt state :runner "Astrolabe")
      (click-prompt state :runner "Bankroll")
      (click-prompt state :runner "Chameleon")
      (click-prompt state :runner "Equivocation")
      (is (= 12 (count (:discard (get-runner)))) "12 cards in discard (HART still in play area)")
      (is (= 0 (count (:deck (get-runner)))) "No cards in stack")
      (is (= 0 (count (:rfg (get-runner)))) "Nothing removed from game")
      (click-prompt state :runner "OK")
      (is (= 7 (count (:discard (get-runner)))) "7 cards in discard")
      (is (= 5 (count (:deck (get-runner)))) "5 cards shuffled back into stack")
      (is (= 1 (count (:rfg (get-runner)))) "HART removed from game")))

(deftest harmony-ar-therapy-can-play-with-empty-heap
    ;; Can play with empty heap
    (do-game
      (new-game {:runner {:hand ["Harmony AR Therapy"]
                          :deck ["Sure Gamble"]}})
      (take-credits state :corp)
      (is (zero? (count (:discard (get-runner)))) "Heap is empty")
      (play-from-hand state :runner "Harmony AR Therapy")
      (is (= 1 (count (:rfg (get-runner)))) "HART removed from game")))

(deftest harmony-ar-therapy-shuffle-back-less-than-5-cards
    ;; Shuffle back less than 5 cards
    (do-game
      (new-game {:runner {:hand [(qty "Find the Truth" 2) (qty "Astrolabe" 2) (qty "Bankroll" 2) (qty "Chameleon" 2) (qty "Dirty Laundry" 2) (qty "Equivocation" 2)]
                          :deck ["Harmony AR Therapy"]}})
      (take-credits state :corp)
      (dotimes [_ 12] (core/move state :runner (first (:hand (get-runner))) :discard))
      (draw state :runner 1)
      (play-from-hand state :runner "Harmony AR Therapy")
      (is (= 6 (count (prompt-buttons :runner))) "Cards are shown distinctly")
      (click-prompt state :runner "Dirty Laundry")
      (click-prompt state :runner "Astrolabe")
      (click-prompt state :runner "Bankroll")
      (click-prompt state :runner "Done")
      (is (= 12 (count (:discard (get-runner)))) "12 cards in discard (HART still in play area)")
      (is (= 0 (count (:deck (get-runner)))) "No cards in stack")
      (is (= 0 (count (:rfg (get-runner)))) "Nothing removed from game")
      (click-prompt state :runner "OK")
      (is (= 9 (count (:discard (get-runner)))) "9 cards in discard")
      (is (= 3 (count (:deck (get-runner)))) "3 cards shuffled back into stack")
      (is (= 1 (count (:rfg (get-runner)))) "HART removed from game")))

(deftest harmony-ar-therapy-start-over-function
    ;; Start over function
    (do-game
      (new-game {:runner {:hand [(qty "Find the Truth" 2) (qty "Astrolabe" 2) (qty "Bankroll" 2) (qty "Chameleon" 2) (qty "Dirty Laundry" 2) (qty "Equivocation" 2)]
                          :deck ["Harmony AR Therapy"]}})
      (take-credits state :corp)
      (dotimes [_ 12] (core/move state :runner (first (:hand (get-runner))) :discard))
      (draw state :runner 1)
      (play-from-hand state :runner "Harmony AR Therapy")
      (is (= 6 (count (prompt-buttons :runner))) "Cards are shown distinctly")
      (click-prompt state :runner "Dirty Laundry")
      (click-prompt state :runner "Astrolabe")
      (click-prompt state :runner "Bankroll")
      (click-prompt state :runner "Chameleon")
      (click-prompt state :runner "Equivocation")
      (click-prompt state :runner "Start over")
      (is (= 12 (count (:discard (get-runner)))) "12 cards in discard (HART still in play area)")
      (is (= 0 (count (:deck (get-runner)))) "No cards in stack")
      (is (= 0 (count (:rfg (get-runner)))) "Nothing removed from game")
      (click-prompt state :runner "Dirty Laundry")
      (click-prompt state :runner "Find the Truth")
      (click-prompt state :runner "Bankroll")
      (click-prompt state :runner "Chameleon")
      (click-prompt state :runner "Equivocation")
      (click-prompt state :runner "OK")
      (is (= 7 (count (:discard (get-runner)))) "7 cards in discard")
      (is (= 5 (count (:deck (get-runner)))) "5 cards shuffled back into stack")
      (is (= 1 (count (:rfg (get-runner)))) "HART removed from game")))

(deftest harmony-ar-therapy-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Blacklist"]}
                 :runner {:discard [(qty "Find the Truth" 2) (qty "Astrolabe" 2) (qty "Bankroll" 2) (qty "Chameleon" 2) (qty "Dirty Laundry" 2) (qty "Equivocation" 2)]
                          :hand ["Harmony AR Therapy"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (is (= 12 (count (:discard (get-runner)))) "Well stocked Heap")
      (play-from-hand state :runner "Harmony AR Therapy")
      (is (= 12 (count (:discard (get-runner)))) "Well stocked Heap")
      (is (= 1 (count (:rfg (get-runner)))) "HART removed from game")))

(deftest high-stakes-job
  ;; High Stakes Job - run on server with at least 1 unrezzed piece of ice, gains 12 credits if successful
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["High-Stakes Job"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 1)
    (is (= 6 (:credit (get-runner))) "Runner starts with 6 credits")
    (play-from-hand state :runner "High-Stakes Job")
    (is (= ["HQ"] (prompt-buttons :runner)) "Only has 1 server to choose from")
    (click-prompt state :runner "HQ")
    (run-continue-until state :success)
    (is (= 12 (:credit (get-runner))) "Runner gains 12 credits")))

(deftest hostage
  ;; Hostage - Search for connection, add it to grip, optionally play installing cost
  (do-game
    (new-game {:runner {:hand ["Hostage"]
                        :deck ["Kati Jones"]}}) ; 2 cost connection
    (take-credits state :corp)
    (let [original-deck-count (count (:deck (get-runner)))
          original-hand-count (count (:hand (get-runner)))]
      (play-from-hand state :runner "Hostage")
      (click-prompt state :runner "Kati Jones")
      (click-prompt state :runner "No")
      (is (= (+ 5 -1) (:credit (get-runner))) "Spent 1 credits")
      (is (= 0 (count (get-resource state))) "Pulled card was not installed")
      (is (= (+ original-deck-count -1) (count (:deck (get-runner)))) "Took card from deck")
      (is (= (+ original-hand-count -1 1) (count (:hand (get-runner)))) "Put card in hand")))
  (testing "Basic test, installing"
    (do-game
      (new-game {:runner {:hand ["Hostage"]
                          :deck ["Kati Jones"]}}) ; 2 cost connection
      (take-credits state :corp)
      (is (changed? [(count (:deck (get-runner))) -1
                     (count (:hand (get-runner))) -1
                     (:credit (get-runner)) -3]
            (play-from-hand state :runner "Hostage")
            (click-prompt state :runner "Kati Jones")
            (click-prompt state :runner "Yes")))
      (is (= "Kati Jones" (:title (get-resource state 0))) "Pulled card was correctly installed"))))

(deftest hostage-not-enough-to-play-pulled-card-4364
    ;; Not enough to play pulled card (#4364)
    (do-game
     (new-game {:runner {:hand ["Hostage"]
                         :deck ["Professional Contacts"]}}) ; 5 cost connection
    (take-credits state :corp)
     (let [original-deck-count (count (:deck (get-runner)))
           original-hand-count (count (:hand (get-runner)))]
       (play-from-hand state :runner "Hostage")
       (click-prompt state :runner "Professional Contacts")
       (is (= (+ 5 -1) (:credit (get-runner))) "Spent 1 credit")
       (is (= 0 (count (get-resource state))) "Pulled card was not installed")
       (is (= (+ original-deck-count -1) (count (:deck (get-runner)))) "Took card from deck")
       (is (= (+ original-hand-count -1 1) (count (:hand (get-runner)))) "Put card in hand"))))

(deftest hot-pursuit-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:runner {:deck ["Hot Pursuit"]}})
      (take-credits state :corp)
      (play-run-event state "Hot Pursuit" :hq)
      (is (= (+ 5 -2 9) (:credit (get-runner))) "Gained 9 credits on successful run")
      (is (= 1 (count-tags state)) "Took 1 tag on successful run")
      (is (prompt-map :runner) "Still have access prompt")
      (click-prompt state :runner "No action")
      (is (not (:run @state)) "Run is finished")))

(deftest hot-pursuit-vs-seb
  ;; Basic behavior
  (do-game
    (new-game {:runner {:id "Sebastião Souza Pessoa: Activist Organizer"
                        :credits 2
                        :deck ["Hot Pursuit" "The Supplier"]}})
    (take-credits state :corp)
    (play-run-event state "Hot Pursuit" :hq)
    (is (= 9 (:credit (get-runner))) "Gained creds before firing sub's ability")
    (is (= 1 (count-tags state)) "Took 1 tag on successful run")
    (click-card state :runner "The Supplier")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run is finished")))


(deftest hot-pursuit-bounce-from-hq
    ;; Bounce from HQ
    (do-game
      (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                        :deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:deck ["Hot Pursuit"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hot Pursuit")
      (is (:run @state) "Initiated run")
      (run-continue state)
      (is (= (get-ice state :hq 0) (core/get-current-ice state)))
      (card-ability state :corp (:identity (get-corp)) 0)
      (click-prompt state :corp "Archives")
      (run-continue state)
      (is (= (+ 5 -2) (:credit (get-runner))) "Gained no credits on redirected run")
      (is (zero? (count-tags state)) "Took 0 tags on redirected run")
      (is (not (:run @state)) "Run is finished")))

(deftest i-ve-had-worse
  ;; I've Had Worse - Draw 3 cards when lost to net/meat damage; don't trigger if flatlined
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Hedge Fund"]
                        :credits 10}
                 :runner {:deck ["I've Had Worse" (qty "Sure Gamble" 3) (qty "Imp" 2)]
                          :hand ["I've Had Worse"]}})
      (damage state :corp :net 1)
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 3 (count (:hand (get-runner)))) "I've Had Worse triggered and drew 3 cards")
      (starting-hand state :runner ["I've Had Worse" "Imp" "Imp"])
      (damage state :corp :meat 4)
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")
      (is (= 4 (count (:discard (get-runner)))) "All 3 cards in Grip trashed by Scorched Earth")
      (is (= 3 (count (:deck (get-runner)))) "No cards drawn from I've Had Worse")))

(deftest i-ve-had-worse-will-save-you-if-you-apocalypse-away-a-lot-of-cards-vs-hostile-infrastructure
    ;; Will save you if you apocalypse away a lot of cards vs Hostile Infrastructure
    (do-game
      (new-game {:corp {:deck ["Hostile Infrastructure" (qty "Ice Wall" 2)]}
                 :runner {:deck [(qty "I've Had Worse" 3) (qty "Sure Gamble" 3) (qty "Apocalypse" 2)]}})
      (starting-hand state :runner ["I've Had Worse" "Apocalypse"])
      (starting-hand state :corp ["Hostile Infrastructure" "Ice Wall" "Ice Wall"])
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (play-from-hand state :runner "Apocalypse")
      (is (not= "Flatline" (:reason @state)) "Win condition does not report flatline")))

(deftest illumination-test
  (do-game
    (new-game {:runner {:hand ["Illumination" "Daily Casts" "Orca" "Fermenter"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Illumination")
    (run-continue-until state :success)
    (is (changed? [(:credit (get-runner)) -2]
                  (click-card state :runner "Daily Casts"))
        "Saved 1 to install casts")
    (is (changed? [(:credit (get-runner)) 0]
                  (click-card state :runner "Orca"))
        "No orca install")
    (is (changed? [(:credit (get-runner)) 0]
                  (click-card state :runner "Fermenter"))
        "Free fermenter install")
    (click-prompt state :runner "Done")
    (is (not (:run @state)))))

(deftest immolation-script
  ;; Immolation Script
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]
                        :discard ["Ice Wall"]}
                 :runner {:hand ["Immolation Script"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-run-event state "Immolation Script" :archives)
      (click-prompt state :runner "Ice Wall")
      (click-card state :runner (get-ice state :hq 0))
      (is (not (get-ice state :hq 0)) "Ice Wall is trashed")
      (is (= 2 (count (:discard (get-corp)))) "2 cards in trash now")))

(deftest immolation-script-with-no-ice-in-archives
    ;; with no ice in archives
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Immolation Script"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Immolation Script")
      (run-continue state)
      (run-continue state)
      (is (no-prompt? state :runner)) "No prompt for runner"))

(deftest immolation-script-with-no-ice-installed
    ;; with no ice installed
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :discard ["Ice Wall"]}
                 :runner {:hand ["Immolation Script"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Immolation Script")
      (run-continue state)
      (run-continue state)
      (is (no-prompt? state :runner)) "No prompt for runner"))

(deftest in-the-groove-vanilla-test
    ;; Vanilla test
    (do-game
     (new-game {:runner {:deck ["Clone Chip" "Sacrificial Construct"]
                         :hand ["In the Groove" "Clone Chip" "Clone Chip" "Clone Chip"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "In the Groove")
     ;; the below is just to ensure we don't get weird bugs with event registration if card is somewhere else
     (core/gain state :runner :click 3)
     (core/move state :runner (first (:discard (get-runner))) :deck)
     (is (= 0 (count (:discard (get-runner)))) "Runner discard is empty")
     (dotimes [_ 2]
       (is (changed? [(count (:hand (get-runner))) 0]
             ; play 1, draw 1 for net 0
             (play-from-hand state :runner "Clone Chip")
             (click-prompt state :runner "Draw 1 card"))
           "Drew card from In the Groove"))
     (play-from-hand state :runner "Clone Chip")
     (is (changed? [(:credit (get-runner)) 1]
           (click-prompt state :runner "Gain 1 [Credits]")))
     (play-from-hand state :runner "Sacrificial Construct")
     (is (no-prompt? state :runner) "No prompt because Sacrificial Construct is not expensive")
     (take-credits state :runner)
     (take-credits state :corp)
     (play-from-hand state :runner "Clone Chip")
     (is (no-prompt? state :runner) "No prompt because In the Groove does not last between turns")))

(deftest in-the-groove-cybernetics-interaction
    ;; Cybernetics interaction
    (do-game
     (new-game {:runner {:deck [(qty "In the Groove" 3) "Brain Cage"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "In the Groove")
     (play-from-hand state :runner "Brain Cage")
     (is (= 0 (:brain-damage (get-runner))) "No core damage taken yet")
     (click-prompt state :runner "Brain Cage")
     (is (= 1 (:brain-damage (get-runner))) "Core damage taken")
     (is (changed? [(:credit (get-runner)) 1]
           (click-prompt state :runner "Gain 1 [Credits]")))))

(deftest in-the-groove-hayley-kaplan-interaction
    ;; Hayley Kaplan interaction
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["In the Groove" "Pelangi" "Imp" "Sure Gamble"]
                          :id "Hayley Kaplan: Universal Scholar"}})
      (take-credits state :corp)
      (play-from-hand state :runner "In the Groove")
      (play-from-hand state :runner "Pelangi")
      (click-prompt state :runner "In the Groove")
      (click-prompt state :runner "Gain 1 [Credits]")
      (click-prompt state :runner "Yes")
      (is (changed? [(count (:hand (get-runner))) 0]
            ; play 1, draw 1 for net 0
            (click-card state :runner (find-card "Imp" (:hand (get-runner))))
            (click-prompt state :runner "Draw 1 card"))
          "Drew card from In the Groove trigger off Hayley install")))

(deftest in-the-groove-reboot-interaction
  ;; In the Groove should not trigger off facedown installs
  (do-game
    (new-game {:runner {:hand ["In the Groove" "Reboot"]
                        :discard ["Sure Gamble" "Paperclip" "Clot"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "In the Groove")
    (play-run-event state "Reboot" :archives)
    (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
    (click-card state :runner (find-card "Paperclip" (:discard (get-runner))))
    (click-card state :runner (find-card "Clot" (:discard (get-runner))))
    (click-prompt state :runner "Done")
    (is (no-prompt? state :runner) "No In the Groove prompts should be available.")))

(deftest independent-thinking
  ;; Independent Thinking - Trash 2 installed cards, including a facedown directive, and draw 2 cards
  (do-game
    (new-game {:runner {:id "Apex: Invasive Predator"
                        :deck ["Neutralize All Threats" (qty "Independent Thinking" 2)
                               (qty "Fan Site" 3) (qty "Street Magic" 3)]}})
    (starting-hand state :runner ["Fan Site" "Fan Site" "Neutralize All Threats"
                                  "Independent Thinking" "Independent Thinking"])
    (take-credits state :corp)
    (end-phase-12 state :runner)
    (click-card state :runner (find-card "Neutralize All Threats" (:hand (get-runner))))
    (play-from-hand state :runner "Fan Site")
    (let [fs (get-resource state 0)
          nat (get-runner-facedown state 0)]
      (play-from-hand state :runner "Independent Thinking")
      (click-card state :runner fs)
      (click-card state :runner nat)
      (click-prompt state :runner "Done")
      (is (= 4 (count (:hand (get-runner)))) "Trashing 2 cards draws 2 card"))))

(deftest indexing
  ;; Indexing - Full test
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Adonis Campaign" "Quandary"
                             "Jackson Howard" "Global Food Initiative"]}
               :runner {:deck ["Indexing"]}})
    (dotimes [_ 5] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (is (zero? (count (:hand (get-corp)))))
    (is (= 5 (count (:deck (get-corp)))))
    (play-run-event state "Indexing" :rd)
    (click-prompt state :runner "Indexing")
    (click-prompt state :runner (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Global Food Initiative" (:deck (get-corp))))
    ;; try starting over
    (click-prompt state :runner "Start over")
    (click-prompt state :runner (find-card "Global Food Initiative" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Quandary" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Adonis Campaign" (:deck (get-corp))))
    (click-prompt state :runner (find-card "Caprice Nisei" (:deck (get-corp)))) ;this is the top card of R&D
    (click-prompt state :runner "Done")
    (is (= "Caprice Nisei" (:title (first (:deck (get-corp))))))
    (is (= "Adonis Campaign" (:title (second (:deck (get-corp))))))
    (is (= "Quandary" (:title (second (rest (:deck (get-corp)))))))
    (is (= "Jackson Howard" (:title (second (rest (rest (:deck (get-corp))))))))
    (is (= "Global Food Initiative" (:title (second (rest (rest (rest (:deck (get-corp)))))))))))

(deftest infiltration-gain-2
    ;; Gain 2
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Infiltration"]}})
      (take-credits state :corp)
      (let [credits (:credit (get-runner))]
        (play-from-hand state :runner "Infiltration")
        (click-prompt state :runner "Gain 2 [Credits]")
        (is (= (+ 2 credits) (:credit (get-runner))) "Runner gains 2"))))

(deftest infiltration-expose
    ;; Expose
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Infiltration"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Infiltration")
      (click-prompt state :runner "Expose a card")
      (click-card state :runner "Ice Wall")
      (is (last-log-contains? state "Runner uses Infiltration to expose Ice Wall protecting HQ at position 0")
          "Infiltration properly exposes the ice")))

(deftest information-sifting-hudson-interaction-max-access
    ;; Hudson interaction :max-access
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi"
                               "DNA Tracker" "Hudson 1.0"]}
                 :runner {:deck ["Information Sifting"]}})
      (play-from-hand state :corp "Hudson 1.0" "HQ")
      (let [hud (get-ice state :hq 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Information Sifting")
        (run-continue state)
        (rez state :corp hud)
        (run-continue state)
        (card-subroutine state :corp hud 0) ; Runner cannot access more than 1 card
        (run-continue state)
        (run-continue state)
        (click-card state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))))
        (click-prompt state :corp "Done")
        (click-prompt state :runner "Pile 2 (3 cards)")
        (click-prompt state :runner "Card from pile 2")
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner) "No prompt to access further cards.")
        (is (no-prompt? state :corp) "No more Info Sifting prompts"))))

(deftest inject
  ;; Inject - Draw 4 cards from Stack and gain 1 credit per trashed program
  (do-game
    (new-game {:runner {:deck ["Inject" (qty "Imp" 2) (qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Imp" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Imp" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (is (= 4 (count (:deck (get-runner)))))
    (play-from-hand state :runner "Inject")
    (is (= 2 (count (:hand (get-runner)))) "2 non-programs kept in Grip")
    (is (= 2 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "2 programs in Heap")
    (is (= 6 (:credit (get-runner)))
        "Paid 1 credit to play Inject, gained 2 credits from trashed programs")))

(deftest injection-attack
  ;; Injection Attack
  (do-game
    (new-game {:corp {:deck ["Paper Wall"]}
               :runner {:deck ["Injection Attack" "Corroder"]}})
    (play-from-hand state :corp "Paper Wall" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Injection Attack")
    (click-prompt state :runner "Archives")
    (is (= 2 (get-strength (get-program state 0))) "Corroder at 2 strength")
    (click-card state :runner (get-program state 0))
    (is (= 4 (get-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (is (= 4 (get-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (run-continue state)
    (is (= 2 (get-strength (get-program state 0))) "Corroder reset to 2 strength")))

(deftest inside-job
  ;; Inside Job
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (is (:run @state) "A run has been initiated")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")))

(deftest inside-job-vs-doppelganger
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Inside Job" "Doppelgänger"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (rez state :corp (get-ice state :hq 0))
    (play-cards state :runner "Doppelgänger" ["Inside Job" "HQ"])
    (run-continue-until state :encounter-ice)
    (is (= :movement (:phase (:run @state))) "Bypassed")
    (run-continue-until state :success)
    (click-prompts state :runner "Yes" "HQ")
    (run-continue-until state :encounter-ice)
    (is (= :encounter-ice (:phase (:run @state))) "Did not bypass on doppel run")))

(deftest inside-job-only-bypasses-one-ice
    ;; Only bypasses one ice
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Ice Wall" 2)]}
                 :runner {:hand ["Inside Job"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Inside Job")
      (click-prompt state :runner "HQ")
      (is (:run @state) "A run has been initiated")
      (run-continue state)
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= :encounter-ice (:phase (get-run))) "Run not has bypassed Ice Wall")))

(deftest insight
  ;; Insight
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Elizabeth Mills"
                             "Jackson Howard" "Director Haas"]}
               :runner {:deck ["Insight"]}})
    (dotimes [_ 4] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (is (zero? (count (:hand (get-corp)))))
    (is (= 4 (count (:deck (get-corp)))))
    (play-from-hand state :runner "Insight")
    (is (waiting? state :runner) "Runner is waiting for Corp to reorder")
    (click-prompt state :corp (find-card "Director Haas" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Elizabeth Mills" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp "Done")
    (is (not (waiting? state :runner)) "Waiting prompt done")
    (is (= "Caprice Nisei" (:title (nth (:deck (get-corp)) 0))))
    (is (= "Jackson Howard" (:title (nth (:deck (get-corp)) 1))))
    (is (= "Elizabeth Mills" (:title (nth (:deck (get-corp)) 2))))
    (is (= "Director Haas" (:title (nth (:deck (get-corp)) 3))))))

(deftest interdiction
  ;; Corp cannot rez non-ice cards during runner's turn
  (do-game
    (new-game {:corp {:deck ["Jeeves Model Bioroids" "Jackson Howard"]}
               :runner {:deck ["Street Peddler"
                               (qty "Interdiction" 3)]}})
    (starting-hand state :runner ["Street Peddler" "Interdiction"])
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (rez state :corp jeeves)
      (is (rezzed? (refresh jeeves)) "Jeeves is rezzed.  Interdiction not active when on Peddler")
      (play-from-hand state :runner "Interdiction")
      (rez state :corp jackson {:expect-rez false})
      (is (not (rezzed? (refresh jackson))) "Jackson is not rezzed"))))

(deftest into-the-depths
  ;; Into the depths
  (do-game
    ;; Didn't pass any ice
    (new-game {:runner {:hand ["Into the Depths"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Into the Depths")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "No action"))
  (do-game
    ;; Passed 1 ice
    (new-game {:runner {:hand ["Into the Depths"]}
               :corp {:hand [(qty "Ice Wall" 5)] :credits 50}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Into the Depths")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Gain 4 [Credits]")
    (click-prompt state :runner "No action"))
  (do-game
    ;; Passed 4 ice
    (new-game {:runner {:hand ["Into the Depths"] :deck ["D4v1d"]}
               :corp {:hand [(qty "Ice Wall" 5)] :credits 50}})
    (core/gain state :corp :click 2)
    (doseq [n [0 1 2 3]]
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq n)))
    (take-credits state :corp)
    (play-from-hand state :runner "Into the Depths")
    (click-prompt state :runner "HQ")
    (run-continue-until state :success)
    (click-prompt state :runner "Gain 4 [Credits]")
    (click-prompt state :runner "Install a program from the stack")
    (click-prompt state :runner "D4v1d")
    (click-prompt state :runner "Charge a card")
    (click-card state :runner "D4v1d")
    (click-prompt state :runner "No action")
    (is (= 4 (get-counters (get-program state 0) :power)) "4 counters on david")))

(deftest into-the-depths-inversificator-still-counts-the-ice
  (do-game
    (new-game {:runner {:hand ["Into the Depths" "Inversificator"]
                        :credits 20}
               :corp {:hand [(qty "Quandary" 2)]}})
    (play-from-hand state :corp "Quandary" "HQ")
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (rez state :corp (get-ice state :hq 0))
    (play-from-hand state :runner "Inversificator")
    (play-from-hand state :runner "Into the Depths")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state :encounter-ice)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "End the run")
    (run-continue state :movement)
    (click-prompt state :runner "Yes")
    (click-card state :runner (get-ice state :rd 0))
    (run-continue-until state :success)
    (click-prompt state :runner "Gain 4 [Credits]")))

(deftest isolation
  ;; Isolation - A resource must be trashed, gain 7c
  (do-game
    (new-game {:runner {:deck ["Kati Jones" "Isolation"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Isolation")
    (is (= 2 (count (get-in @state [:runner :hand]))) "Isolation could not be played because no resource is installed")
    (is (zero? (count (get-resource state))) "Kati Jones is not installed")
    (play-from-hand state :runner "Kati Jones")
    (is (= 1 (count (get-resource state))) "Kati Jones was installed")
    (let [kj (get-resource state 0)]
      (play-from-hand state :runner "Isolation")
      (click-card state :runner kj)
      (is (nil? (refresh kj)) "Kati Jones was trashed")
      (is (= 8 (:credit (get-runner))) "Gained 7 credits")
      (is (= 2 (count (:discard (get-runner)))) "Kati Jones and Isolation are in the discard"))))

(deftest itinerant-protesters
  ;; Itinerant Protesters
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Itinerant Protesters"]}})
    (take-credits state :corp)
    (is (= 5 (hand-size :corp)) "Corp starts with handsize of 5")
    (play-from-hand state :runner "Itinerant Protesters")
    (change state :corp :bad-publicity 1)
    (is (= 4 (hand-size :corp)) "Corp's handsize is lowered by 1 for a bad publicity")
    (change state :corp :bad-publicity 3)
    (is (= 1 (hand-size :corp)))))

(deftest jailbreak
  ;; Jailbreak
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Vanilla" 2)]}
               :runner {:deck [(qty "Sure Gamble" 2)]
                        :hand [(qty "Jailbreak" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Jailbreak")
    (click-prompt state :runner "R&D")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (is (= 2 (count (:hand (get-runner)))) "One played, one drawn")
    (play-from-hand state :runner "Jailbreak")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")
    (is (= 2 (count (:hand (get-runner)))) "One played, one drawn")))

(deftest joy-ride
  ;; Basic Functionality
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "PAD Campaign" 5)]}
               :runner {:hand ["Joy Ride"]
                        :deck [(qty "Sure Gamble" 5)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Joy Ride")
    (run-continue state)
    (is (changed? [(count (:hand (get-runner))) 5]
          (run-continue state))
        "Drew 5 cards")
    (click-prompt state :runner "No action")))

(deftest katorga-breakout
  ;; Basic Functionality
  (do-game
    (new-game {:corp {:hand ["BOOM!"]}
               :runner {:hand ["Katorga Breakout" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Katorga Breakout")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Sure Gamble")
    (click-prompt state :runner "No action")
    (is (find-card "Sure Gamble" (:hand (get-runner))) "Sure Gamble is in hand")))

(deftest khusyuk-basic-functionality
    ;; Basic functionality
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi"
                               "DNA Tracker" "Excalibur" "Fire Wall"]}
                 :runner {:deck ["Khusyuk"
                                 (qty "Cache" 3)
                                 (qty "Akamatsu Mem Chip" 3)
                                 "Gordian Blade"]}})
      (draw state :corp)
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "DNA Tracker" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Excalibur" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Fire Wall" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      (is (= (:title (nth (-> @state :corp :deck) 3)) "DNA Tracker"))
      (is (= (:title (nth (-> @state :corp :deck) 4)) "Excalibur"))
      (is (= (:title (nth (-> @state :corp :deck) 5)) "Fire Wall"))
      ;; R&D is now from top to bottom: A B C D E F
      (take-credits state :corp)
      (core/gain state :runner :click 100)
      (core/gain state :runner :credit 100)
      (dotimes [_ 4] (draw state :runner))
      (dotimes [_ 3] (play-from-hand state :runner "Cache"))
      (dotimes [_ 3] (play-from-hand state :runner "Akamatsu Mem Chip"))
      (play-from-hand state :runner "Gordian Blade")
      (play-run-event state "Khusyuk" :rd)
      (click-prompt state :runner "1 [Credit]: 6 cards")
      (is (last-log-contains? state "Accelerated Beta Test, Brainstorm, Chiyashi, DNA Tracker, Excalibur, and Fire Wall")
          "Revealed correct 6 cards from R&D")
      (click-prompt state :runner "Brainstorm")
      (click-prompt state :runner "No action")))

(deftest khusyuk-khusyuk-would-reveal-more-cards-than-there-are-in-r-d
    ;; Khusyuk would reveal more cards than there are in R&D
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi"]}
                 :runner {:deck ["Khusyuk"
                                 (qty "Cache" 3)
                                 (qty "Akamatsu Mem Chip" 3)]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      ;; R&D is now from top to bottom: A B C
      (take-credits state :corp)
      (core/gain state :runner :click 100)
      (core/gain state :runner :credit 100)
      (dotimes [_ 3] (draw state :runner))
      (dotimes [_ 3] (play-from-hand state :runner "Cache"))
      (dotimes [_ 3] (play-from-hand state :runner "Akamatsu Mem Chip"))
      (play-run-event state "Khusyuk" :rd)
      (click-prompt state :runner "1 [Credit]: 6 cards")
      (is (last-log-contains? state "Accelerated Beta Test, Brainstorm, and Chiyashi") "Revealed correct 3 cards from R&D")
      (click-prompt state :runner "Brainstorm")
      (click-prompt state :runner "No action")))

(deftest khusyuk-no-other-cards-in-r-d-are-accessed
    ;; No other cards in R&D are accessed
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Technician Team"]}
                 :runner {:deck ["Khusyuk"
                                 (qty "Cache" 3)
                                 "R&D Interface"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      ;; R&D is now from top to bottom: A B C
      (play-from-hand state :corp "Dedicated Technician Team" "R&D")
      (take-credits state :corp)
      (core/gain state :runner :click 100)
      (core/gain state :runner :credit 100)
      (dotimes [_ 3] (play-from-hand state :runner "Cache"))
      (play-from-hand state :runner "R&D Interface")
      (play-run-event state "Khusyuk" :rd)
      (click-prompt state :runner "1 [Credit]: 3 cards")
      (is (last-log-contains? state "Accelerated Beta Test, Brainstorm, and Chiyashi") "Revealed correct 3 cards from R&D")
      (click-prompt state :runner "Brainstorm")
      (click-prompt state :runner "No action")
      (is (no-prompt? state :runner) "No access prompt on C or D, so no other cards were accessed")))

(deftest khusyuk-when-played-with-no-installed-cards
    ;; When played with no installed cards
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Technician Team"]}
                 :runner {:deck ["Khusyuk"
                                 (qty "Cache" 3)
                                 "R&D Interface"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-run-event state "Khusyuk" :rd)
      (click-prompt state :runner "1 [Credit]: 0 cards")
      (is (no-prompt? state :runner) "Runner shouldn't get any access prompt when nothing is installed")))

(deftest khusyuk-interaction-with-the-turning-wheel
    ;; Interaction with The Turning Wheel
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Technician Team"]}
                 :runner {:deck ["Khusyuk" "The Turning Wheel"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "The Turning Wheel")
      (let [tww (get-resource state 0)]
        (play-run-event state "Khusyuk" :rd)
        (is (zero? (get-counters (refresh tww) :power)) "The Turning Wheel shouldn't gain counters yet"))))

(deftest khusyuk-ash-interaction
    ;; Ash interaction
    (do-game
      (new-game {:corp {:deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Dedicated Technician Team" "Ash 2X3ZB9CY"]}
                 :runner {:deck ["Khusyuk"
                                 (qty "Cache" 3)]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      ;; R&D is now from top to bottom: A B C
      (play-from-hand state :corp "Ash 2X3ZB9CY" "R&D")
      (let [ash (get-content state :rd 0)]
        (rez state :corp ash)
        (take-credits state :corp)
        (core/gain state :runner :click 100)
        (core/gain state :runner :credit 100)
        (dotimes [_ 3] (play-from-hand state :runner "Cache"))
        (play-run-event state "Khusyuk" :rd)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0") ; lose Ash trace
        (click-prompt state :runner "1 [Credit]: 3 cards")
        (is (second-last-log-contains? state "Accelerated Beta Test, Brainstorm, and Chiyashi") "Revealed correct 3 cards from R&D")
        (is (no-prompt? state :runner) "No prompt to access cards."))))

(deftest khusyuk-eater-interaction
    ;; Eater interaction
    (do-game
      (new-game {:corp {:hand ["Accelerated Beta Test" "Brainstorm" "Chiyashi" "Ice Wall"]}
                 :runner {:deck ["Khusyuk"
                                 (qty "Cache" 3)
                                 "Eater"]}})
      (core/move state :corp (find-card "Accelerated Beta Test" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Brainstorm" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Chiyashi" (:hand (get-corp))) :deck)
      (is (= (:title (nth (-> @state :corp :deck) 0)) "Accelerated Beta Test"))
      (is (= (:title (nth (-> @state :corp :deck) 1)) "Brainstorm"))
      (is (= (:title (nth (-> @state :corp :deck) 2)) "Chiyashi"))
      ;; R&D is now from top to bottom: A B C
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (core/gain state :runner :click 100)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Eater")
      (dotimes [_ 3] (play-from-hand state :runner "Cache"))
      (play-from-hand state :runner "Khusyuk")
      (run-continue state)
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0) ; use Eater
      (click-prompt state :runner "End the run")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "1 [Credit]: 3 cards")
      (is (second-last-log-contains? state "Accelerated Beta Test, Brainstorm, and Chiyashi") "Revealed correct 3 cards from R&D")
      (is (no-prompt? state :runner) "No prompt to access cards.")))

(deftest knifed
  ;; Knifed - Make a run, trash a barrier if all subs broken
  (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Knifed" "Corroder"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Knifed")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall is trashed")))

(deftest knifed-should-not-work-like-hippo-issue-6382
    ;; Can trash ice even if an ice with the same subtype has been broken during a previous run. Issue #6382
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Knifed" "Corroder"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (run-on state "R&D")
      (rez state :corp (get-ice state :rd 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (run-jack-out state)
      (play-from-hand state :runner "Knifed")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue-until state :approach-ice)
      (is (find-card "Ice Wall" (:discard (get-corp))) "Ice Wall is trashed")))

(deftest knifed-can-only-trash-a-single-ice-per-run-issue-4791
    ;; Can only trash a single ice per run. Issue #4791
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Knifed" "Corroder"]
                          :credits 10}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Knifed")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue-until state :approach-ice)
      (is (find-card "Ice Wall" (:discard (get-corp))) "Ice Wall is trashed")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (card-ability state :runner (get-program state 0) 0)
      (click-prompt state :runner "End the run")
      (run-continue state)
      (is (get-ice state :hq 0) "Second Ice Wall is not trashed")))

(deftest kraken
  ;; Kraken
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover" "Ice Wall"]}
               :runner {:hand ["Kraken"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Kraken")
    (is (no-prompt? state :runner) "No prompt as the runner hasn't stolen an agenda yet")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Kraken")
    (click-prompt state :runner "HQ")
    (click-card state :corp (get-ice state :hq 0))
    (is (not (get-ice state :hq 0)) "Ice Wall is trashed")))

(deftest labor-rights-basic-behavior
    ;; Basic behavior
    (do-game
      (new-game {:runner {:hand ["Labor Rights"] :deck ["Sure Gamble" "Lawyer Up" "Knifed"]}})
      (take-credits state :corp)
      (is (empty? (:discard (get-runner))) "Starts with no cards in discard")
      (is (= 3 (count (:deck (get-runner)))) "Starts with 3 cards in deck")
      (play-from-hand state :runner "Labor Rights")
      (is (last-log-contains? state "Runner trashes"))
      (is (last-log-contains? state "from the top of the stack"))
      (is (last-log-contains? state "Lawyer Up"))
      (is (last-log-contains? state "Sure Gamble"))
      (is (last-log-contains? state "Knifed"))
      (is (empty? (:deck (get-runner))) "Milled 3 cards")
      (is (= 3 (count (:discard (get-runner)))) "4 cards in deck - 3x trashed")
      (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
      (click-card state :runner (find-card "Knifed" (:discard (get-runner))))
      (click-card state :runner (find-card "Lawyer Up" (:discard (get-runner))))
      (is (= 2 (count (:deck (get-runner)))) "2 cards in deck")
      (is (= 1 (count (:hand (get-runner)))) "1 card in hand")
      (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg")))

(deftest labor-rights-less-than-3-cards
    ;; Less than 3 cards
    (do-game
      (new-game {:runner {:hand [(qty "Labor Rights" 2)] :deck ["Sure Gamble"]}})
      (take-credits state :corp)
      (is (empty? (:discard (get-runner))) "Starts with no cards in discard")
      (is (= 1 (count (:deck (get-runner)))) "Starts with 1 card in deck")
      (play-from-hand state :runner "Labor Rights")
      (is (last-log-contains? state "Runner trashes Sure Gamble from the top of the stack"))
      (is (empty? (:deck (get-runner))) "Milled 1 card")
      (is (= 1 (count (:discard (get-runner)))) "2 cards in deck - 1x trashed")
      (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
      (is (empty? (:deck (get-runner))) "No cards in deck")
      (is (= 2 (count (:hand (get-runner)))) "2 cards in hand")
      (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg")
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Labor Rights")
      (is (last-log-contains? state "Runner trashes no cards"))))

(deftest labor-rights-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5) "Blacklist"]}
                 :runner {:hand ["Labor Rights"] :deck [(qty "Sure Gamble" 3) (qty "Lawyer Up" 3) "Knifed"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (is (empty? (:discard (get-runner))) "Starts with no cards in discard")
      (is (= 7 (count (:deck (get-runner)))) "Starts with 7 cards in deck")
      (play-from-hand state :runner "Labor Rights")
      (take-credits state :runner)
      (is (no-prompt? state :runner) "Shuffle prompt did not come up")
      (is (= 3 (count (:deck (get-runner)))) "3 cards in deck")
      (is (= 3 (count (:discard (get-runner)))) "3 cards in discard")
      (is (= 1 (count (:hand (get-runner)))) "1 card in hand")
      (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg")))

(deftest lawyer-up
  ;; Lawyer Up - Lose 2 tags and draw 3 cards
  (do-game
    (new-game {:runner {:deck ["Lawyer Up" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (gain-tags state :runner 3)
    (play-from-hand state :runner "Lawyer Up")
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (= 2 (:click (get-runner))) "Spent 2 clicks")
    (is (= 1 (count-tags state)) "Lost 2 tags")))

(deftest lean-and-mean
  ;; Lean and Mean
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Lean and Mean" 3) (qty "Corroder" 5)]
                        :credits 100}})
    (take-credits state :corp)
    (core/gain state :runner :click 10)
    (testing "Duration and bonus"
      (play-from-hand state :runner "Corroder")
      (is (= 2 (core/get-strength (get-program state 0))) "Corroder starts with 2 str")
      (play-from-hand state :runner "Lean and Mean")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (is (= 4 (core/get-strength (get-program state 0))) "Corroder gains 2 str from Lean and Mean")
      (run-continue state)
      (is (= 2 (core/get-strength (get-program state 0))) "Lean and Mean's str bonus goes away at run end"))
    (testing "Bonus applies to multiple icebreakers"
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Lean and Mean")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (is (= 4 (core/get-strength (get-program state 0))) "Corroder gains 2 str from Lean and Mean")
      (is (= 4 (core/get-strength (get-program state 1))) "Corroder gains 2 str from Lean and Mean")
      (is (= 4 (core/get-strength (get-program state 2))) "Corroder gains 2 str from Lean and Mean")
      (run-continue state))
    (testing "Bonus doesn't apply when there are too many programs installed"
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Lean and Mean")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (run-continue state)
      (is (= 4 (count (get-program state))) "4 programs installed")
      (is (= 2 (core/get-strength (get-program state 0))) "Corroder doesn't gain any strength from Lean and Mean"))))

(deftest leave-no-trace
  ;; Leave No Trace should derez ice that was rezzed during the run
  (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Leave No Trace"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Leave No Trace")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue-until state :success)
      (is (not (rezzed? (get-ice state :hq 0))) "Inner Ice Wall should not be rezzed")
      (is (rezzed? (get-ice state :hq 1)) "Outer Ice Wall should be rezzed still")))

(deftest leave-no-trace-should-not-derez-ice-that-has-changed-during-a-run
    ;; should not derez ice that has changed during a run
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Leave No Trace"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (is (rezzed? (get-ice state :hq 0)) "Ice Wall should be rezzed initially")
      (play-from-hand state :runner "Leave No Trace")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (core/add-prop state :corp (core/make-eid state) (get-ice state :hq 0) :advance-counter 1)
      (run-continue state)
      (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
      (is (rezzed? (get-ice state :hq 0)) "Ice Wall should still be rezzed")))

(deftest leave-no-trace-should-trigger-derez-events-for-runner-not-corp-3919
    ;; Should trigger :derez events for Runner, not Corp (#3919)
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Leave No Trace" "Keros Mcintyre"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Keros Mcintyre")
      (play-from-hand state :runner "Leave No Trace")
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "HQ")
        (run-continue state)
        (rez state :corp (get-ice state :hq 0))
        (run-continue-until state :success)
        (is (= (+ credits 2) (:credit (get-runner))) "Keros should trigger off derez")
        (is (not (rezzed? (get-ice state :hq 0))) "Inner Ice Wall should not be rezzed")
        (is (rezzed? (get-ice state :hq 1)) "Outer Ice Wall should be rezzed still"))))

(deftest legwork
  ;; Legwork
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 3)]}
               :runner {:hand ["Legwork"]}})
    (take-credits state :corp)
    (play-run-event state "Legwork" :hq)
    (dotimes [_ 3]
      (click-prompt state :runner "Steal"))
    (is (not (:run @state)) "Run has finished")))

(deftest legwork-doesn-t-give-bonus-accesses-when-unsuccessful
  ;; Doesn't give bonus accesses when unsuccessful
    (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand [(qty "Hostile Takeover" 3)]}
               :runner {:hand ["Legwork"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Legwork")
    (run-continue state)
    (run-jack-out state)
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (is (not (:run @state)) "Run has finished")))

(deftest legwork-doppelganger-interaction
    ;; Doppelgänger interaction
    (do-game
     (new-game {:runner {:hand ["Doppelgänger" "Legwork"]}
                :corp {:hand [(qty "Hostile Takeover" 5)]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Doppelgänger")
     (play-run-event state "Legwork" :hq)
     (do (dotimes [_ 3]
           (click-prompt state :runner "Steal"))
         (is (not (:run @state)))
         (click-prompt state :runner "Yes")
         (click-prompt state :runner "HQ")
         (run-continue state)
         (is (:run @state) "New run started")
         (run-continue state)
         (click-prompt state :runner "Steal")
         (is (not (:run @state))
             "Legwork only gives bonus accesses on its own run when combined with Doppelgänger"))))

(deftest leverage-corp-takes-bad-publicity
    ;; Corp takes bad publicity
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Leverage"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Leverage")
      (is (no-prompt? state :corp) "No prompt as runner didn't run HQ")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (zero? (count-bad-pub state)) "Corp has no bad pub")
      (play-from-hand state :runner "Leverage")
      (click-prompt state :corp "Yes")
      (is (= 2 (count-bad-pub state)) "Corp gains 2 bad pub from Leverage")))

(deftest leverage-corp-doesn-t-take-bad-publicity
    ;; Corp doesn't take bad publicity
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["BOOM!"]}
                 :runner {:hand ["Leverage"]}
                 :tags 2})
      (is (not (:winner @state)) "No winner is declared yet")
      (take-credits state :corp)
      (play-from-hand state :runner "Leverage")
      (is (no-prompt? state :corp) "No prompt as runner didn't run HQ")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Leverage")
      (click-prompt state :corp "No")
      (take-credits state :runner)
      (play-from-hand state :corp "BOOM!")
      (is (not (:winner @state)) "Runner doesn't take any damage")))

(deftest levy-ar-lab-access
  ;; Levy AR Lab Access
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck ["Magnum Opus"]
                        :hand ["Levy AR Lab Access" "Easy Mark"]
                        :discard [(qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Levy AR Lab Access")
    (is (= 5 (count (:hand (get-runner)))) "Runner should draw 5 cards")
    (is (zero? (count (:deck (get-runner)))) "Stack should be empty")
    (is (zero? (count (:discard (get-runner)))) "Heap should be empty")
    (is (= "Levy AR Lab Access" (:title (get-rfg state :runner 0))) "Levy should be rfg'd")))

(deftest levy-ar-lab-access-heap-locked
      ;; Heap Locked
    (do-game
        (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                            :hand ["Blacklist"]}
                   :runner {:deck ["Magnum Opus"]
                            :hand ["Levy AR Lab Access" "Easy Mark"]
                            :discard [(qty "Sure Gamble" 3)]}})
        (play-from-hand state :corp "Blacklist" "New remote")
        (rez state :corp (refresh (get-content state :remote1 0)))
        (take-credits state :corp)
        (play-from-hand state :runner "Levy AR Lab Access")
        (is (= 2 (count (:hand (get-runner)))) "Runner should draw 2 cards")
        (is (zero? (count (:deck (get-runner)))) "Stack should be empty")
        (is (= 3 (count (:discard (get-runner)))) "Heap should have 3 cards")
        (is (= "Levy AR Lab Access" (:title (get-rfg state :runner 0))) "Levy should be rfg'd")))

(deftest lie-low-draw-4
  (do-game
    (new-game {:runner {:hand ["Lie Low" "Dirty Laundry"]
                        :deck ["Sure Gamble" "Easy Mark" "Fermenter" "Rezeki"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Lie Low")
    (is (changed? [(count (:hand (get-runner))) 4]
          (click-prompt state :runner "Draw 4 cards"))
        "Drew 4 cards")))

(deftest lie-low-remove-tags
  (doseq [n [0 1 2]]
    (do-game
      (new-game {:runner {:hand ["Lie Low"]}})
      (gain-tags state :runner 2)
      (take-credits state :corp)
      (play-from-hand state :runner "Lie Low")
      (is (changed? [(count-tags state) (- n)]
            (click-prompt state :runner "Remove up to 2 tags")
            (if (= n 1)
              (click-prompt state :runner "Remove 1 tag")
              (click-prompt state :runner (str "Remove " n " tags"))))
          (str "removed " n " tags")))))

(deftest lucky-find
  ;; Lucky Find
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Lucky Find"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Lucky Find")
      (is (= (+ credits -3 9) (:credit (get-runner))) "Runner should spend 3 and gain 9"))))

(deftest mad-dash
  ;; Mad Dash - Make a run. Move to score pile as 1 point if steal agenda.  Take 1 meat if not
  (do-game
    (new-game {:corp {:deck ["Project Atlas"]}
               :runner {:deck [(qty "Mad Dash" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mad Dash")
    (click-prompt state :runner "Archives")
    (run-continue state)
    (run-continue state)
    (is (= 2 (count (:discard (get-runner)))) "Took a meat damage")
    (play-from-hand state :runner "Mad Dash")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Steal")
    (is (= 2 (count (:scored (get-runner)))) "Mad Dash moved to score area")
    (is (= 3 (:agenda-point (get-runner))) "Mad Dash scored for 1 agenda point")))

(deftest maintainance-access-test
  (do-game
    (new-game {:corp {:hand ["Project Atlas"] :deck ["Mavirus"] :discard ["Mavirus"]
                      :score-area ["SSL Endorsement"]}
               :runner {:hand ["Maintenance Access"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Maintenance Access")
    (run-continue-until state :success)
    (click-prompt state :runner "Steal")))

(deftest making-an-entrance
  ;; Making an Entrance - Full test
  (do-game
    (new-game {:runner {:deck [(qty "Making an Entrance" 2) "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron"]}})
    (starting-hand state :runner ["Making an Entrance"])
    (is (= 1 (count (:hand (get-runner)))))
    (take-credits state :corp)
    (play-from-hand state :runner "Making an Entrance")
    ;; trash cards
    (is (zero? (count (:discard (get-runner)))))
    (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
    (is (= 2 (count (:discard (get-runner)))))
    (click-prompt state :runner "Done")
    ;; start arranging
    (click-prompt state :runner (find-card "Making an Entrance" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
    ;; try starting over
    (click-prompt state :runner "Start over")
    (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Making an Entrance" (:deck (get-runner)))) ;this is the top card on stack
    (click-prompt state :runner "Done")
    (is (= "Making an Entrance" (:title (first (:deck (get-runner))))))
    (is (= "Sure Gamble" (:title (second (:deck (get-runner))))))
    (is (= "Corroder" (:title (second (rest (:deck (get-runner)))))))
    (is (= "Patron" (:title (second (rest (rest (:deck (get-runner))))))))
    (draw state :runner)
    (is (= "Making an Entrance" (:title (first (:hand (get-runner))))))
    (is (= 1 (count (:hand (get-runner)))))
    (play-from-hand state :runner "Making an Entrance")
    (is (= 1 (count (:hand (get-runner)))) "Can only play on first click")))

(deftest marathon-trashed-on-unsuccessful-run
    ;; Trashed on unsuccessful run
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Marathon"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Marathon")
      (click-prompt state :runner "Server 1")
      (run-continue-until state :movement)
      (run-jack-out state)
      (is (find-card "Marathon" (:discard (get-runner))) "Marathon should be trashed")
      (is (not (find-card "Marathon" (:hand (get-runner)))) "Marathon should not be in hand")))

(deftest marathon-moved-to-hand-on-successful-run
    ;; Moved to hand on successful run
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Marathon"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (take-credits state :corp)
      (let [clicks (:click (get-runner))]
        (play-from-hand state :runner "Marathon")
        (click-prompt state :runner "Server 1")
        (run-continue-until state :success)
        (is (find-card "Marathon" (:hand (get-runner))) "Marathon should be in hand")
        (is (= clicks (:click (get-runner))) "Runner should gain 1 click"))
      (is (not (:run @state)) "Run has ended")
      (run-on state "Server 1" {:wait-at-initiation true})
      (is (not (:run @state)) "Run shouldn't be initiated on a Marathon'd server")))

(deftest mars-for-martians
  ;; Mars for Martians - Full test
  (do-game
    (new-game {:runner {:deck ["Mars for Martians" "Clan Vengeance" "Counter Surveillance"
                               "Jarogniew Mercs" (qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Mars for Martians" "Clan Vengeance" "Counter Surveillance" "Jarogniew Mercs"])
    (take-credits state :corp)
    (play-from-hand state :runner "Clan Vengeance")
    (play-from-hand state :runner "Counter Surveillance")
    (play-from-hand state :runner "Jarogniew Mercs")
    (play-from-hand state :runner "Mars for Martians")
    (is (= 1 (:click (get-runner))) "Mars for Martians not played, priority event")
    (take-credits state :runner)
    (take-credits state :corp)
    (gain-tags state :runner 4)
    (is (= 5 (count-tags state)) "+1 tag from Jarogniew Mercs")
    (is (= 1 (count (:hand (get-runner)))))
    (is (= 2 (:credit (get-runner))))
    (play-from-hand state :runner "Mars for Martians")
    (is (= 3 (count (:hand (get-runner)))) "3 clan resources, +3 cards but -1 for playing Mars for Martians")
    (is (= 7 (:credit (get-runner))) "5 tags, +5 credits")))

(deftest mass-install
  ;; Mass Install
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Mass Install" "Corroder" "Self-modifying Code" "Cloak"]
                        :credits 10}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mass Install")
    (click-card state :runner "Corroder")
    (is (= "Corroder" (:title (get-program state 0))) "Corroder should be installed")
    (click-card state :runner "Self-modifying Code")
    (is (= "Self-modifying Code" (:title (get-program state 1))) "SMC should be installed")
    (click-card state :runner "Cloak")
    (is (= "Cloak" (:title (get-program state 2))) "Cloak should be installed")
    (is (no-prompt? state :runner) "Runner should have no more prompts")))

(deftest meeting-of-minds
  (do-game
    (new-game {:runner {:hand [(qty "Meeting of Minds" 2) "Liberated Account" "Kati Jones" "Always Be Running" "Crypt"]
                        :deck ["The Class Act" "Cookbook" "Smartware Distributor"]
                        :credits 10}})
    (take-credits state :corp)
    ;; Choosing virtual
    (play-from-hand state :runner "Meeting of Minds")
    (click-prompt state :runner "Virtual")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Cookbook")
    (is (changed? [(:credit (get-runner)) 2]
          (click-card state :runner "Cookbook")
          (click-card state :runner "Always Be Running")
          (click-prompt state :runner "Done"))
        "Runner gained 2 credits")
    (is (no-prompt? state :runner))
    ;; Choosing connection
    (play-from-hand state :runner "Meeting of Minds")
    (click-prompt state :runner "Connection")
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "The Class Act")
    (is (changed? [(:credit (get-runner)) 2]
          (click-card state :runner "The Class Act")
          (click-card state :runner "Kati Jones")
          (click-prompt state :runner "Done"))
        "Runner gained 2 credits")))

(deftest mining-accident
  ;; Mining Accident
  (do-game
    (new-game {:runner {:deck [(qty "Mining Accident" 3)]
                        :credits 10}})
    (take-credits state :corp)
    (is (changed? [(:click (get-runner)) 0]
          (play-from-hand state :runner "Mining Accident"))
        "Couldn't play Mining Accident without running a central first")
    (run-empty-server state "HQ")
    (click-prompt state :runner "No action")
    (is (changed? [(count-bad-pub state) 1]
          (play-from-hand state :runner "Mining Accident")
          (click-prompt state :corp "Take 1 bad publicity"))
        "Corp took 1 BP")
    (is (changed? [(:credit (get-corp)) -5]
          (play-from-hand state :runner "Mining Accident")
          (click-prompt state :corp "Pay 5 [Credits]"))
        "Corp paid 5c")
    (is (changed? [(count-bad-pub state) 1]
          (play-from-hand state :runner "Mining Accident")
          (is (= 1 (count (prompt-buttons :corp))) "No option to pay credits if corp is below 5c")
          (click-prompt state :corp "Take 1 bad publicity"))
        "Corp took 1 BP without getting a prompt")))

(deftest mobius-second-run-triggered
    ;; Second run triggered
    (do-game
      (new-game {:runner {:deck [(qty "Möbius" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (play-from-hand state :runner "Möbius")
      (run-continue-until state :success)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (not (:run @state)) "Run is over")
      (is (no-prompt? state :runner) "No prompt to run a third time")))

(deftest mobius-gain-4-credits-after-succesful-second-run
    ;; Gain 4 credits after succesful second run
    (do-game
      (new-game {:runner {:deck [(qty "Möbius" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-from-hand state :runner "Möbius")
      (run-continue-until state :success)
      (is (= 5 (:credit (get-runner))))
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (= 9 (:credit (get-runner))))))

(deftest mobius-no-second-run-if-first-is-unsuccesful
    ;; No second run if first is unsuccesful
    (do-game
      (new-game {:runner {:deck [(qty "Möbius" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (play-from-hand state :runner "Möbius")
      (run-continue state)
      (run-jack-out state)
      (is (no-prompt? state :runner) "No option to run again on unsuccessful run")))

(deftest mobius-normal-rnd-run-does-not-gain-cred
    ;; Normal rnd run does not gain cred
    (do-game
      (new-game {:runner {:deck [(qty "Möbius" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (play-from-hand state :runner "Möbius")
      (run-continue-until state :success)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (changed? [(:credit (get-runner)) 0]
            (run-empty-server state :rd)
            (click-prompt state :runner "No action"))
          "Normal run on R&D didn't give any credits")))

(deftest mobius-recurred-use
    ;; Recurred use
    (do-game
      (new-game {:runner {:deck ["Möbius" "Déjà Vu"]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-from-hand state :runner "Möbius")
      (run-continue state)
      (run-continue state)
      (is (= 5 (:credit (get-runner))))
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (= 9 (:credit (get-runner))))
      (play-from-hand state :runner "Déjà Vu")
      (click-prompt state :runner (find-card "Möbius" (:discard (get-runner))))
      (is (no-prompt? state :runner) "Recurring a non-virus card stops Déjà Vu prompting further")
      (is (= 1 (count (:hand (get-runner)))))
      (play-from-hand state :runner "Möbius")
      (run-continue state)
      (run-continue state)
      (is (= 7 (:credit (get-runner))))
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No action")
      (is (= 11 (:credit (get-runner))))))

(deftest modded
  ;; Modded - Install a program or piece of hardware at a 3 credit discount
  (do-game
    (new-game {:runner {:deck [(qty "Modded" 2)
                               "HQ Interface"
                               "Nerve Agent"
                               "Earthrise Hotel"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Modded")
    (click-card state :runner (find-card "Earthrise Hotel" (:hand (get-runner))))
    (is (empty? (get-resource state)) "Can't install resources with Modded")
    (click-card state :runner (find-card "HQ Interface" (:hand (get-runner))))
    (is (= 1 (count (get-hardware state))) "Installed HQ Interface")
    (is (= 4 (:credit (get-runner))) "Paid 1 credit instead of 4")
    (play-from-hand state :runner "Modded")
    (click-card state :runner (find-card "Nerve Agent" (:hand (get-runner))))
    (is (= 1 (count (get-program state))) "Installed Nerve Agent")
    (is (= 4 (:credit (get-runner))) "Paid 0 credits")))

(deftest moshing
  (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Moshing" (qty "Lamprey" 3)]}})
      (take-credits state :corp)
      (is (= 0 (count (:discard (get-runner)))) "Runner has no cards in heap")
      (is (= 4 (count (:hand (get-runner)))) "Runner starts with 4 cards")
      (is (not (find-card "Sure Gamble" (:hand (get-runner)))) "Runner does not have Sure Gamble in grip")
      (play-from-hand state :runner "Moshing")
      (is (= 0 (count (:discard (get-runner)))) "Moshing is still in play")
      (dotimes [card 3] (click-card state :runner (nth (:hand (get-runner)) card)))
      (is (= 4 (count (:discard (get-runner)))) "Runner trashed 3 cards and discarded Moshing")
      (is (= 3 (count (:hand (get-runner)))) "Runner draws 3 cards")
      (is (find-card "Sure Gamble" (:hand (get-runner))) "Runner drew Sure Gamble")))

(deftest mutual-favor
  ;; Mutual Favor
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 4)]}
                 :runner {:deck [(qty "Aumakua" 3)]
                          :hand ["Mutual Favor"]}})
      (take-credits state :corp)
      (let [original-deck-count (count (:deck (get-runner)))
            original-hand-count (count (:hand (get-runner)))]
        (run-empty-server state :archives)
        (play-from-hand state :runner "Mutual Favor")
        (click-prompt state :runner "Aumakua")
        (click-prompt state :runner "No")
        (is (= 5 (:credit (get-runner))) "Spent 0 credits")
        (is (= 0 (count (get-program state))) "Pulled card was not installed")
        (is (= (+ original-deck-count -1) (count (:deck (get-runner)))) "Took card from deck...")
        (is (= (+ original-hand-count 1 -1) (count (:hand (get-runner)))) "...into hand -one played, +one drawn")))
  (testing "Basic test - install"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 4)]}
                 :runner {:deck [(qty "Aumakua" 3)]
                          :hand ["Mutual Favor"]}})
      (take-credits state :corp)
      (let [original-deck-count (count (:deck (get-runner)))
            original-hand-count (count (:hand (get-runner)))]
        (run-empty-server state :archives)
        (play-from-hand state :runner "Mutual Favor")
        (click-prompt state :runner "Aumakua")
        (click-prompt state :runner "Yes")
        (is (= (+ 5 -3) (:credit (get-runner))) "Spent 3 credits")
        (is (= 1 (count (get-program state))) "Pulled card was installed")
        (is (= (+ original-deck-count -1) (count (:deck (get-runner)))) "Took card from deck...")
        (is (= (+ original-hand-count -1) (count (:hand (get-runner)))) "...into play -one played"))))
  (testing "Basic test - no prompt when not enough credits to install"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 4)]}
                 :runner {:deck [(qty "Aumakua" 3)]
                          :hand ["Mutual Favor"]
                          :credits 0}})
      (take-credits state :corp)
      (let [original-deck-count (count (:deck (get-runner)))
            original-hand-count (count (:hand (get-runner)))]
        (run-empty-server state :archives)
        (play-from-hand state :runner "Mutual Favor")
        (click-prompt state :runner "Aumakua")
        (is (= 0 (:credit (get-runner))) "Spent 0 credits")
        (is (= 0 (count (get-program state))) "Pulled card was not installed")
        (is (= (+ original-deck-count -1) (count (:deck (get-runner)))) "Took card from deck...")
        (is (= (+ original-hand-count 1 -1) (count (:hand (get-runner)))) "...into hand -one played, +one drawn"))))
  (testing "Basic test - no prompt when no successful run"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 4)]}
                 :runner {:deck [(qty "Aumakua" 3)]
                          :hand ["Mutual Favor"]}})
      (take-credits state :corp)
      (let [original-deck-count (count (:deck (get-runner)))
            original-hand-count (count (:hand (get-runner)))]
        (play-from-hand state :runner "Mutual Favor")
        (click-prompt state :runner "Aumakua")
        (is (= 5 (:credit (get-runner))) "Spent 0 credits")
        (is (= 0 (count (get-program state))) "Pulled card was not installed")
        (is (= (+ original-deck-count -1) (count (:deck (get-runner)))) "Took card from deck...")
        (is (= (+ original-hand-count 1 -1) (count (:hand (get-runner)))) "...into hand -one played, +one drawn")))))

(deftest net-celebrity-pay-credits-prompt
    ;; Pay-credits prompt
    (do-game
      (new-game {:runner {:deck ["Net Celebrity" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Net Celebrity")
      (play-from-hand state :runner "Corroder")
      (let [nc (first (get-in @state [:runner :current]))
            cor (get-program state 0)]
        (is (changed? [(:credit (get-runner)) -1]
              (card-ability state :runner cor 1))
            "Paid credit outside of run")
        (run-on state :hq)
        (card-ability state :runner cor 1)
        (is (changed? [(:credit (get-runner)) 0]
              (click-card state :runner nc))
            "Used 1 credit from Net Celebrity"))))

(deftest networking
  ;; Networking
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Networking"]}})
    (take-credits state :corp)
    (gain-tags state :runner 4)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Networking")
      (is (= 3 (count-tags state)) "Runner should lose 1 tag")
      (click-prompt state :runner "Yes")
      (is (= (dec credits) (:credit (get-runner))) "Runner should spend 1 on Networking ability")
      (is (zero? (count (:discard (get-runner)))) "Runner's discard should be empty")
      (is (= "Networking" (-> (get-runner) :hand first :title))))
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Networking")
      (is (= 2 (count-tags state)) "Runner should lose 1 tag")
      (click-prompt state :runner "No")
      (is (= credits (:credit (get-runner))) "Runner should spend 1 on Networking ability")
      (is (= 1 (count (:discard (get-runner)))) "Runner's discard should be empty")
      (is (= "Networking" (-> (get-runner) :discard first :title)) "Networking should be in heap"))))

(deftest notoriety
  ;; Notoriety - Run all 3 central servers successfully and play to gain 1 agenda point
  (do-game
    (new-game {:corp {:deck ["Hedge Fund"]}
               :runner {:deck ["Notoriety"]}})
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Notoriety")
    (is (= 1 (count (:scored (get-runner)))) "Notoriety moved to score area")
    (is (= 1 (:agenda-point (get-runner))) "Notoriety scored for 1 agenda point")))

(deftest office-supplies
  ;; Office Supplies
  (letfn [(office-supplies-test [link]
            (do-game
              (new-game {:runner {:deck [(qty "Office Supplies" 2)
                                         (qty "Access to Globalsec" 100)]}})
              (take-credits state :corp)
              (core/gain state :runner :credit 1000 :click link)
              (starting-hand state :runner (concat (repeat 2 "Office Supplies")
                                                   (repeat 4 "Access to Globalsec")))
              (dotimes [_ link]
                (play-from-hand state :runner "Access to Globalsec"))
              (let [credits (:credit (get-runner))]
                (play-from-hand state :runner "Office Supplies")
                (is (= (- credits (- 4 link)) (:credit (get-runner)))))
              (let [credits (:credit (get-runner))]
                (click-prompt state :runner "Gain 4 [Credits]")
                (is (= (+ 4 credits) (:credit (get-runner))) (str "Runner should gain " (utils/quantify link "credit"))))
              (play-from-hand state :runner "Office Supplies")
              (let [grip (-> (get-runner) :hand count)]
                (click-prompt state :runner "Draw 4 cards")
                (is (= (+ 4 grip) (-> (get-runner) :hand count)) "Runner should draw 4 cards"))))]
    (doall (map office-supplies-test (range 5)))))

(deftest on-the-lam-vs-tags
    ;; vs tags
    (do-game
      (new-game {:corp {:deck ["SEA Source"]}
                 :runner {:deck ["Daily Casts" "On the Lam"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "On the Lam")
      (click-card state :runner (get-resource state 0))
      (run-empty-server state "Archives")
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "On the Lam")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "1")
      (is (zero? (count-tags state)) "Runner should avoid tag")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap")))

(deftest on-the-lam-vs-damage
    ;; vs damage
    (do-game
      (new-game {:corp {:deck ["Show of Force"]}
                 :runner {:deck ["Daily Casts" "On the Lam"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "On the Lam")
      (click-card state :runner (get-resource state 0))
      (take-credits state :runner)
      (play-and-score state "Show of Force")
      (click-prompt state :runner "On the Lam")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "2")
      (is (zero? (count-tags state)) "Runner should avoid all meat damage")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap")))

(deftest on-the-lam-should-stay-active-after-apocalypse-5195
    ;; should stay active after Apocalypse #5195
    (do-game
      (new-game {:corp {:deck ["SEA Source"]}
                 :runner {:hand ["Daily Casts" "On the Lam" "Apocalypse"]
                          :credits 10}})
      (take-credits state :corp)
      (core/gain state :runner :click 10)
      (play-from-hand state :runner "Daily Casts")
      (play-from-hand state :runner "On the Lam")
      (click-card state :runner (get-resource state 0))
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (play-from-hand state :runner "Apocalypse")
      (let [facedowns (filter :facedown (core/all-installed state :runner))
            casts (find-card "Daily Casts" facedowns)
            otl (find-card "On the Lam" facedowns)]
        (is casts "Casts facedown")
        (is (not otl) "OTL not facedown"))
      (take-credits state :runner)
      (play-from-hand state :corp "SEA Source")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (click-prompt state :runner "On the Lam")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "1")
      (is (zero? (count-tags state)) "Runner should avoid tag")))

(deftest out-of-the-ashes-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp {:deck ["Kala Ghoda Real TV" "Underway Renovation"]}
                 :runner {:deck [(qty "Out of the Ashes" 6)]}})
      (play-from-hand state :corp "Underway Renovation" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Out of the Ashes")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (run-continue state)
      (trash-from-hand state :runner "Out of the Ashes")
      (trash-from-hand state :runner "Out of the Ashes")
      (trash-from-hand state :runner "Out of the Ashes")
      (trash-from-hand state :runner "Out of the Ashes")
      (is (zero? (count (:hand (get-runner)))))
      (is (= 5 (count (:discard (get-runner)))))
      (take-credits state :runner)
      (let [underway (get-content state :remote1 0)]
        (click-advance state :corp (refresh underway)))
      (is (= 6 (count (:discard (get-runner)))))
      (take-credits state :corp)
      ;; remove 5 Out of the Ashes from the game
      (dotimes [_ 5]
        (is (not (no-prompt? state :runner)))
        (click-prompt state :runner "Yes")
        (click-prompt state :runner "Archives")
        (run-continue-until state :success))
      (click-prompt state :runner "No")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 5 (count (:rfg (get-runner)))))
      (is (not (:runner-phase-12 @state)) "Start of turn triggers have finished")
      (take-credits state :runner)
      (take-credits state :corp)
      ;; ensure that if you decline the rfg, game will still ask the next turn
      (is (not (no-prompt? state :runner)))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (run-continue state)
      (is (zero? (count (:discard (get-runner)))))
      (is (= 6 (count (:rfg (get-runner)))))
      (is (not (:runner-phase-12 @state)) "Start of turn triggers have finished")))

(deftest out-of-the-ashes-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Blacklist"]}
                 :runner {:discard [(qty "Out of the Ashes" 6)]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (take-credits state :runner)
      (take-credits state :corp)
      (is (no-prompt? state :runner) "OOTA prompt did not come up")))

(deftest out-of-the-ashes-interactive-with-other-start-of-turn-triggers
    ;; Interactive with other start of turn triggers
    (do-game
     (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]}
                :runner {:hand [(qty "Out of the Ashes" 3) "Rezeki" "Data Folding"]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Rezeki")
     (play-from-hand state :runner "Data Folding")
     (trash-from-hand state :runner "Out of the Ashes")
     (trash-from-hand state :runner "Out of the Ashes")
     (trash-from-hand state :runner "Out of the Ashes")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (= `("Rezeki" "Data Folding" "Out of the Ashes" "Done") (prompt-titles :runner)) "Out of the Ashes reduced to a single choice")
     (click-prompt state :runner "Rezeki")
     (click-prompt state :runner "Out of the Ashes")
     (click-prompt state :runner "Yes")
     (click-prompt state :runner "Archives")
     (run-continue state)
     (run-continue state)
     (click-prompt state :runner "No")
     (is (= 2 (count (:discard (get-runner)))))
     (is (no-prompt? state :runner) "No further start of turn prompts")
     (is (no-prompt? state :corp) "No waiting for start of turn triggers prompt")
     (is (not (:runner-phase-12 @state)) "Start of turn triggers have finished")))

(deftest overclock
  ;; Overclock - Gain 5 temporary credits
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"]}
               :runner {:deck ["Overclock"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Overclock")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-continue state)
    (is (= 4 (:credit (get-runner))))
    (click-prompt state :runner "Pay 5 [Credits] to trash") ; choose to trash Eve
    (dotimes [_ 5]
      (click-card state :runner "Overclock"))
    (is (and (zero? (count (:hand (get-corp))))
             (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Eve in Archives")
    (is (= 4 (:credit (get-runner))))))

(deftest overclock-earth-station
  ;;ensure that overclock can't pay for the earth station ability
  (do-game
    (new-game {:runner {:hand ["Overclock" "Sure Gamble"]}
               :corp {:id "Earth Station: SEA Headquarters" :hand ["PAD Campaign"]}})
    (card-ability state :corp (get-in @state [:corp :identity]) 0)
    (is (:flipped (get-in @state [:corp :identity])) "Earth station is on flip side")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (is (changed? [(:credit (get-runner)) -7]
          (play-from-hand state :runner "Overclock")
          (click-prompt state :runner "Server 1"))
        "Paid for earth station")
    (is (no-prompt? state :runner) "waiting on earth station payment prompt")))

(deftest paper-tripping
  ;; Paper Tripping
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Paper Tripping"]}})
    (take-credits state :corp)
    (gain-tags state :runner 100)
    (play-from-hand state :runner "Paper Tripping")
    (is (zero? (count-tags state)) "Runner should lose all tags")))

(deftest peace-in-our-time-no-runs-allowed
    ;; no runs allowed
    (do-game
      (new-game {:runner {:deck ["Peace in Our Time"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
      (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
      (play-from-hand state :runner "Peace in Our Time")
      (is (= 13 (:credit (get-corp))) "Corp gains 5 credits")
      (is (= 14 (:credit (get-runner))) "Runner gains 10 credits")
      (run-on state "HQ" {:wait-at-initiation true})
      (is (not (:run @state)) "Not allowed to make a run")))

(deftest peace-in-our-time-cannot-play-if-agenda-scored-previously
    ;; cannot play if agenda scored previously
    (do-game
      (new-game {:runner {:deck ["Peace in Our Time"]}
                 :corp {:hand ["Hostile Takeover"]}})
      (play-and-score state "Hostile Takeover")
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
      (play-from-hand state :runner "Peace in Our Time")
      (is (= 5 (:credit (get-runner))) "Runner cannot play Peace in Our time, still has 5 credits")))

(deftest pinhole-threading
  (do-game
    ;;can't access in same server, can't steal/trash agenda
    (new-game {:runner {:hand ["Imp" "Pinhole Threading"]}
               :corp {:hand ["Project Beale" "PAD Campaign"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "Project Beale" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (play-from-hand state :runner "Pinhole Threading")
    (click-prompt state :runner "Server 1")
    (run-continue state)
    (run-continue state)
    (click-card state :runner "Project Beale")
    (is (= ["No action"] (mapv :value (:choices (prompt-map :runner)))))))

(deftest planned-assault
  ;; Planned Assault
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck ["Account Siphon"]
                        :hand ["Planned Assault"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Planned Assault")
    (is (not (:run @state)) "No run should be initiated yet")
    (click-prompt state :runner "Account Siphon")
    (is (:run @state) "A run should be initiated")
    (run-continue state)
    (is (= 2 (:click (get-runner))) "Runner should only spend 2 clicks on Planned Assault")))

(deftest political-graffiti
  ;; Political Graffiti - swapping with Turntable works / purging viruses restores points
  (do-game
      (new-game {:corp {:deck ["Breaking News" "Chronos Project"]}
                 :runner {:deck ["Turntable" "Political Graffiti"]}})
      (play-from-hand state :corp "Breaking News" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (is (= 1 (:agenda-point (get-corp))))
      (take-credits state :corp)
      (play-run-event state "Political Graffiti" :archives)
      (click-card state :runner (find-card "Breaking News" (:scored (get-corp))))
      (is (zero? (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
      (play-from-hand state :runner "Turntable")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Breaking News" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))))
      (is (zero? (:agenda-point (get-runner))))
      (take-credits state :runner)
      (purge state :corp)
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 1 (:agenda-point (get-runner))))))

(deftest political-graffiti-forfeiting-agenda-with-political-graffiti-does-not-refund-double-points-issue-2765
    ;; Forfeiting agenda with Political Graffiti does not refund double points. Issue #2765
    (do-game
      (new-game {:corp {:deck ["Project Kusanagi" "Corporate Town"]}
                 :runner {:deck ["Political Graffiti"]}})
      (play-from-hand state :corp "Corporate Town" "New remote")
      (play-and-score state "Project Kusanagi")
      (is (zero? (:agenda-point (get-corp))))
      (take-credits state :corp)
      (play-run-event state "Political Graffiti" :archives)
      (let [project-kusanagi (get-scored state :corp 0)
            corporate-town (get-content state :remote1 0)]
        (click-card state :runner project-kusanagi)
        (is (= -1 (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
        (take-credits state :runner)
        (rez state :corp corporate-town {:expect-rez false})
        (click-card state :corp (refresh project-kusanagi)))
      (is (zero? (:agenda-point (get-corp))) "Forfeiting agenda did not refund extra agenda points")
      (is (= 1 (count (:discard (get-runner)))) "Political Graffiti is in the Heap")))

(deftest populist-rally
  ;; Populist Rally
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Data Dealer" "Populist Rally"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Populist Rally")
    (is (= ["Data Dealer" "Populist Rally"] (->> (get-runner) :hand (map :title) sort))
        "Populist Rally shouldn't be played")
    (play-from-hand state :runner "Data Dealer")
    (play-from-hand state :runner "Populist Rally")
    (take-credits state :runner)
    (is (= 2 (:click (get-corp))) "Corp should gain only 2 clicks this turn")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 3 (:click (get-corp))) "Corp should gain 3 clicks as normal in later turns")))

(deftest power-nap
  ;; Power Nap
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Power Nap" 3)]}})
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Power Nap")
      (is (= (+ credits 2) (:credit (get-runner))) "Runner should gain 2"))
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Power Nap")
      (is (= (+ credits 3) (:credit (get-runner)))
          "Runner should gain 3 for 1 double in heap"))
    (let [credits (:credit (get-runner))]
      (play-from-hand state :runner "Power Nap")
      (is (= (+ credits 4) (:credit (get-runner)))
          "Runner should gain 4 for 2 doubles in heap"))))

(deftest power-to-the-people
  ;; Power to the People - Gain 7c the first time you access an agenda
  (do-game
    (new-game {:corp {:deck ["NAPD Contract" "Hostile Takeover" "PAD Campaign"]}
               :runner {:deck ["Power to the People"]}})
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (core/lose state :runner :credit 2)
    (play-from-hand state :runner "Power to the People")
    (is (= 3 (:credit (get-runner))) "Can't afford to steal NAPD")
    (is (changed? [(:credit (get-runner)) 0]
          (run-empty-server state "Server 2"))
        "No credits gained from accessing non-agenda cards")
    (click-prompt state :runner "No action")
    (run-empty-server state "Server 1")
    (is (= 10 (:credit (get-runner))) "Gained 7c on access, can steal NAPD")
    (click-prompt state :runner "Pay to steal")
    (is (= 2 (:agenda-point (get-runner))) "Stole agenda")
    (is (= 6 (:credit (get-runner))))
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (= 6 (:credit (get-runner))) "No credits gained from 2nd agenda access")))

(deftest prey-full-test
    ;; Full test
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Enigma"]}
                 :runner {:hand [(qty "Prey" 2) (qty "Clone Chip" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Enigma" "R&D")
      (rez state :corp (get-ice state :hq 0))
      (rez state :corp (get-ice state :rd 0))
      (take-credits state :corp)
      (core/gain state :runner :click 5)
      (dotimes [_ 3]
        (play-from-hand state :runner "Clone Chip"))
      (play-from-hand state :runner "Prey")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (run-continue state)
      (is (get-ice state :hq 0) "Ice Wall should not be trashed yet")
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-hardware state 0))
      (is (not (get-ice state :hq 0)) "Ice Wall should be trashed")
      (run-continue state)
      (play-from-hand state :runner "Prey")
      (click-prompt state :runner "R&D")
      (run-continue state)
      (run-continue state)
      (run-continue state)
      (is (get-ice state :rd 0) "Enigma should not be trashed yet")
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (get-hardware state 1))
      (is (not (get-ice state :rd 0)) "Enigma should be trashed")))

(deftest prey-inteaction-with-reaper
  ;; Prey - Multiple cards trashed trigger Reaper
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Enigma"]}
                :runner {:hand ["Prey" "Reaver" (qty "No Free Lunch" 2)]
                         :deck ["Sure Gamble"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (core/gain state :runner :click 1)
    (play-from-hand state :runner "No Free Lunch")
    (play-from-hand state :runner "No Free Lunch")
    (play-from-hand state :runner "Reaver")
    (play-from-hand state :runner "Prey")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (changed? [(count (:hand (get-runner))) 1]
          (click-card state :runner (get-resource state 0))
          (click-card state :runner (get-resource state 1)))
        "Drew 1 card from Reaver")))

(deftest prey-correct-prompt-when-ice-has-0-strength-issue-4743
    ;; Correct prompt when ice has 0 strength. Issue #4743
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Burke Bugs"]}
                 :runner {:hand ["Prey" (qty "Clone Chip" 3)]}})
      (play-from-hand state :corp "Burke Bugs" "HQ")
      (take-credits state :corp)
      (dotimes [_ 3]
        (play-from-hand state :runner "Clone Chip"))
      (play-from-hand state :runner "Prey")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (get-ice state :hq 0) "Ice Wall should not be trashed yet")
      (is (= "Trash Burke Bugs?" (:msg (prompt-map :runner)))
          "Runner has correct prompt")
      (click-prompt state :runner "Yes")
      (is (find-card "Burke Bugs" (:discard (get-corp))) "Burke Bugs is trashed")
      (is (not (get-ice state :hq 0)) "Burke Bugs is trashed")))

(deftest prey-prompt-should-be-shown-only-until-ice-is-trashed
    ;; Prompt should be shown only until ice is trashed.
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Burke Bugs" "Ice Wall"]}
                 :runner {:hand ["Prey" (qty "Clone Chip" 3)]}})
      (play-from-hand state :corp "Burke Bugs" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (dotimes [_ 3]
        (play-from-hand state :runner "Clone Chip"))
      (play-from-hand state :runner "Prey")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (rez state :corp (get-ice state :hq 1))
      (run-continue state)
      (run-continue state)
      (is (get-ice state :hq 1) "Ice Wall should not be trashed yet")
      (is (= "Trash 1 installed card to trash Ice Wall?" (:msg (prompt-map :runner)))
          "Runner has correct prompt")
      (click-prompt state :runner "Yes")
      (click-card state :runner (get-hardware state 0))
      (is (find-card "Ice Wall" (:discard (get-corp))) "Ice Wall is trashed")
      (is (not (get-ice state :hq 1)) "Ice Wall is trashed")
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (run-continue state)
      (is (not (= "Trash Burke Bugs?" (:msg (prompt-map :runner))))
          "Runner has no prompt trash ice")))

(deftest privileged-access
  (do-game
    (new-game {:runner {:hand ["Privileged Access"]
                        :discard ["Verbal Plasticity" "Rachel Beckman"]}})
    (take-credits state :corp)
    (gain-tags state :runner 1)
    (play-from-hand state :runner "Privileged Access")
    (is (not (:run @state)) "Cannot play Privileged Access when tagged")
    (remove-tag state :runner)
    (play-from-hand state :runner "Privileged Access")
    (run-continue state)
    (is (changed? [(count-tags state) 1]
                  (run-continue state)
                  (is (= ["Verbal Plasticity" "Done"] (prompt-titles :runner)))
                  (is (changed? [(:credit (get-runner)) -1]
                                (click-prompt state :runner "Verbal Plasticity"))
                      "Install Verbal Plasticity from heap for 2 credits less")
                  (is (= "Verbal Plasticity" (get-title (get-resource state 0)))))
        "Gain a tag from successful run on Archives")
    (is (not (:run @state)) "Run ended")))

(deftest privileged-access-threat
  (do-game
    (new-game {:corp {:hand ["Obokata Protocol"]}
               :runner {:hand ["Privileged Access"]
                        :discard ["Cleaver" "Orca"]}})
    (play-and-score state "Obokata Protocol")
    (take-credits state :corp)
    (play-run-event state "Privileged Access" :archives)
    (click-prompt state :runner "Privileged Access (resource)")
    (click-prompt state :runner "Done")
    (is (= ["Cleaver" "Done"] (prompt-titles :runner)))
    (click-prompt state :runner "Cleaver")
    (is (= "Cleaver" (get-title (get-program state 0))))
    (is (not (:run @state)) "Run ended")))

(deftest privileged-access-jesminder
  (do-game
    (new-game {:runner {:id "Jesminder Sareen: Girl Behind the Curtain"
                        :hand ["Privileged Access"]
                        :discard ["Verbal Plasticity"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Privileged Access")
    (run-continue state)
    (run-continue state)
    (is (no-prompt? state :runner))
    (is (zero? (count-tags state)) "No tag gained during the run")))

(deftest privileged-access-jarogniew-mercs
  (do-game
    (new-game {:corp {:hand ["Obokata Protocol"]}
               :runner {:hand ["Privileged Access"]
                        :discard ["Jarogniew Mercs" "Marjanah"]}})
    (play-and-score state "Obokata Protocol")
    (take-credits state :corp)
    (play-from-hand state :runner "Privileged Access")
    (run-continue state)
    (run-continue state)
    ;; resolving program installation first
    (click-prompt state :runner "Privileged Access (program)")
    (click-prompt state :runner "Marjanah")
    (click-prompt state :runner "Jarogniew Mercs")
    (is (no-prompt? state :runner))))

(deftest process-automation
  ;; Process Automation
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Process Automation"]}})
    (take-credits state :corp)
    (let [credits (:credit (get-runner))
          hand (dec (count (:hand (get-runner))))]
      (play-from-hand state :runner "Process Automation")
      (is (= (+ 2 credits) (:credit (get-runner))) "Should gain 2 credits")
      (is (= (inc hand) (count (:hand (get-runner)))) "Should draw 1 card"))))

(deftest push-your-luck-corp-guesses-correctly
    ;; Corp guesses correctly
    (do-game
      (new-game {:runner {:hand ["Push Your Luck"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Push Your Luck")
      (click-prompt state :runner "3")
      (click-prompt state :corp "Odd")
      (is (zero? (:credit (get-runner))) "Corp guessed correctly")))

(deftest push-your-luck-corp-guesses-incorrectly
    ;; Corp guesses incorrectly
    (do-game
      (new-game {:runner {:hand ["Push Your Luck"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Push Your Luck")
      (click-prompt state :runner "3")
      (click-prompt state :corp "Even")
      (is (= 6 (:credit (get-runner))) "Corp guessed incorrectly")))

(deftest push-your-luck-interaction-with-government-investigations
    ;; Interaction with Government Investigations
    (do-game
      (new-game {:runner {:hand ["Push Your Luck" "Government Investigations"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Government Investigations")
      (play-from-hand state :runner "Push Your Luck")
      (is (= ["0" "1" "3"] (prompt-buttons :runner)) "Runner can't choose 2 because of Government Investigations")))

(deftest pushing-the-envelope
  ;; Run. Add 2 strength to each installer breaker.
  (do-game
    (new-game {:runner {:deck [(qty "Pushing the Envelope" 3) (qty "Corroder" 2) "Atman"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 20)
    (core/gain state :runner :click 10)
    (draw state :runner)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Atman")
    (click-prompt state :runner "0")
    (let [atman (get-program state 1)
          corr (get-program state 0)]
      (is (zero? (get-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (get-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :runner "Pushing the Envelope")
      (click-prompt state :runner "Archives")
      (run-continue state)
      (run-continue state)
      ; 3 cards in hand - no boost
      (is (zero? (get-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (get-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :runner "Pushing the Envelope")
      (click-prompt state :runner "Archives")
      (run-continue state)
      ; 2 cards in hand - boost
      (is (= 2 (get-strength (refresh atman))) "Atman 2 current strength")
      (is (= 4 (get-strength (refresh corr))) "Corroder 2 current strength")
      (run-continue state)
      (is (zero? (get-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (get-strength (refresh corr))) "Corroder 2 current strength"))))

(deftest quality-time
  ;; Quality Time
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck [(qty "Sure Gamble" 5)]
                        :hand ["Quality Time"]}})
    (take-credits state :corp)
    (is (= 1 (count (:hand (get-runner)))) "Runner should have 1 card in hand")
    (play-from-hand state :runner "Quality Time")
    (is (= 5 (count (:hand (get-runner)))) "Runner should draw 5 cards")))

(deftest queen-s-gambit
  ;; Check that Queen's Gambit prevents access of card #1542
  (do-game
    (new-game {:corp {:deck [(qty "PAD Campaign" 2)]}
               :runner {:deck ["Queen's Gambit"]}})
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Queen's Gambit")
    (let [pad (get-content state :remote1 0)
          runner-creds (:credit (get-runner))]
      (click-prompt state :runner "3")
      (click-card state :runner pad)
      (is (= (+ runner-creds 6) (:credit (get-runner))) "Gained 6 credits from Queen's Gambit")
      (is (= 3 (get-counters (refresh pad) :advancement)) "3 advancement counters placed on PAD Campaign by Queen's Gambit")
      (is (not (core/can-access? state :runner (refresh pad))) "Cannot access PAD Campgain")
      (run-empty-server state "Server 1")
      (is (not (:run @state)) "Run ended since no cards could be accessed"))
    (let [other-pad (get-content state :remote2 0)]
      (is (core/can-access? state :runner other-pad)) "Not prevented from accessing other cards")
    (take-credits state :runner)
    (take-credits state :corp)
    (let [pad (get-content state :remote1 0)
          runner-creds (:credit (get-runner))]
      (run-empty-server state "Server 1")
      (is (core/can-access? state :runner (refresh pad)) "Can access PAD Campgain next turn")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= (- runner-creds 4) (:credit (get-runner))) "Paid 4 credits to trash PAD Campaign"))))

(deftest quest-completed
  ;; Quest Completed
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover"]}
               :runner {:hand ["Quest Completed"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (click-prompt state :runner "No action")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Quest Completed")
    (click-card state :runner "Hostile Takeover")
    (click-prompt state :runner "Steal")))

(deftest raindrops-cut-stone
  (do-game
    (new-game {:corp {:hand ["Komainu"]}
               :runner {:hand ["Raindrops Cut Stone", (qty "Sure Gamble" 5)]
                        :deck [(qty "Sure Gamble" 15)]}})
    (play-from-hand state :corp "Komainu" "HQ")
    (let [kmu (get-ice state :hq 0)]
      (rez state :corp kmu)
      (take-credits state :corp)
      (play-from-hand state :runner "Raindrops Cut Stone")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (is (= 5 (count (:subroutines (refresh kmu)))) "5 cards in hand")
      (fire-subs state (refresh kmu))
      (is (zero? (count (:hand (get-runner)))) "hand wiped by komainu")
      (run-continue state)
      (is (changed? [(:credit (get-runner)) +3
             (count (:hand (get-runner))) +5]
            (run-continue state))
          "gained 3 credits from raindrop"))))

(deftest raindrops-cut-stone-end-the-run-sub
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Raindrops Cut Stone"]
                        :deck [(qty "Sure Gamble" 5)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (let [iw (get-ice state :hq 0)]
      (rez state :corp iw)
      (take-credits state :corp)
      (play-from-hand state :runner "Raindrops Cut Stone")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (is (changed? [(:credit (get-runner)) +3
             (count (:hand (get-runner))) 1]
            (fire-subs state (refresh iw)))
          "gained 3 credits from raindrop"))))

(deftest raindrops-cut-stone-no-sub-fired
  (do-game
    (new-game {:corp {:hand ["Ice Wall"]}
               :runner {:hand ["Raindrops Cut Stone"]
                        :deck [(qty "Sure Gamble" 5)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Raindrops Cut Stone")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (run-continue state)
    (is (changed? [(:credit (get-runner)) +3
                   (count (:hand (get-runner))) 0]
          (click-prompt state :runner "No action"))
        "gained 3 credits from raindrop")))

(deftest raindrops-cut-stone-forced-encounter
  (do-game
    (new-game {:corp {:hand ["Ganked!" "Enigma"]}
               :runner {:hand ["Raindrops Cut Stone"]
                        :deck [(qty "Ika" 5)]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Raindrops Cut Stone")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (let [enigma (get-ice state :hq 0)]
      (rez state :corp (refresh enigma))
      (run-continue-until state :success)
      (is (last-log-contains? state "Runner accesses Ganked!") "Ganked! message printed to log")
      (click-prompt state :corp "Yes")
      (click-card state :corp "Enigma")
      (let [enc (:ice (core/get-current-encounter state))]
        (is (= "Enigma" (:title enc)) "Encountering enigma via ganked")
        ;; should draw 2 from raindrops when we fire subs
        (is (changed? [(count (:hand (get-runner))) 2]
              (fire-subs state enc))
            "Drew 2 cards from raindrops cut stone")))))

;; Rebirth
(let [akiko "Akiko Nisei: Head Case"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"]
  (deftest rebirth-kate-discount
    ;; Rebirth - Kate's discount applies after rebirth
    (do-game
     (new-game {:runner {:id professor
                         :deck ["Magnum Opus" "Rebirth"]}
                :options {:start-as :runner}})
     (play-from-hand state :runner "Rebirth")
     (is (= (first (prompt-titles :runner)) akiko) "List is sorted")
     (is (every? #(some #{%} (prompt-titles :runner))
                 [kate kit]))
     (is (not-any? #(some #{%} (prompt-titles :runner))
                   [professor whizzard jamie]))
     (click-prompt state :runner kate)
     (is (= kate (-> (get-runner) :identity :title)))
     (is (= 1 (get-link state)) "1 link")
     (is (empty? (:discard (get-runner))))
     (is (= "Rebirth" (-> (get-runner) :rfg first :title)))
     (is (changed? [(:credit (get-runner)) -4]
           (play-from-hand state :runner "Magnum Opus")))))

  (deftest rebirth-kate-install-hardware-before-prevents-discount
    ;; Rebirth - Installing Hardware before does prevent Kate's discount"
    (do-game
     (new-game {:runner {:id professor
                         :deck ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"]}
                :options {:start-as :runner}})
     (play-from-hand state :runner "Clone Chip")
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner kate)
     (is (= kate (get-in (get-runner) [:identity :title])) "Rebirthed into Kate")
     (is (changed? [(:credit (get-runner)) -1]
           (play-from-hand state :runner "Akamatsu Mem Chip"))
         "Discount not applied for 2nd install")))

  (deftest rebirth-kate-install-resource-before-get-discount
    ;; Rebirth - Installing Resource before does not prevent Kate's discount
    (do-game
     (new-game {:runner {:id professor
                         :deck ["Akamatsu Mem Chip" "Rebirth" "Same Old Thing"]}
                :options {:start-as :runner}})
     (play-from-hand state :runner "Same Old Thing")
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner kate)
     (is (= kate (get-in (get-runner) [:identity :title])) "Rebirthed into Kate")
     (is (changed? [(:credit (get-runner)) 0]
           (play-from-hand state :runner "Akamatsu Mem Chip"))
         "Discount is applied for 2nd install (since it is the first Hardware / Program)")))

  (deftest rebirth-whizzard
    ;; Rebirth - Whizzard works after rebirth
    (do-game
     (new-game {:corp {:deck ["Ice Wall"]}
                :runner {:id reina
                         :deck ["Rebirth"]}})
     (play-from-hand state :corp "Ice Wall" "R&D")
     (take-credits state :corp)
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner whizzard)
     (card-ability state :runner (:identity (get-runner)) 0)
     (is (= 6 (:credit (get-runner))) "Took a Whizzard credit")
     (is (changed? [(:credit (get-corp)) -1]
           (rez state :corp (get-ice state :rd 0)))
         "Reina is no longer active")))

  (deftest rebirth-lose-link
    ;; Rebirth - Lose link from ID
    (do-game
     (new-game {:runner {:id kate
                         :deck ["Rebirth" "Access to Globalsec"]}
                :options {:start-as :runner}})
     (play-from-hand state :runner "Access to Globalsec")
     (is (= 2 (get-link state)) "2 link before rebirth")
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner chaos)
     (is (= 1 (get-link state)) "1 link after rebirth")))

  (deftest rebirth-gain-link
    ;; Rebirth - Gain link from ID
    (do-game
     (new-game {:runner {:id professor
                         :deck ["Rebirth" "Access to Globalsec"]}
                :options {:start-as :runner}})
     (play-from-hand state :runner "Access to Globalsec")
     (is (= 1 (get-link state)) "1 link before rebirth")
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner kate)
     (is (= 2 (get-link state)) "2 link after rebirth")))

  (deftest rebirth-implementation-notes
    ;; Rebirth - Implementation notes are kept, regression test for #3722
    (do-game
     (new-game {:runner {:id professor
                         :deck ["Rebirth"]}
                :options {:start-as :runner}})
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner chaos)
     (is (= :full (get-in (get-runner) [:identity :implementation])) "Implementation note kept as `:full`")))

  (deftest rebirth-reina-rezzing-ice-before-prevents-additional-cost
    ;; Rebirth - Rezzing Ice before prevents Reina's additional cost
    (do-game
     (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                :runner {:id whizzard
                         :deck ["Rebirth"]}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (play-from-hand state :corp "Ice Wall" "R&D")
     (take-credits state :corp)
     (is (changed? [(:credit (get-corp)) -1]
           (rez state :corp (get-ice state :hq 0)))
         "Only pay 1 to rez ice wall when against Whizzard")
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner reina)
     (is (= reina (get-in (get-runner) [:identity :title])) "Rebirthed into Reina")
     (is (changed? [(:credit (get-corp)) -1]
           (rez state :corp (get-ice state :rd 0)))
         "Additional cost from Reina not applied for 2nd ice rez")))

  (deftest rebirth-reina-rezzing-asset-before-get-additional-cost
    ;; Rebirth - Rezzing Asset before does not prevent Reina's additional cost
    (do-game
     (new-game {:corp {:deck ["Ice Wall" "Mark Yale"]}
                :runner {:id whizzard
                         :deck ["Rebirth"]}})
     (play-from-hand state :corp "Ice Wall" "HQ")
     (play-from-hand state :corp "Mark Yale" "New remote")
     (take-credits state :corp)
     (is (changed? [(:credit (get-corp)) -1]
           (rez state :corp (get-content state :remote1 0)))
         "Only pay 1 to rez Mark Yale")
     (play-from-hand state :runner "Rebirth")
     (click-prompt state :runner reina)
     (is (= reina (get-in (get-runner) [:identity :title])) "Rebirthed into Reina")
     (is (changed? [(:credit (get-corp)) -2]
           (rez state :corp (get-ice state :hq 0)))
         "Additional cost from Reina applied for 1st ice rez")))

  (deftest rebirth-only-legal-identities
    ;; Rebirth - Only shows legal IDs
    (do-game
     (new-game {:runner {:id reina
                         :hand ["Rebirth"]}
                :options {:format :standard}})
     (take-credits state :corp)
     (play-from-hand state :runner "Rebirth")
     (is (every? #(legal? :standard :legal %) (prompt-buttons :runner)) "Only legal IDs are available"))))

(deftest reboot-happy-path
    ;; Happy Path
    (do-game
    (new-game {:runner {:deck ["Reboot"]
                        :discard ["Sure Gamble" "Paperclip" "Clot"]}})
    (take-credits state :corp)
    (is (empty? (core/all-installed state :runner)) "Runner starts with no installed cards")
    (is (= 3 (count (:discard (get-runner)))) "Runner starts with 3 cards in trash")
    (is (empty? (:rfg (get-runner))) "Runner starts with no discarded cards")
    (play-run-event state "Reboot" :archives)
    (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
    (click-card state :runner (find-card "Paperclip" (:discard (get-runner))))
    (click-card state :runner (find-card "Clot" (:discard (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 3 (count (filter :facedown (core/all-installed state :runner)))) "Runner has 3 facedown cards")
    (is (= 3 (count (core/all-installed state :runner))) "Runner has no other cards installed")
    (is (empty? (:discard (get-runner))) "Runner has empty trash")
    (is (= 1 (count (:rfg (get-runner)))) "Runner has 1 card in RFG")))

(deftest reboot-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Blacklist"]
                          :discard "Blacklist"}
                 :runner {:deck ["Reboot"]
                          :discard ["Sure Gamble" "Paperclip" "Clot"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (is (empty? (core/all-installed state :runner)) "Runner starts with no installed cards")
      (is (= 3 (count (:discard (get-runner)))) "Runner starts with 3 cards in trash")
      (is (empty? (:rfg (get-runner))) "Runner starts with no discarded cards")
      (play-from-hand state :runner "Reboot")
      (run-continue state)
      (run-continue state)
      (is (no-prompt? state :runner) "Reboot prompt did not come up")))


(deftest recon-runner-jacks-out
    ;; Runner jacks out
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall"]}
                 :runner {:hand ["Recon"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Recon")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "Yes")
      (is (not (:run @state)) "Run has been ended")))

(deftest recon-runner-doesn-t-jack-out
    ;; Runner doesn't jack out
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Enigma" "Ice Wall"]}
                 :runner {:hand ["Recon"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Recon")
      (click-prompt state :runner "HQ")
      (run-continue state)
      (run-continue state)
      (click-prompt state :runner "No")
      (is (:run @state) "Run continues")
      (run-continue-until state :encounter-ice)
      (is (not= "Jack out?" (:msg (prompt-map :runner))) "No prompt to Jack out on second encounter")))

(deftest rejig
  ;; Rejig
  (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:id "Az McCaffrey: Mechanical Prodigy"
                          :hand ["Rejig" "Box-E" "Clone Chip"]}})
      (play-from-hand state :runner "Clone Chip")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Rejig")
      (is (= 1 (count (:hand (get-runner)))))
      (click-card state :runner "Clone Chip")
      (is (= 2 (count (:hand (get-runner)))))
      (is (= 8 (:credit (get-runner))))
      (click-card state :runner "Box-E")
      (is (= 1 (count (:hand (get-runner)))))
      (is (= 6 (:credit (get-runner)))) "Discounted install from Rejig + Az, Box-E only costs 2"))

(deftest rejig-with-kabonesa-wu-issue-4105
    ;; with Kabonesa Wu. Issue #4105
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:id "Kabonesa Wu: Netspace Thrillseeker"
                          :deck ["Gordian Blade"]
                          :hand ["Rejig"]}})
      (card-ability state :runner (:identity (get-runner)) 0)
      (click-prompt state :runner "Gordian Blade")
      (is (= 2 (:credit (get-runner))) "Runner only spends 3 for Gordian Blade")
      (play-from-hand state :runner "Rejig")
      (click-card state :runner "Gordian Blade")
      (is (find-card "Gordian Blade" (:hand (get-runner))) "Gordian Blade should have returned to the Grip")
      (click-card state :runner "Gordian Blade")
      (is (= 2 (:credit (get-runner))))
      (take-credits state :runner)
      (is (= "Gordian Blade" (:title (get-program state 0))) "Kabonesa Wu shouldn't rfg card bounced and reinstalled with Rejig")))

(deftest rejig-uninstalling-and-reinstalling-should-allow-once-per-turn-effects-again-issue-4217
    ;; Uninstalling and reinstalling should allow once per turn effects again. Issue #4217
    (do-game
      (new-game {:options {:start-as :runner}
                 :corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Rejig" "Stargate"]
                          :credits 100}})
      (play-from-hand state :runner "Stargate")
      (let [stargate (get-program state 0)]
        (card-ability state :runner stargate 0)
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "Hedge Fund")
        (play-from-hand state :runner "Rejig")
        (click-card state :runner "Stargate")
        (click-card state :runner "Stargate"))
      (let [stargate (get-program state 0)]
        (card-ability state :runner stargate 0)
        (run-continue state)
        (run-continue state)
        (click-prompt state :runner "Hedge Fund"))))

(deftest rejig-vs-aniccam
  ;; Rejig
  (do-game
    (new-game {:options {:start-as :runner}
               :runner {:hand ["Rejig" "Aniccam"]
                        :deck ["Ika"]}})
    (play-from-hand state :runner "Aniccam")
    (play-from-hand state :runner "Rejig")
    (click-card state :runner "Aniccam")
    (is-hand? state :runner ["Aniccam"])
    (click-card state :runner "Aniccam")
    (is-hand? state :runner ["Ika"])))

(deftest reprise
  ;; Reprise
  (do-game
    (new-game {:corp {:hand [(qty "Project Vitruvius" 2)]}
               :runner {:deck [(qty "Reprise" 2)]}})
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (take-credits state :corp)
    (is (changed? [(:click (get-runner)) 0]
          (play-from-hand state :runner "Reprise"))
        "Couldn't play Reprise without stealing an agenda this turn")
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Reprise")
    (click-card state :runner (get-content state :remote1 0))
    (is (= 1 (count (:hand (get-corp)))))
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-continue state)
    (click-prompt state :runner "Steal")
    (play-from-hand state :runner "Reprise")
    (click-prompt state :runner "Done")
    (click-prompt state :runner "No")
    (is (no-prompt? state :runner))))

(deftest reshape
  ;; Reshape - Swap 2 unrezzed pieces of ice
  (do-game
    (new-game {:corp {:deck [(qty "Vanilla" 2) "Paper Wall"]}
               :runner {:deck ["Reshape"]}})
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Vanilla" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Reshape")
    (click-card state :runner (get-ice state :rd 0))
    (click-card state :runner (get-ice state :hq 0))
    (is (not (no-prompt? state :runner)) "Can't target rezzed Vanilla, prompt still open")
    (click-card state :runner (get-ice state :hq 1))
    (is (no-prompt? state :runner))
    (is (= "Vanilla" (:title (get-ice state :rd 0))) "Vanilla swapped to R&D")
    (is (= "Paper Wall" (:title (get-ice state :hq 1))) "Paper Wall swapped to HQ outer position")))

(deftest retrieval-run-happy-path
    ;; Happy Path
    (do-game
      (new-game {:runner {:deck ["Retrieval Run" "Morning Star"]}})
      (take-credits state :corp)
      (trash-from-hand state :runner "Morning Star")
      (play-run-event state "Retrieval Run" :archives)
      (click-prompt state :runner "Retrieval Run")
      (let [ms (first (:discard (get-runner)))]
        (click-prompt state :runner ms)
        (is (= "Morning Star" (:title (first (get-program state))))
            "Morning Star installed")
        (is (= 2 (:credit (get-runner))) "Morning Star installed at no cost")
        (is (= 2 (core/available-mu state)) "Morning Star uses 2 memory"))))

(deftest retrieval-run-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp   {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Blacklist"]}
                 :runner {:deck ["Retrieval Run" "Morning Star"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (trash-from-hand state :runner "Morning Star")
      (play-run-event state "Retrieval Run" :archives)
      (click-prompt state :runner "Retrieval Run")
      (is (no-prompt? state :runner) "Retrieval run didn't attempt to install from heap.")
      (is (not (:run @state)) "Run is complete")))

(deftest rigged-results-corp-guesses-correctly
    ;; Corp guesses correctly
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck [(qty "Rigged Results" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Rigged Results")
      (click-prompt state :runner "0")
      (click-prompt state :corp "0")
      (is (no-prompt? state :runner) "Rigged Results failed for runner")
      (is (no-prompt? state :corp) "Rigged Results failed for runner")))

(deftest rigged-results-corp-guesses-incorrectly
    ;; Corp guesses incorrectly
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck [(qty "Rigged Results" 3)]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Rigged Results")
      (click-prompt state :runner "2")
      (click-prompt state :corp "1")
      (click-card state :runner (get-ice state :hq 0))
      (is (= [:hq] (:server (:run @state))) "Runner is running on HQ")
      (is (= 3 (:credit (get-runner))) "Rigged results spends credits")))

(deftest rigged-results-interaction-with-government-investigations
    ;; Interaction with Government Investigations
    (do-game
      (new-game {:runner {:hand ["Rigged Results" "Government Investigations"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Government Investigations")
      (play-from-hand state :runner "Rigged Results")
      (is (= ["0" "1"] (prompt-buttons :runner)) "Runner can't choose 2 because of Government Investigations")))

(deftest rigging-up
  ;; Rigging Up - Modded, but you may charge
  (do-game
    (new-game {:runner {:hand ["Rigging Up" "D4v1d"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rigging Up")
    (click-card state :runner "D4v1d")
    (click-prompt state :runner "Yes")
    (is (= 4 (get-counters (get-program state 0) :power)) "Rigging up gave +1 counter"))
  (do-game
    (new-game {:runner {:hand ["Rigging Up" "D4v1d"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Rigging Up")
    (click-card state :runner "D4v1d")
    (click-prompt state :runner "No")
    (is (= 3 (get-counters (get-program state 0) :power)) "Rigging up gave no extra counters"))
  (do-game
    (new-game {:runner {:hand ["Rigging Up" "Maw"]}})
    (take-credits state :corp)
    (is (changed? [(:credit (get-runner)) -3]
          (play-from-hand state :runner "Rigging Up")
          (click-card state :runner "Maw")
          (is (no-prompt? state :runner) "No prompt because maw cannot be charged"))
        "saved 3 credits with Rigging up")))

(deftest rip-deal
  ;; Rip Deal - replaces number of HQ accesses with heap retrieval
  (do-game
      (new-game {:corp   {:deck ["Vanilla"]}
                 :runner {:deck ["Rip Deal" "Easy Mark"]}})
      (trash-from-hand state :runner "Easy Mark")
      (take-credits state :corp)
      (play-run-event state "Rip Deal" :hq)
      (is (= "Add cards from heap to grip?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Choose 1 card to add from the heap to the grip" (:msg (prompt-map :runner))))
      (click-card state :runner "Easy Mark")
      (is (= 1 (-> (get-runner) :hand count)))
      (is (= "Easy Mark" (-> (get-runner) :hand first :title)))
      (is (no-prompt? state :corp) "Corp should have no more prompts")
      (is (no-prompt? state :runner) "Runner should have no more prompts")
      (is (nil? (get-run)) "Run is ended")))

(deftest rip-deal-with-gauntlet-2942
    ;; with Gauntlet #2942
    (do-game
      (new-game {:corp   {:deck    [(qty "Hedge Fund" 5)]
                          :hand    [(qty "Vanilla" 5)]
                          :credits 10}
                 :runner {:hand    ["The Gauntlet" "Corroder" "Rip Deal"]
                          :discard ["Easy Mark" "Sure Gamble"]
                          :credits 15}})
      (play-from-hand state :corp "Vanilla" "HQ")
      (play-from-hand state :corp "Vanilla" "HQ")
      (rez state :corp (get-ice state :hq 0))
      (rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "The Gauntlet")
      (play-from-hand state :runner "Corroder")
      (let [corroder (get-program state 0)
            iw0 (get-ice state :hq 0)
            iw1 (get-ice state :hq 1)]
        (play-from-hand state :runner "Rip Deal")
        (run-continue-until state :encounter-ice iw1)
        (card-ability state :runner corroder 0)
        (click-prompt state :runner "End the run")
        (run-continue-until state :encounter-ice iw0)
        (card-ability state :runner corroder 0)
        (click-prompt state :runner "End the run")
        (run-continue-until state :success)
        (click-prompt state :runner "Yes")
        (is (= "Choose 2 cards to add from the heap to the grip" (:msg (prompt-map :runner)))))
      (click-card state :runner "Easy Mark")
      (click-card state :runner "Sure Gamble")
      (is (= 2 (-> (get-runner) :hand count)))
      (is (= ["Easy Mark" "Sure Gamble"] (->> (get-runner) :hand (map :title) (into []))))
      (is (no-prompt? state :corp) "Corp should have no more prompts")
      (is (no-prompt? state :runner) "Runner should have no more prompts")
      (is (nil? (get-run)) "Run is ended")))

(deftest rip-deal-can-still-access-upgrades
    ;; Can still access upgrades
    (do-game
      (new-game {:corp   {:deck ["Vanilla"]
                          :hand ["Hedge Fund" "Anoetic Void"]}
                 :runner {:deck ["Rip Deal" "Easy Mark"]}})
      (trash-from-hand state :runner "Easy Mark")
      (play-from-hand state :corp "Anoetic Void" "HQ")
      (take-credits state :corp)
      (play-run-event state "Rip Deal" :hq)
      (is (= "Add cards from heap to grip?" (:msg (prompt-map :runner))))
      (click-prompt state :runner "Yes")
      (is (= "Choose 1 card to add from the heap to the grip" (:msg (prompt-map :runner))))
      (click-card state :runner "Easy Mark")
      (is (= 1 (-> (get-runner) :hand count)))
      (is (= "Easy Mark" (-> (get-runner) :hand first :title)))
      (is (accessing state "Anoetic Void"))
      (click-prompt state :runner "No action")
      (is (nil? (get-run)) "Run is ended")))

(deftest rip-deal-heap-locked
    ;; Heap Locked
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                          :hand ["Blacklist"]}
                 :runner {:deck ["Rip Deal" "Easy Mark"]}})
      (trash-from-hand state :runner "Easy Mark")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "Rip Deal")
      (run-continue-until state :success)
      (is (no-prompt? state :runner) "Rip Deal prompt did not come up")))

(deftest ritual-test
  (do-game
    (new-game {:runner {:hand [(qty "Ritual" 4)]
                        :deck [(qty "Sure Gamble" 15)]}})
    (take-credits state :corp)
    (doseq [rc [3 2 1 0]]
      (is (changed? [(count (:hand (get-runner))) (- rc 1)]
                    (play-from-hand state :runner "Ritual"))))))

(deftest rumor-mill-full-test
    ;; Full test
    (do-game
      (new-game {:corp {:deck [(qty "Project Atlas" 2)
                               "Caprice Nisei" "Chairman Hiro" "Cybernetics Court"
                               "Elizabeth Mills" "Ibrahim Salem"
                               "Housekeeping" "Director Haas" "Oberth Protocol"]}
                 :runner {:deck ["Rumor Mill"]}})
      (core/gain state :corp :credit 100 :click 100 :bad-publicity 1)
      (draw state :corp 100)
      (play-from-hand state :corp "Caprice Nisei" "New remote")
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (play-from-hand state :corp "Cybernetics Court" "New remote")
      (play-from-hand state :corp "Elizabeth Mills" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Ibrahim Salem" "New remote")
      (play-from-hand state :corp "Oberth Protocol" "New remote")
      (core/move state :corp (find-card "Director Haas" (:hand (get-corp))) :deck)
      (rez state :corp (get-content state :remote2 0))
      (rez state :corp (get-content state :remote3 0))
      (score-agenda state :corp (get-content state :remote5 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 100 :click 100)
      (is (= 9 (hand-size :corp)) "Corp has +4 hand size")
      (is (= 3 (hand-size :runner)) "Runner has -2 hand size")
      (play-from-hand state :runner "Rumor Mill")
      ;; Additional costs to rez should NOT be applied
      (rez state :corp (get-content state :remote6 0))
      (is (= 1 (count (:scored (get-corp)))) "No agenda was auto-forfeit to rez Ibrahim Salem")
      ;; In-play effects
      (is (= 5 (hand-size :corp)) "Corp has original hand size")
      (is (= 5 (hand-size :runner)) "Runner has original hand size")
      ;; "When you rez" effects should not apply
      (rez state :corp (get-content state :remote4 0))
      (is (= 1 (count-bad-pub state)) "Corp still has 1 bad publicity")
      ;; Run events (Caprice)
      ;; Make sure Rumor Mill applies even if card is rezzed after RM is put in play.
      (rez state :corp (get-content state :remote1 0))
      (run-on state :remote1)
      (run-continue state)
      (is (no-prompt? state :corp) "Caprice prompt is not showing")
      (click-prompt state :runner "No action")
      ;; Trashable execs
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (empty? (:scored (get-runner))) "Chairman Hiro not added to runner's score area")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (empty? (:scored (get-runner))) "Director Haas not added to runner's score area")
      (take-credits state :runner)
      ;; Trash RM, make sure everything works again
      (play-from-hand state :corp "Housekeeping")
      (is (= 9 (hand-size :corp)) "Corp has +4 hand size")
      (is (= 5 (hand-size :runner)) "Runner has +0 hand size")
      ;; Additional costs to rez should now be applied again
      (rez state :corp (get-content state :remote7 0) {:expect-rez false})
      (click-card state :corp (get-in (get-corp) [:scored 0]))
      (is (zero? (count (:scored (get-corp)))) "Agenda was auto-forfeit to rez Oberth")
      (derez state :corp (get-content state :remote4 0))
      (rez state :corp (get-content state :remote4 0))
      (is (zero? (count-bad-pub state)) "Corp has 0 bad publicity")
      (card-ability state :corp (get-content state :remote4 0) 0) ; Elizabeth Mills, should show a prompt
      (is (= 1 (count-bad-pub state)) "Elizabeth Mills ability allowed")))

(deftest rumor-mill-make-sure-rumor-mill-is-not-active-when-hosted-on-peddler
    ;; Make sure Rumor Mill is not active when hosted on Peddler
    (do-game
      (new-game {:corp {:deck ["Jeeves Model Bioroids"]}
                 :runner {:deck ["Street Peddler" (qty "Rumor Mill" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (take-credits state :runner)
      (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
      (let [jeeves (get-content state :remote1 0)]
        (rez state :corp jeeves)
        (card-ability state :corp jeeves 0)
        (is (= 3 (:click (get-corp))) "Corp has 3 clicks - Jeeves working ok"))))

(deftest run-amok
  ;; Run Amok
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["Run Amok"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Enigma" "New remote")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Run Amok")
    (click-prompt state :runner "Server 1")
    (run-continue state)
    (rez state :corp (get-ice state :remote1 0))
    (run-continue state)
    (run-continue state)
    (run-continue state)
    (click-card state :runner "Ice Wall")
    (is (prompt-map :runner) "Prompt should still be active as Ice Wall wasn't rezzed this run")
    (click-card state :runner "Enigma")
    (is (no-prompt? state :runner) "Enigma should be trashed")
    (is (= "Enigma" (-> (get-corp) :discard first :title)) "Enigma should be trashed")))

(deftest running-hot
  ;; Amped Up - Gain 3 clicks and take 1 unpreventable brain damage
  (do-game
    (new-game {:runner {:deck ["Running Hot"
                               "Feedback Filter"
                               (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Running Hot")
    (is (no-prompt? state :runner)
        "Feedback Filter core damage prevention opportunity not given")
    (is (= 5 (:click (get-runner))) "Runner gained 2 clicks from Running Hot")
    (is (= 2 (count (:discard (get-runner)))) "Runner discarded 1 card from damage")
    (is (= 4 (hand-size :runner)) "Runner handsize decreased by 1")
    (is (= 1 (:brain-damage (get-runner))) "Took 1 core damage")))

(deftest running-interference
  ;; Running Interference
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Archer" "Hostile Takeover"]
                      :credits 100}
               :runner {:hand ["Running Interference"]}})
    (play-and-score state "Hostile Takeover")
    (play-from-hand state :corp "Archer" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Running Interference")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (let [archer (get-ice state :hq 0)
          credits (:credit (get-corp))]
      (rez state :corp archer {:expect-rez false})
      (click-card state :corp (get-scored state :corp 0))
      (is (no-prompt? state :corp) "Only 1 agenda required to rez")
      (is (= (- credits (* 2 (:cost archer))) (:credit (get-corp))) "Rezzing Archer costs double")
      (is (rezzed? (refresh archer)) "Archer is rezzed"))
    (run-continue-until state :movement)
    (run-jack-out state)
    (let [iw (get-ice state :hq 1)
          credits (:credit (get-corp))]
      (run-on state "HQ")
      (rez state :corp iw)
      (is (no-prompt? state :corp))
      (is (= (- credits (:cost iw)) (:credit (get-corp))) "Rezzing Ice Wall costs normal"))))

(deftest s-dobrado-no-threat
  (do-game
    (new-game {:corp {:hand ["Vanilla" "Ice Wall"]}
               :runner {:hand ["S-Dobrado"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "S-Dobrado")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (rez state :corp (get-ice state :hq 1))
    (rez state :corp (get-ice state :hq 0))
    (is (:run @state) "A run has been initiated")
    (run-continue state)
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")
    (run-continue state)
    (run-continue state)
    (is (= :encounter-ice (:phase (get-run))))))

(deftest s-dobrado-threat-active
  (do-game
    (new-game {:corp {:hand ["Vanilla" "Ice Wall" "Government Takeover"]}
               :runner {:hand ["S-Dobrado"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-and-score state "Government Takeover")
    (take-credits state :corp)
    (play-from-hand state :runner "S-Dobrado")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (rez state :corp (get-ice state :hq 1))
    (rez state :corp (get-ice state :hq 0))
    (is (:run @state) "A run has been initiated")
    (run-continue state)
    (is (= :movement (:phase (get-run))) "Run has bypassed Ice Wall")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "Yes")
    (is (= :movement (:phase (get-run))))))

(deftest satellite-uplink-when-exposing-2-cards
    ;; when exposing 2 cards
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Hostile Takeover"]}
                 :runner {:hand ["Satellite Uplink"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "Server 1")
      (take-credits state :corp)
      (play-from-hand state :runner "Satellite Uplink")
      (click-card state :runner "Ice Wall")
      (click-card state :runner "Hostile Takeover")))

(deftest satellite-uplink-when-exposing-1-card
    ;; when exposing 2 cards
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Ice Wall" "Hostile Takeover"]}
                 :runner {:hand ["Satellite Uplink"]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Hostile Takeover" "Server 1")
      (take-credits state :corp)
      (play-from-hand state :runner "Satellite Uplink")
      (click-card state :runner "Ice Wall")
      (click-prompt state :runner "Done")))

(deftest scavenge-happy-path
    ;; Happy Path
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Scavenge" "Corroder"]
                          :discard ["Mass-Driver"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Scavenge")
      (let [credits (:credit (get-runner))]
        (click-card state :runner "Corroder")
        (click-card state :runner "Mass-Driver")
        (is (= "Mass-Driver" (:title (get-program state 0))) "Mass-Driver is now installed")
        (is (= (+ credits 2 -8) (:credit (get-runner))) "Scavenge should give discount"))))

(deftest scavenge-blacklist-rezzed
    ;; Blacklist Rezzed
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 3) "Blacklist"]}
                 :runner {:hand ["Scavenge" "Corroder" "Engolo"]
                          :discard ["Mass-Driver"]
                          :credits 10}})
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote2 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (play-from-hand state :runner "Scavenge")
      (let [credits (:credit (get-runner))]
        (click-card state :runner "Corroder")
        (click-card state :runner "Mass-Driver")
        (is (= "Choose a program to install" (:msg (prompt-map :runner))) "Grip is only option")
        (click-card state :runner "Engolo")
        (is (= "Engolo" (:title (get-program state 0))) "Engolo is now installed")
        (is (= (+ credits 2 -5) (:credit (get-runner))) "Scavenge should give discount"))))

(deftest scrounge
  (do-game
    (new-game {:runner {:hand ["Scrounge"] :deck ["Rezeki"] :discard ["Ika" "Mayfly"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Scrounge")
    (click-card state :runner "Ika")
    (click-card state :runner "Mayfly")
    (is-deck? state :runner ["Rezeki" "Mayfly"])))

(deftest scrubbed
  ;; First piece of ice encountered each turn has -2 Strength for remainder of the run
  (do-game
    (new-game {:corp {:deck ["Turing"]}
               :runner {:hand ["Street Peddler" "Scrubbed"]}})
    (play-from-hand state :corp "Turing" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (let [turing (get-ice state :hq 0)]
      (rez state :corp turing)
      (is (= 2 (get-strength (refresh turing))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (get-strength (refresh turing))) "Scrubbed not active when on Peddler")
      (run-continue state :movement)
      (run-jack-out state)
      (play-from-hand state :runner "Scrubbed")
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (get-strength (refresh turing))) "Scrubbed reduces strength by 2")
      (run-continue state :movement)
      (run-jack-out state))))

(deftest security-leak
  (do-game
    (new-game {:corp {:hand ["Hostile Takeover"]}
               :runner {:hand ["Security Leak"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Security Leak")
    (take-credits state :runner)
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (dotimes [_ 2]
      (is (changed? [(:credit (get-corp)) -2]
            (click-advance state :corp (get-content state :remote1 0)))
          "Spent 2c to advance hostile takeover"))
    (score state :corp (get-content state :remote1 0))
    (is (is-discard? state :runner ["Security Leak"]) "Security leak works like other currents")))

(deftest showing-off
  ;; Showing Off
  (do-game
    (new-game {:corp {:hand [(qty "Hedge Fund" 5)]
                      :deck ["Accelerated Beta Test" "Brainstorm" "Chiyashi"
                             "DNA Tracker" "Excalibur" "Fire Wall"]}
               :runner {:hand ["Showing Off"]}})
    (stack-deck state :corp ["Accelerated Beta Test" "Brainstorm" "Chiyashi"
                             "DNA Tracker" "Excalibur" "Fire Wall"])
    (take-credits state :corp)
    (play-run-event state "Showing Off" :rd)
    (is (accessing state "Fire Wall") "The accessed card is on the bottom of the deck")
    (is (= "Accelerated Beta Test" (-> (get-corp) :deck first :title)) "The top of the deck is an entirely different card")
    (click-prompt state :runner "No action")))

(deftest showing-off-vs-medium
  (let [deck-order ["Hedge Fund" "IPO" "Restructure" "Ice Wall" "Orion" "Extract" "Scarcity of Resources"]]
    (do-game
      (new-game {:corp {:hand []
                        :deck deck-order}
                 :runner {:hand ["Showing Off" "Medium"]}})
      (stack-deck state :corp deck-order)
      (take-credits state :corp)
      (play-from-hand state :runner "Medium")
      (core/add-counter state :runner (core/make-eid state) (get-program state 0) :virus 3)
      (play-from-hand state :runner "Showing Off")
      (run-continue-until state :success)
      (click-prompt state :runner "3")
      (dotimes [n 4]
        (let [target-card (nth (reverse deck-order) n)]
          (is (= (:msg (prompt-map :runner)) (str "You accessed " target-card "."))
              (str "Accessing the card at index " n " of the bottom of the deck (" target-card ")")))
        (click-prompt state :runner "No action"))
      (is (no-prompt? state :runner) "No longer accessing cards"))))

(deftest shred-test
  (doseq [opt [(str "Trash 3 cards randomly from your hand") "The run does not end"]]
    (do-game
      (new-game {:runner {:hand ["Shred"]}
                 :corp {:hand ["Prisec" "Ice Wall" "Ganked!" "PAD Campaign" "Hedge Fund" "IPO"
                               "Restructure" "Rashida Jaheem"]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Prisec" "New remote")
      (play-from-hand state :corp "Ganked!" "Server 1")
      (play-from-hand state :corp "PAD Campaign" "Server 1")
      (play-from-hand state :corp "Ice Wall" "Server 1")
      (rez state :corp (get-ice state :remote1 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Shred")
      (click-prompt state :runner "Server 1")
      (run-continue state)
      (run-continue state :encounter-ice)
      (card-subroutine state :corp (get-ice state :remote1 0) 0)
      (click-prompt state :corp opt)
      (if (= opt "The run does not end")
        (do (is (no-prompt? state :runner) "no prompt")
            (is (no-prompt? state :corp) "no prompt")
            (is (:run @state) "still run"))
        (is (not (:run @state)) "not run")))))

(deftest singularity
  ;; Singularity - Run a remote; if successful, trash all contents at no cost
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei"
                             "Breaker Bay Grid"
                             "Eve Campaign"]}
               :runner {:deck ["Singularity"]}})
    (play-from-hand state :corp "Breaker Bay Grid" "New remote")
    (play-from-hand state :corp "Caprice Nisei" "Server 1")
    (play-from-hand state :corp "Eve Campaign" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Singularity")
    (click-prompt state :runner "Server 1")
    (run-continue state)
    (is (= 2 (:click (get-runner))) "Runner spends 2 clicks on double event")
    (is (= 1 (:credit (get-runner))) "Runner pays 4 credits for Singularity")
    (run-continue state)
    (is (= 3 (count (:discard (get-corp)))) "All 3 cards trashed from Server 1")
    (is (= 1 (:credit (get-runner))) "No credits paid for trashing")
    (is (nil? (get-in @state [:corp :servers :remote1 :content])) "Server 1 no longer exists")))

(deftest social-engineering
  ;; Social Engineering
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Fire Wall" "Ice Wall"]
                      :credits 10}
               :runner {:hand ["Social Engineering"]}})
    (play-from-hand state :corp "Fire Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Social Engineering")
    (click-card state :runner "Fire Wall")
    (let [credits (:credit (get-runner))]
      (rez state :corp (get-ice state :rd 0))
      (is (= credits (:credit (get-runner))) "Shouldn't gain credits from different ice rez")
      (rez state :corp (get-ice state :hq 0))
      (is (= (+ credits 5) (:credit (get-runner))) "Should gain credits from correct ice rez"))))

(deftest spark-of-inspiration-basic
  (do-game
    (new-game {:runner {:hand ["Spark of Inspiration"]
                        :deck ["Torch"]}})
    (take-credits state :corp)
    (is (changed? [(:credit (get-runner)) -3]
          (play-from-hand state :runner "Spark of Inspiration")
          (click-prompt state :runner "Yes")
          (is (= "Torch" (:title (get-program state 0))) "installed torch"))
        "Spent 3 on spark, 0 on install")))

(deftest spark-of-inspiration-decline-install
  (do-game
    (new-game {:runner {:hand ["Spark of Inspiration"]
                        :deck ["Torch"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Spark of Inspiration")
    (click-prompt state :runner "No")
    (is (not (get-program state 0))) "Torch was not installed"))

(deftest spark-no-targets
  (do-game
    (new-game {:runner {:hand ["Spark of Inspiration"]
                        :deck [(qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (changed? [(:credit (get-runner)) -3]
          (play-from-hand state :runner "Spark of Inspiration"))
        "Spent 3 on spark, 0 on install")
    (is (no-prompt? state :runner))
    (is (zero? (count (:set-aside (get-runner)))) "cards returned to stack")))

(deftest spear-phishing
  ;; Spear Phishing
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall"]}
               :runner {:hand ["Spear Phishing"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Spear Phishing")
    (click-prompt state :runner "HQ")
    (is (:run @state) "A run has been initiated")))

(deftest spec-work
  ;; Spec Work
  (do-game
    (new-game {:runner {:hand ["Spec Work" "Cache"]
                        :deck [(qty "Cache" 2)]}
               :options {:start-as :runner}})
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Spec Work")
    (is (= 3 (:credit (get-runner))) "Paid credit cost for Spec Work")
    (is (= 0 (count (:hand (get-runner)))))
    (click-card state :runner (get-program state 0))
    (is (= 7 (:credit (get-runner))) "+4 credits after paying for Spec Work")
    (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")))

(deftest special-order
  ;; Special Order
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck ["Corroder"]
                        :hand ["Special Order"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Special Order")
    (click-prompt state :runner "Corroder")
    (is (= ["Corroder"] (->> (get-runner) :hand (map :title)))
        "Corroder should be in the hand now")))

(deftest spooned
  ;; Spooned
  (do-game
    (new-game {:corp {:deck ["Enigma"]}
               :runner {:deck ["Spooned" "Gordian Blade"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Gordian Blade")
    (play-from-hand state :runner "Spooned")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (rez state :corp (get-ice state :hq 0))
    (run-continue state)
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "Force the Runner to lose [Click]")
    (click-prompt state :runner "End the run")
    (run-continue state)
    (run-continue state)
    (is (= 1 (count (:discard (get-corp)))) "Enigma is trashed")))

(deftest spooned-works-after-not-breaking-the-first-ice
  ;; Spooned
  (do-game
    (new-game {:corp {:deck ["Vanilla" "Ice Wall"]}
               :runner {:deck ["Spooned" "Engolo"]
                        :id "Rielle \"Kit\" Peddler: Transhuman"
                        :credits 10}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (rez state :corp (get-ice state :hq 0))
    (rez state :corp (get-ice state :hq 1))
    (take-credits state :corp)
    (play-from-hand state :runner "Engolo")
    (play-from-hand state :runner "Spooned")
    (click-prompt state :runner "HQ")
    (run-continue-until state :encounter-ice)
    (click-prompt state :runner "No")
    (run-continue-until state :encounter-ice)
    (click-prompt state :runner "Yes")
    (card-ability state :runner (get-program state 0) 0)
    (click-prompt state :runner "End the run")
    (is (= '("Vanilla") (map :title (:discard (get-corp)))) "Vanilla trashed")))

(deftest spot-the-prey
  ;; Spot the Prey
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Hostile Takeover"]}
               :runner {:hand ["Spot the Prey"]}})
    (play-from-hand state :corp "Hostile Takeover" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Spot the Prey")
    (click-card state :runner "Hostile Takeover")
    (is (last-log-contains? state "Runner uses Spot the Prey to expose Hostile Takeover"))
    (click-prompt state :runner "HQ")
    (is (:run @state) "Run should be initiated")))

(deftest spree
  (do-game
    (new-game {:corp {:hand [(qty "Ice Wall" 3)]}
               :runner {:hand ["Botulus" "Tranquilizer" "Spree"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Botulus")
    (click-card state :runner (get-ice state :hq 0))
    (play-from-hand state :runner "Tranquilizer")
    (click-card state :runner (get-ice state :rd 0))
    (play-from-hand state :runner "Spree")
    (click-prompt state :runner "Archives")
    (is (:run @state) "Run should be initiated")
    (run-continue state)
    (let [spree (-> (get-runner) :play-area first)
          hq (get-ice state :hq 0)
          ar (get-ice state :archives 0)
          rd (get-ice state :rd 0)]
      (is (= 3 (get-counters (refresh spree) :power)) "Spree has 3 power counters")
      (is (changed? [(get-counters (refresh spree) :power) -1
                     (-> (refresh hq) :hosted count) -1
                     (-> (refresh ar) :hosted count) 1]
                    (card-ability state :runner (refresh spree) 0)
                    (click-card state :runner (-> (refresh hq) :hosted first))
                    (click-card state :runner (refresh ar)))
          "Spend a counter to move a trojan to ice in attacked server")
      (is (changed? [(-> (refresh rd) :hosted count) 0
                     (-> (refresh hq) :hosted count) 0
                     (-> (refresh ar) :hosted count) 0]
                    (card-ability state :runner (refresh spree) 0)
                    (click-card state :runner (-> (refresh rd) :hosted first))
                    (click-card state :runner (refresh hq)))
          "Cannot move a trojan to ice in a server that is not under attack"))))

(deftest steelskin-scarring
  (do-game
    (new-game {:runner {:hand ["Steelskin Scarring"] :deck [(qty "Sure Gamble" 45)]}})
    (damage state :corp :net 1)
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards from steelskin"))
  (do-game
    (new-game {:runner {:hand ["Steelskin Scarring"] :deck [(qty "Sure Gamble" 45)]}
               :corp {:hand ["Kala Ghoda Real TV"]}})
    (core/move state :runner (find-card "Steelskin Scarring" (:hand (get-runner))) :deck {:front true})
    (core/fake-checkpoint state)
    (is (= "Steelskin Scarring" (:title (first (:deck (get-runner))))) "Steelskin is ontop of the stack")
    (play-from-hand state :corp "Kala Ghoda Real TV" "New remote")
    (rez state :corp (get-content state :remote1 0))
    (card-ability state :corp (get-content state :remote1 0) 1)
    (click-prompt state :runner "Yes")
    (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards when steelskin gets trashed from stack"))
  (do-game
    (new-game {:runner {:hand ["Steelskin Scarring"] :deck [(qty "Sure Gamble" 45)]}})
    (take-credits state :corp)
    (is (changed? [(count (:hand (get-runner))) 2]
          (play-from-hand state :runner "Steelskin Scarring"))
        "Drew 3 (+2 net cards) with steelskin")))

(deftest steelskin-scarring-waiting-prompts
  (do-game
    (new-game {:runner {:deck [(qty "Steelskin Scarring" 44)]
                        :hand [(qty "Steelskin Scarring" 2)]}})
    (damage state :corp :net 1)
    (is (waiting? state :corp) "Corp is waiting for the runner")
    (is (changed? [(count (:hand (get-runner))) 2]
          (click-prompt state :runner "Yes"))
        "Draw 2")
    (damage state :corp :net 1)
    (is (waiting? state :corp) "Corp is waiting for the runner")
    (is (changed? [(count (:hand (get-runner))) 0]
          (click-prompt state :runner "No"))
        "Draw none (decline)")
    (is (no-prompt? state :corp) "Corp is not waiting anymore")
    (is (= 2 (count (:hand (get-runner)))))
    (damage state :corp :meat 2)
    (is (waiting? state :corp) "Corp is waiting for the runner to pick a steelskin")))


(deftest stimhack
  ;; Stimhack - Gain 9 temporary credits and take 1 brain damage after the run
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"]}
               :runner {:deck ["Stimhack" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Stimhack")
    (click-prompt state :runner "HQ")
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-continue state)
    (run-continue state)
    (is (= 14 (:credit (get-runner))))
    (is (= 9 (:run-credit (get-runner))) "Gained 9 credits for use during the run")
    (click-prompt state :runner "Pay 5 [Credits] to trash") ; choose to trash Eve
    (is (and (zero? (count (:hand (get-corp))))
             (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Eve in Archives")
    (is (= 5 (:credit (get-runner))))
    (is (zero? (count (:hand (get-runner)))) "Lost card from Grip to core damage")
    (is (= 4 (hand-size :runner)))
    (is (= 1 (:brain-damage (get-runner))))))

(deftest strike-fund
  (do-game
    (new-game {:runner {:hand ["Strike Fund" "Strike Fund"]}})
    (is (changed? [(:credit (get-runner)) 2]
          (damage state :runner :meat 1)
          (is (waiting? state :corp) "Corp is waiting for the runner")
          (click-prompt state :runner "Yes"))
        "Gained 2 credits from Strike Fund")
    (take-credits state :corp)
    (is (changed? [(:credit (get-runner)) 3]
          (play-from-hand state :runner "Strike Fund"))
        "Gained 3 credits from playing Strike Fund")))

(deftest sure-gamble
  ;; Sure Gamble
  (do-game
    (new-game {:runner {:deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Sure Gamble")
    (is (= 9 (:credit (get-runner))))))

(deftest surge-valid-target
    ;; Valid target
    (do-game
      (new-game {:runner {:deck ["Imp" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
        (play-from-hand state :runner "Surge")
        (click-card state :runner imp)
        (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))

(deftest surge-don-t-fire-surge-if-target-is-not-a-virus
    ;; Don't fire surge if target is not a virus
    (do-game
      (new-game {:runner {:deck ["Security Testing" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (play-from-hand state :runner "Surge")
      (is (no-prompt? state :runner) "Surge does not fire on Security Testing")))

(deftest surge-don-t-fire-surge-if-target-does-not-have-virus-counter-flag-set
    ;; Don't fire surge if target does not have virus counter flag set
    (do-game
      (new-game {:runner {:deck ["Imp" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
        (take-credits state :runner)
        (take-credits state :corp)
        (play-from-hand state :runner "Surge")
        (is (no-prompt? state :runner) "Surge does not fire on Imp turn after install"))))

(deftest surge-don-t-allow-surging-gorman-drip-since-it-happens-on-the-corp-turn
    ;; Don't allow surging Gorman Drip, since it happens on the corp turn
    (do-game
      (new-game {:runner {:deck ["Gorman Drip v1" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gorman Drip v1")
      (let [gd (get-program state 0)]
        (is (zero? (get-counters gd :virus)) "Gorman Drip starts without counters")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh gd) :virus))
            "Gorman Drip gains 3 counters after Corp clicks 3 times for credits")
        (play-from-hand state :runner "Surge")
        (is (no-prompt? state :runner) "Surge does not trigger on Gorman Drip"))))

(deftest syn-attack-and-corp-chooses-to-draw
    ;; and corp chooses to draw
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["SYN Attack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "SYN Attack")
      (let [hand (count (:hand (get-corp)))]
        (click-prompt state :corp "Draw 4 cards")
        (is (= (+ hand 4) (count (:hand (get-corp)))) "Corp should draw 4 cards"))))

(deftest syn-attack-and-corp-chooses-to-discard
    ;; and corp chooses to discard
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["SYN Attack"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "SYN Attack")
      (let [hand (count (:hand (get-corp)))]
        (click-prompt state :corp "Discard 2 cards from HQ")
        (click-card state :corp (first (:hand (get-corp))))
        (click-card state :corp (second (:hand (get-corp))))
        (is (= (+ hand -2) (count (:hand (get-corp)))) "Corp should discard 2 cards"))))

(deftest system-outage
  ;; When Corp draws 1+ cards, it loses 1 if it is not the first time he or she has drawn cards this turn
  (do-game
    (new-game {:corp {:deck [(qty "Turing" 10)]}
               :runner {:deck ["Street Peddler"
                               (qty "System Outage" 3)]}})
    (starting-hand state :corp [])
    (starting-hand state :runner ["Street Peddler" "System Outage"])
    (take-credits state :corp) ; corp at 8cr
    (play-from-hand state :runner "Street Peddler")
    (take-credits state :runner)
    (click-draw state :corp)
    (is (= 8 (:credit (get-corp))) "1st card drawn for free - System Outage on Peddler")
    (click-draw state :corp)
    (is (= 8 (:credit (get-corp))) "2nd card drawn for free - System Outage on Peddler")
    (take-credits state :corp) ; corp at 9cr
    (is (= 9 (:credit (get-corp))) "Corp at 9")
    (play-from-hand state :runner "System Outage")
    (take-credits state :runner)
    (click-draw state :corp)
    (is (= 8 (:credit (get-corp))) "1st card drawn cost 1cr - System Outage active")
    (click-draw state :corp)
    (is (= 7 (:credit (get-corp))) "2nd card drawn cost 1cr - System Outage active")))

(deftest system-seizure
  ;; System Seizure - First icebreaker boosted keeps strength for remainder of that run.
  (do-game
    (new-game {:corp {:deck ["Wraparound"]}
               :runner {:deck [(qty "Corroder" 2) "System Seizure"]
                        :credits 20}})
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "System Seizure")
    (let [c1 (get-program state 0)
          c2 (get-program state 1)]
      (run-empty-server state "R&D") ;; Check that System Seizure triggers even if another run has been made
      (run-on state "HQ") ;; Check that System Seizure only keeps strength on one of the breakers
      (rez state :corp (get-ice state :hq 0))
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c1 1)
      (card-ability state :runner c2 1)
      (is (= 3 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 3 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 3 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      ;; Check that System Seizure does not keep strength on 2nd run
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c1 1)
      (card-ability state :runner c2 1)
      (is (= 3 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 3 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 3 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      (take-credits state :runner)
      (take-credits state :corp)
      ;; System Seizure resets which card it tracks each turn
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c2 1)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 3 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 3 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (refresh c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (refresh c2))) "Corroder 2 has 2 strength"))))

(deftest test-run-programs-hosted-after-install-get-returned-to-stack-issue-1081
    ;; Programs hosted after install get returned to Stack. Issue #1081
    (do-game
      (new-game {:corp {:deck ["Wraparound"]}
                 :runner {:deck [(qty "Test Run" 2) "Morning Star"
                                 "Knight" "Leprechaun"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (rez state :corp wrap))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (trash-from-hand state :runner "Morning Star")
      (trash-from-hand state :runner "Knight")
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (play-from-hand state :runner "Leprechaun")
        (play-from-hand state :runner "Test Run")
        (click-prompt state :runner "Heap")
        (click-prompt state :runner ms)
        (click-prompt state :runner "Leprechaun"))
      (let [lep (get-program state 0)]
        (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep"))
      (take-credits state :runner)
      (is (= "Morning Star" (:title (first (:deck (get-runner))))) "Morning Star returned to Stack from host")
      (take-credits state :corp)
      (let [kn (find-card "Knight" (:discard (get-runner)))]
        (play-from-hand state :runner "Test Run")
        (click-prompt state :runner "Heap")
        (click-prompt state :runner kn)
        (click-prompt state :runner "The Rig"))
      (let [wrap (get-ice state :hq 0)
            kn (get-program state 1)]
        (card-ability state :runner kn 0)
        (click-card state :runner wrap)
        (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
        (take-credits state :runner)
        (is (= "Knight" (:title (first (:deck (get-runner))))) "Knight returned to Stack from host ice"))))

(deftest test-run-make-sure-program-remains-installed-if-scavenged
    ;; Make sure program remains installed if Scavenged
    (do-game
      (new-game {:runner {:hand ["Test Run" "Scavenge" "Inti"]
                          :discard ["Morning Star"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Test Run")
      (click-prompt state :runner "Heap")
      (click-prompt state :runner "Morning Star")
      (is (= 2 (:credit (get-runner))) "Program installed for free")
      (play-from-hand state :runner "Scavenge")
      (click-card state :runner "Morning Star")
      (click-card state :runner "Morning Star")
      (take-credits state :runner)
      (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
      (is (= "Morning Star" (:title (get-program state 0))) "Morning Star still installed")))

(deftest test-run-heap-locked-test
    ;; Heap Locked Test
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5) "Blacklist"]}
                 :runner {:hand [(qty "Test Run" 3) "Scavenge" "Inti"]
                          :deck ["Pelangi"]
                          :discard ["Morning Star"]}})
      (play-from-hand state :corp "Blacklist" "New remote")
      (rez state :corp (refresh (get-content state :remote1 0)))
      (take-credits state :corp)
      (play-from-hand state :runner "Test Run")
      (is (= "Install a program from the stack?" (:msg (prompt-map :runner))) "Stack is only option")
      (is (= 1 (-> (prompt-map :runner) :choices count)) "Runner has 1 choice")
      (is (= ["Stack"] (prompt-buttons :runner)) "Runner's only choice is Stack")))


(deftest the-maker-s-eye
  (do-game
     (new-game {:corp {:deck [(qty "Quandary" 5)]
                       :hand [(qty "Quandary" 5)]}
                :runner {:deck ["The Maker's Eye"]}})
     (take-credits state :corp)
     (play-run-event state "The Maker's Eye" :rd)
     (is (accessing state "Quandary"))
     (click-prompt state :runner "No action")
     (is (accessing state "Quandary"))
     (click-prompt state :runner "No action")
     (is (accessing state "Quandary"))
     (click-prompt state :runner "No action")
     (is (not (:run @state)))))

(deftest makers-eye-can-access-upgrades-installed-during-run
  (do-game
    (new-game {:corp {:hand ["Drafter" "Crisium Grid"]
                      :deck [(qty "Hedge Fund" 3)]}
               :runner {:hand ["The Maker's Eye"]}})
    (play-from-hand state :corp "Drafter" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "The Maker's Eye")
    (run-continue state)
    (let [dra (get-ice state :rd 0)]
      (rez state :corp dra)
      (run-continue state :encounter-ice)
      (card-subroutine state :corp (refresh dra) 1)
      (click-card state :corp "Crisium Grid")
      (click-prompt state :corp "R&D")
      (run-continue-until state :success)
      (click-prompt state :runner "Unrezzed upgrade")
      (click-prompt state :runner "No action"))))

(deftest the-maker-s-eye-doppelganger-interaction
    ;; Doppelgänger interaction
    (do-game
     (new-game {:runner {:hand ["Doppelgänger" "The Maker's Eye"]}
                :corp {:deck [(qty "Hostile Takeover" 20)]
                       :hand [(qty "Hostile Takeover" 3)]}})
     (take-credits state :corp)
     (play-from-hand state :runner "Doppelgänger")
     (play-run-event state "The Maker's Eye" :rd)
     (do (dotimes [_ 3]
           (click-prompt state :runner "Steal"))
         (is (not (:run @state)))
         (click-prompt state :runner "Yes")
         (click-prompt state :runner "R&D")
         (run-continue state)
         (is (:run @state) "New run started")
         (run-continue state)
         (click-prompt state :runner "Steal")
         (is (not (:run @state))
             "The Maker's Eye only gives bonus accesses on its own run when combined with Doppelgänger"))))

(deftest the-noble-path
  ;; The Noble Path - Prevents damage during run
  (do-game
    (new-game {:runner {:hand ["The Noble Path" "Sports Hopper" "Sure Gamble"]
                        :deck [(qty "Sure Gamble" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Sports Hopper")
    (play-from-hand state :runner "The Noble Path")
    (is (zero? (count (:hand (get-runner)))) "Playing Noble Path trashes the remaining cards in hand")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (card-ability state :runner (get-hardware state 0) 0)
    (damage state :runner :net 2)
    (is (= 2 (count (:hand (get-runner)))) "Damage was prevented")
      ;; Finish the run and check that damage works again
    (run-continue state)
    (click-prompt state :runner "No action")
    (damage state :runner :net 2)
    (is (zero? (count (:hand (get-runner)))) "Damage works again after run")))

(deftest the-price
  ;; trash the top 4, install one paying 3 less
  (do-game
    (new-game {:runner {:deck ["Liberated Account", "Daily Casts", "Earthrise Hotel", "Rezeki"]
                        :hand ["The Price"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "The Price")
    (click-prompt state :runner "Rezeki")
    (is (= 4 (:credit (get-runner))))
    (is (= 4 (count (:discard (get-runner)))))) "3 + price")

(deftest the-price-of-freedom
  ;; The Price of Freedom - A connection must be trashed, the card is removed from game, then the corp can't advance cards next turn
  (do-game
    (new-game {:corp {:deck ["NAPD Contract"]}
               :runner {:deck ["Kati Jones" "The Price of Freedom"]}})
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp has 7 credits (play NAPD + 2 clicks for credit)")
    (play-from-hand state :runner "The Price of Freedom")
    (is (= 2 (count (get-in @state [:runner :hand]))) "The Price of Freedom could not be played because no connection is installed")
    (is (zero? (count (get-in (get-runner) [:rig :resource]))) "Kati Jones is not installed")
    (play-from-hand state :runner "Kati Jones")
    (is (= 1 (count (get-resource state))) "Kati Jones was installed")
    (play-from-hand state :runner "The Price of Freedom")
    (let [kj (get-resource state 0)]
      (click-card state :runner kj)
      (is (zero? (count (get-in @state [:runner :hand]))) "The Price of Freedom can be played because a connection is in play")
      (is (zero? (count (get-in (get-runner) [:rig :resource]))) "Kati Jones was trashed wth The Price of Freedom")
      (is (= 1 (count (:discard (get-runner)))) "The Price of Freedom was removed from game, and only Kati Jones is in the discard"))
    (take-credits state :runner)
    (let [napd (get-content state :remote1 0)]
      (click-advance state :corp (refresh napd))
      (is (= 7 (:credit (get-corp))) "NAPD contract could not be advanced because of The Price of Freedom")
      (take-credits state :corp)
      (is (= 10 (:credit (get-corp))) "Corp has 10 credits now (3 clicks for credit, no click charged for failed advancing)")
      (take-credits state :runner)
      (click-advance state :corp (refresh napd))
      (click-advance state :corp (refresh napd))
      (click-advance state :corp (refresh napd))
      (is (= 7 (:credit (get-corp))) "NAPD could be advanced (3 credits charged for advancing)"))))

(deftest three-steps-ahead
  ;; Three Steps Ahead
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand ["Three Steps Ahead"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Three Steps Ahead")
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (let [credits (:credit (get-runner))]
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (take-credits state :runner)
      (is (= (+ credits 6) (:credit (get-runner))) "Runner should gain 6 for 3 successful runs"))))

(deftest tinkering
  ;; Tinkering - Add subtypes to ice
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Tinkering"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Tinkering")
    (let [iwall (get-ice state :hq 0)]
      (click-card state :runner iwall)
      (is (has-icon? state (refresh iwall) "T") "Ice Wall has an icon")
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (rez state :corp (refresh iwall))
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (take-credits state :runner)
      (is (no-icons? state (refresh iwall)))
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (not (has-subtype? (refresh iwall) "Code Gate")) "Ice Wall does not have Code Gate")
      (is (not (has-subtype? (refresh iwall) "Sentry")) "Ice Wall does not have Sentry"))))

(deftest transfer-of-wealth
  (do-game
    (new-game {:runner {:hand ["Transfer of Wealth"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Transfer of Wealth")
    (run-continue state)
    (is (changed? [(:credit (get-corp)) -3
                   (:credit (get-runner)) +6
                   (count-tags state) +1
                   (:click (get-runner)) 0]
          (run-continue state))
        "Siphoned bigly")
    (click-prompt state :runner "No action")))

(deftest transfer-of-wealth-vs-redirect
  (do-game
    (new-game {:runner {:hand ["Transfer of Wealth"]}
               :corp {:hand ["Proprionegation"]}})
    (play-and-score state "Proprionegation")
    (take-credits state :corp)
    (play-from-hand state :runner "Transfer of Wealth")
    (card-ability state :corp (get-scored state :corp 0) 0)
    (is (changed? [(:credit (get-corp)) 0
                   (:credit (get-runner)) 0
                   (count-tags state) 0
                   (:click (get-runner)) 0]
          (run-continue-until state :success))
        "Not siphoned")
    (is (no-prompt? state :runner))))

(deftest trade-in
  ;; Trade-in - trash an installed Hardware, gain credits equal to half of install cost,
  ;;            search stack for Hardware and add to grip
  (do-game
    (new-game {:runner {:deck [(qty "Trade-In" 3) (qty "Astrolabe" 2) (qty "Sports Hopper" 2)]}
               :options {:start-as :runner}})
    (starting-hand state :runner ["Trade-In" "Trade-In" "Astrolabe" "Sports Hopper"])
    (core/gain state :runner :click 5 :credit 5)
    (play-from-hand state :runner "Astrolabe")
    (play-from-hand state :runner "Sports Hopper")
    (testing "Trade-in works with Hardware costing 0 or 1 credits (issue #3750)"
      (let [runner-credits (:credit (get-runner))]
        (play-from-hand state :runner "Trade-In")
        (click-card state :runner (get-hardware state 0))
        (is (= 1 (count (:discard (get-runner)))) "Astrolabe in discard")
        (is (= "Astrolabe" (:title (first (:discard (get-runner))))) "Astrolabe in discard")
        (is (= (dec runner-credits) (:credit (get-runner)))
            "Paid 1 credit to play Trade-In and gained 0 credits from trashing Astrolabe")))
    (testing "Trade-In lets runner search for Hardware and add it to Grip"
      (is (= 1 (count (:hand (get-runner)))) "Only 1 Trade-In in Grip")
      ;; Add sports hopper to hand
      (click-prompt state :runner (first (prompt-buttons :runner)))
      (is (= 2 (count (:discard (get-runner)))) "Trade-In now in the heap as well")
      (is (= 2 (count (:hand (get-runner)))) "Sports Hopper added to Grip"))
    (testing "Gain credits when install cost is greater than 1"
      (let [runner-credits (:credit (get-runner))]
        (play-from-hand state :runner "Trade-In")
        (click-card state :runner (get-hardware state 0))
        (is (= runner-credits (:credit (get-runner)))
            "Paid 1 credit to play Trade-In and gained 1 credits from trashing Sports Hopper")
        (click-prompt state :runner (first (prompt-buttons :runner)))
        (is (= 4 (count (:discard (get-runner)))) "2 Trade-In, 1 Astrolabe and 1 Sports Hopper in discard")))))

(deftest traffic-jam
  ;; Traffic Jam - Increase adv requirement based on previously scored copies
  (do-game
    (new-game {:corp {:deck [(qty "TGTBT" 3)]}
               :runner {:deck ["Traffic Jam"]}})
    (play-from-hand state :corp "TGTBT" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "TGTBT" "New remote")
    (score-agenda state :corp (get-content state :remote2 0))
    (play-from-hand state :corp "TGTBT" "New remote")
    (take-credits state :corp)
    (let [tg (get-content state :remote3 0)]
      (play-from-hand state :runner "Traffic Jam")
      (take-credits state :runner)
      (core/gain state :corp :click 2)
      (advance state tg 3)
      (score state :corp (refresh tg))
      (is (= 2 (:agenda-point (get-corp))) "Last TGTBT not scored")
      (is (= 1 (count (get-content state :remote3))))
      (advance state (refresh tg) 1)
      (is (= 4 (get-counters (refresh tg) :advancement)))
      (score state :corp (refresh tg))
      (is (= 2 (:agenda-point (get-corp))) "Not scored with 4 advancements")
      (advance state (refresh tg) 1)
      (is (= 5 (get-counters (refresh tg) :advancement)))
      (score state :corp (refresh tg))
      (is (= 3 (:agenda-point (get-corp))) "Took 5 advancements to score"))))

(deftest tread-lightly
  ;; Tread Lightly
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                      :hand ["Ice Wall" "Vanilla" "Crisium Grid"]
                      :credits 100}
               :runner {:hand ["Tread Lightly"]}})
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Crisium Grid" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Tread Lightly")
    (click-prompt state :runner "HQ")
    (run-continue state)
    (let [vanilla (get-ice state :hq 0)
          icewall (get-ice state :hq 1)
          cgrid (get-content state :hq 0)]
      (is (changed? [(:credit (get-corp)) -3]
            (rez state :corp cgrid))
          "Paid 3 credits to rez Crisium Grid")
      (is (changed? [(:credit (get-corp)) -3]
            (rez state :corp vanilla))
          "Paid 3 credits to rez Vanilla")
      (run-continue state)
      (is (changed? [(:credit (get-corp)) -4]
            (rez state :corp icewall))
          "Paid 4 credits to rez Ice Wall"))))

(deftest trick-shot
  (before-each [state (new-game {:corp {:deck [(qty "Hedge Fund" 10)]
                                        :hand ["Spin Doctor" "NGO Front"]}
                                 :runner {:hand ["Trick Shot"]}})
                _ (do (play-from-hand state :corp "Spin Doctor" "New remote")
                      (play-from-hand state :corp "NGO Front" "New remote")
                      (take-credits state :corp)
                      (play-from-hand state :runner "Trick Shot")
                      (run-continue state))
                ts (-> (:runner @state) :play-area first)]
    (testing "basic"
      (do-game state
        (is (= :rd (first (get-in @state [:run :server]))))
        (is (= 4 (get-counters (refresh ts) :credit)) "Trick Shot has 4 credits on it")
        (is (changed? [(get-counters (refresh ts) :credit) 2]
              (run-continue state))
            "Trick Shot gains 2 credits on successful run")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Server 1")
        (is (= :remote1 (first (get-in @state [:run :server]))))
        (is (= 6 (get-counters (refresh ts) :credit)) "Trick Shot still has 6 credits on it")))
    (testing "only 2 runs please"
      (do-game state
        (run-continue state)
        (is (= "You accessed Hedge Fund." (:msg (prompt-map :runner))))
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (= "Choose a remote server to run" (:msg (prompt-map :runner))))
        (click-prompt state :runner "Server 1")
        (is (some? (:phase (:run @state))))
        (run-continue state)
        (run-continue state)
        (is (= "You accessed Spin Doctor." (:msg (prompt-map :runner))))
        (click-prompt state :runner "No action")
        (is (no-prompt? state :runner))
        (is (nil? (:phase (:run @state))) "Run is finished")))))

(deftest trick-shot-aginfusion-interaction
  (do-game
    (new-game {:corp {:id "AgInfusion: New Miracles for a New World"
                      :deck [(qty "Hedge Fund" 10)]
                      :hand ["Vanilla"]}
               :runner {:hand ["Trick Shot"]}})
    (play-from-hand state :corp "Vanilla" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Trick Shot")
    (run-continue state)
    (run-continue state)
    (click-prompt state :runner "No action")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Server 1")
    (is (= :remote1 (first (get-in @state [:run :server]))))
    (run-continue state)
    (card-ability state :corp (:identity (get-corp)) 0)
    (click-prompt state :corp "R&D")
    (run-continue state)
    (click-prompt state :runner "No action")
    (is (no-prompt? state :runner))))

(deftest uninstall
  ;; Uninstall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:hand [(qty "Uninstall" 2) "Corroder" "Clone Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Clone Chip")
    (play-from-hand state :runner "Uninstall")
    (click-card state :runner "Corroder")
    (play-from-hand state :runner "Uninstall")
    (click-card state :runner "Clone Chip")
    (is (= 2 (count (:hand (get-runner)))) "Runner has played 2 and picked up 2 cards")))

(deftest unscheduled-maintenance
  ;; Unscheduled Maintenance - prevent Corp from installing more than 1 piece of ice per turn
  (do-game
    (new-game {:corp {:deck [(qty "Vanilla" 2) "Breaking News"]}
               :runner {:deck ["Unscheduled Maintenance"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Unscheduled Maintenance")
    (take-credits state :runner)
    (play-from-hand state :corp "Vanilla" "HQ")
    (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "First piece of ice install of turn allowed")
    (play-from-hand state :corp "Vanilla" "R&D")
    (is (empty? (get-in @state [:corp :servers :rd :ices])) "Second piece of ice install of turn blocked")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Vanilla" "R&D")
    (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Current trashed; second piece of ice install of turn allowed")))

(deftest vamp
  ;; Vamp - Run HQ and use replace access to pay credits to drain equal amount from Corp
  (do-game
    (new-game {:runner {:deck ["Vamp" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))))
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 13 (:credit (get-runner))))
    (play-run-event state "Vamp" :hq)
    (click-prompt state :runner "Vamp")
    (click-prompt state :runner "8")
    (is (= 1 (count-tags state)) "Took 1 tag")
    (is (= 5 (:credit (get-runner))) "Paid 8 credits")
    (is (zero? (:credit (get-corp))) "Corp lost all 8 credits")))

(deftest vrcation
  ;; VRcation
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 2)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["VRcation"]}})
      (take-credits state :corp)
      (let [hand (count (:hand (get-runner)))
            clicks (:click (get-runner))]
        (play-from-hand state :runner "VRcation")
        (is (= (+ hand -1 4) (count (:hand (get-runner)))) "Runner plays VRcation and draws 4 cards")
        (is (= (+ clicks -1 -1) (:click (get-runner))) "Runner plays VRcation and loses 1 click"))))

(deftest wanton-destruction
  ;; Wanton Destruction
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Wanton Destruction"]}})
      (take-credits state :corp)
      (play-run-event state "Wanton Destruction" :hq)
      (click-prompt state :runner "Wanton Destruction")
      (is (zero? (count (:discard (get-corp)))) "Corp should have no cards in Archives")
      (click-prompt state :runner "3")
      (is (= 2 (count (:hand (get-corp)))) "Corp should discard 3 cards")
      (is (= 3 (count (:discard (get-corp)))) "Corp should now have 3 cards in Archives")
      (is (zero? (:click (get-runner))) "Runner should spend 3 clicks on ability")))

(deftest wanton-destruction-can-choose-0-for-wanton-destruction-issue-4618
    ;; Can choose 0 for Wanton Destruction. Issue #4618
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Wanton Destruction"]}})
      (take-credits state :corp)
      (play-run-event state "Wanton Destruction" :hq)
      (click-prompt state :runner "Wanton Destruction")
      (is (zero? (count (:discard (get-corp)))) "Corp should have no cards in Archives")
      (click-prompt state :runner "0")
      (is (= 5 (count (:hand (get-corp)))) "Corp should not have discarded cards")
      (is (= 0 (count (:discard (get-corp)))) "Corp should not have discarded cards")
      (is (= 3 (:click (get-runner))) "Runner should spend 0 clicks on ability")
      (is (not (:run @state)) "Run ended")))

(deftest watch-the-world-burn-standard-usage
    ;; Standard usage
    (do-game
     (new-game {:corp {:deck [(qty "PAD Campaign" 2) (qty "Launch Campaign" 3)]}
                :runner {:deck ["Watch the World Burn" (qty "Same Old Thing" 2)]}})
     (play-from-hand state :corp "PAD Campaign" "New remote")
     (play-from-hand state :corp "PAD Campaign" "New remote")
     (play-from-hand state :corp "Launch Campaign" "New remote")
     (take-credits state :corp)
     (is (changed? [(count (get-in @state [:corp :rfg])) 1]
           (play-from-hand state :runner "Watch the World Burn")
           (click-prompt state :runner "Server 1")
           (run-continue state)
           (run-continue state))
         "Server 1 PAD Campaign RFGed")
     (is (zero? (:click (get-runner))) "Terminal event ends the action phase")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (changed? [(count (get-in @state [:corp :rfg])) 1]
           (run-empty-server state "Server 2"))
         "Server 2 PAD Campaign RFGed even on the next turn")
     (is (changed? [(count (get-in @state [:corp :rfg])) 0]
           (run-empty-server state "Server 3"))
         "Server 3 Launch Campaign not RFGed")
     (click-prompt state :runner "No action")
     (core/move state :runner (find-card "Watch the World Burn" (:discard (get-runner))) :hand)
     (core/gain state :runner :credit 3) ;need to be able to play the card
     (is (changed? [(count (get-in @state [:corp :rfg])) 1]
           (play-from-hand state :runner "Watch the World Burn")
           (click-prompt state :runner "Server 3")
           (run-continue state)
           (run-continue state))
         "Server 3 Launch Campaign RFGed")
     (is (zero? (:click (get-runner))) "Terminal event ends the action phase")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (changed? [(count (get-in @state [:corp :rfg])) 1]
           (run-empty-server state "HQ"))
         "HQ Launch Campaign RFGed")))

(deftest watch-the-world-burn-mid-run-accesses
    ;; Mid-run accesses
    (do-game
     (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 2)
                              "Kitsune"
                              "Ice Wall"]}
                :runner {:deck ["Watch the World Burn" (qty "Same Old Thing" 2)]
                         :credits 10}})
     (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
     (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
     (play-from-hand state :corp "Kitsune" "Server 1")
     (take-credits state :corp)
     (play-from-hand state :runner "Watch the World Burn")
     (click-prompt state :runner "Server 1")
     (run-continue state)
     (let [kitsune (get-ice state :remote1 0)]
        (rez state :corp kitsune)
        (run-continue state)
        (card-subroutine state :corp kitsune 0)
        (click-prompt state :corp "Yes")
        (is (changed? [(count (get-in @state [:corp :rfg])) 0]
              (click-card state :corp (find-card "Ice Wall" (:hand (get-corp)))))
            "HQ Ice Wall not RFGed")
        (click-prompt state :runner "No action"))
     (is (>= (:credit (get-runner)) 5) "Runner can trash MVT if they want to")
     (is (changed? [(:credit (get-runner)) 0]
           (run-continue state))
         "Server 1 MVT doesn't trigger")
     (is (= 1 (count (get-in @state [:corp :rfg]))) "MVT was RFGed")
     (take-credits state :runner)
     (take-credits state :corp)
     (is (changed? [(:credit (get-runner)) 0]
           (run-empty-server state "Server 2"))
         "Server 2 MVT doesn't trigger")
     (is (= 2 (count (get-in @state [:corp :rfg]))) "MVT was RFGed")))

(deftest white-hat
  ;; White Hat
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Fire Wall" "Enigma"]}
               :runner {:deck ["White Hat"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (play-from-hand state :runner "White Hat")
    (is (waiting? state :runner) "Runner is waiting for Corp to boost")
    (click-prompt state :corp "0")
    (click-prompt state :runner "4")
    (click-card state :runner  "Ice Wall")
    (click-card state :runner "Enigma")
    (is-deck? state :corp ["Ice Wall" "Enigma"])))

(deftest white-hat-single-card-selected
  ;; White Hat - can shuffle back just a single card
  (do-game
    (new-game {:corp {:hand ["Ice Wall" "Enigma"]}
               :runner {:hand ["White Hat"]}})
    (take-credits state :corp)
    (run-empty-server state :archives)
    (play-from-hand state :runner "White Hat")
    (click-prompt state :corp "0")
    (click-prompt state :runner "4")
    (is (= "Shuffle up to 2 cards into R&D" (:msg (prompt-map :runner))))
    (click-card state :runner "Ice Wall")
    (click-prompt state :runner "Done")
    (is (no-prompt? state :runner))))

(deftest wildcat-strike
  ;; Wildcat Strike
  (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Wildcat Strike"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Wildcat Strike")
      (is (changed? [(:credit (get-runner)) 6]
            (click-prompt state :corp "Runner gains 6 [Credits]"))
          "gain 6 credits from Wildcat Strike"))
  (testing "Basic test - corp selects credits"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand [(qty "Hedge Fund" 5)]}
                 :runner {:deck [(qty "Sure Gamble" 5)]
                          :hand ["Wildcat Strike"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Wildcat Strike")
      (is (changed? [(count (:hand (get-runner))) 4]
            (click-prompt state :corp "Runner draws 4 cards"))
          "draw 4 cards from Wildcat Strike"))))

(deftest windfall
  ;; Windfall
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
               :runner {:deck ["Monolith"]
                        :hand ["Windfall"]}})
    (take-credits state :corp)
    (let [num-shuffles (count (core/turn-events state :runner :runner-shuffle-deck))
          credits (:credit (get-runner))]
      (play-from-hand state :runner "Windfall")
      (is (= (inc num-shuffles) (count (core/turn-events state :runner :runner-shuffle-deck)))
          "Runner should shuffle the stack")
      (is (= (+ credits 18) (:credit (get-runner))) "Runner should gain credits from trash")
      (is (= "Monolith" (-> (get-runner) :discard first :title))
          "Monolith should be trashed"))))

(deftest window-of-opportunity
  (do-game
    (new-game {:corp {:hand ["Echo" "Bloop"]
                      :credits 20}
               :runner {:hand ["Window of Opportunity" "Simulchip" "Fermenter" "The Class Act"]}})
    (play-from-hand state :corp "Bloop" "HQ")
    (play-from-hand state :corp "Echo" "Archives")
    (let [bloop (get-ice state :hq 0)
          echo (get-ice state :archives 0)]
      (rez state :corp echo)
      (rez state :corp bloop {:expect-rez false})
      (click-card state :corp (refresh echo))
      (take-credits state :corp)
      (play-from-hand state :runner "Window of Opportunity")
      (click-prompt state :runner "HQ")
      (is (changed? [(:credit (get-runner)) -1]
                    (click-card state :runner "Fermenter"))
          "Runner paid Fermenter install cost")
      (is (= "Fermenter" (:title (get-program state 0))) "Fermenter is installed")
      (is (= [:hq] (:server (:run @state))) "Running on HQ")
      (is (not (no-prompt? state :runner)) "Window of Opportunity prompt to select ice to derez")
      (click-card state :runner bloop)
      (is (not (rezzed? (refresh bloop))) "Bloop derezzed")
      (run-continue state)
      (run-continue state)
      (run-jack-out state)
      (is (changed? [(:credit (get-corp)) 0]
                    (click-prompt state :corp "Yes"))
          "Corp re-rezzed Bloop ignoring all costs")
      (is (no-prompt? state :runner) "No Bloop prompt to derez an Harmonic ice")
      (is (rezzed? (refresh bloop))))))

(deftest window-of-opportunity-no-derez
  (do-game
    (new-game {:corp {:hand ["Vanilla"]}
               :runner {:hand ["Window of Opportunity" "Simulchip"]}})
    (play-from-hand state :corp "Vanilla" "Archives")
    (take-credits state :corp)
    (play-from-hand state :runner "Window of Opportunity")
    (click-prompt state :runner "Archives")
    (click-card state :runner "Simulchip")
    (is (= "Simulchip" (:title (get-hardware state 0))) "Simulchip is installed")
    (is (= [:archives] (:server (:run @state))) "Running on Archives")
    (is (no-prompt? state :runner) "No Window of Opportunity prompt")))
