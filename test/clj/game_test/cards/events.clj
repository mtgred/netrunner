(ns game-test.cards.events
  (:require [game.core :as core]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [jinteki.utils :refer [count-tags has-subtype?]]
            [clojure.test :refer :all]))

(deftest account-siphon
  ;; Account Siphon
  (testing "Use ability"
    (do-game
      (new-game {:runner {:deck [(qty "Account Siphon" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Account Siphon, use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Replacement effect")
      (is (= 2 (count-tags state)) "Runner took 2 tags")
      (is (= 15 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))
  (testing "Access"
    (do-game
      (new-game {:runner {:deck [(qty "Account Siphon" 3)]}})
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play another Siphon, do not use ability
      (play-run-event state "Account Siphon" :hq)
      (click-prompt state :runner "Access cards")
      (is (zero? (count-tags state)) "Runner did not take any tags")
      (is (= 5 (:credit (get-runner))) "Runner did not gain any credits")
      (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")))
  (testing "New Angeles City Hall interaction"
    ;; Account Siphon - Access
    (do-game
      (new-game {:runner {:deck ["Account Siphon"
                                 "New Angeles City Hall"]}})
      (core/gain state :corp :bad-publicity 1)
      (is (= 1 (:bad-publicity (get-corp))) "Corp has 1 bad publicity")
      (core/lose state :runner :credit 1)
      (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      (play-from-hand state :runner "New Angeles City Hall")
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let [nach (get-resource state 0)]
        (play-run-event state (first (get-in @state [:runner :hand])) :hq)
        (click-prompt state :runner "Replacement effect")
        (is (= 4 (:credit (get-runner))) "Runner still has 4 credits due to BP")
        (card-ability state :runner nach 0)
        (is (= 2 (:credit (get-runner))) "Runner has 2 credits left")
        (card-ability state :runner nach 0)
        (is (zero? (:credit (get-runner))) "Runner has no credits left")
        (click-prompt state :runner "Done"))
      (is (zero? (count-tags state)) "Runner did not take any tags")
      (is (= 10 (:credit (get-runner))) "Runner gained 10 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits"))))

(deftest amped-up
  ;; Amped Up - Gain 3 clicks and take 1 unpreventable brain damage
  (do-game
    (new-game {:runner {:deck ["Amped Up"
                               "Feedback Filter"
                               (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Amped Up")
    (is (empty? (:prompt (get-runner)))
        "Feedback Filter brain damage prevention opportunity not given")
    (is (= 5 (:click (get-runner))) "Runner gained 2 clicks from Amped Up")
    (is (= 2 (count (:discard (get-runner)))) "Runner discarded 1 card from damage")
    (is (= 4 (core/hand-size state :runner)) "Runner handsize decreased by 1")
    (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")))

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

(deftest apocalypse
  ;; Apocalypse
  (testing "Ensure MU is correct and no duplicate cards in heap"
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
      (let [scheherazade (get-program state 0)]
        (card-ability state :runner scheherazade 0)
        (click-card state :runner (find-card "Corroder" (:hand (get-runner))))
        (is (= 3 (core/available-mu state)) "Memory at 3 (-1 from Corroder)"))
      (play-from-hand state :runner "Hivemind")
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
  (testing "with Full Immersion - no duplicate cards in heap #2606"
    (do-game
      (new-game {:corp {:deck ["Full Immersion RecStudio" "Sandburg"
                               "Oaktown Renovation"]}
                 :runner {:deck ["Apocalypse"]}})
      (play-from-hand state :corp "Full Immersion RecStudio" "New remote")
      (let [fir (get-content state :remote1 0)]
        (core/rez state :corp fir)
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
  (testing "with Hostile Infrastructure - should take damage equal to 2x cards on the table"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Infrastructure" 2) (qty "Ice Wall" 2)]}
                 :runner {:deck ["Apocalypse" (qty "Sure Gamble" 9)]}})
      (core/gain state :corp :click 1)
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (core/rez state :corp (get-content state :remote1 0) {:ignore-cost true})
      (core/rez state :corp (get-content state :remote4 0) {:ignore-cost true})
      (take-credits state :corp)
      (core/draw state :runner 5)
      (is (= 10 (count (:hand (get-runner)))) "Runner has 9 cards in hand")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (run-empty-server state "HQ")
      (play-from-hand state :runner "Apocalypse")
      (is (zero? (count (core/all-installed state :corp))) "All installed Corp cards trashed")
      (is (= 4 (count (:discard (get-corp)))) "4 Corp cards in Archives")
      (is (= 1 (count (:hand (get-runner)))) "Runner has one card in hand")
      (is (= 9 (count (:discard (get-runner)))) "There are 9 cards in heap")))
  (testing "Turn Runner cards facedown and reduce memory and hand-size gains"
    (do-game
      (new-game {:corp {:deck [(qty "Launch Campaign" 2) "Ice Wall"]}
                 :runner {:deck ["Logos" "Apocalypse" (qty "Origami" 2)]}})
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (play-from-hand state :corp "Launch Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Logos")
      (is (= 1 (get-in (get-runner) [:hand-size :mod])) "Hand-size increased from Logos")
      (is (= 5 (core/available-mu state)) "Memory increased from Logos")
      (play-from-hand state :runner "Origami")
      (play-from-hand state :runner "Origami")
      (is (= 5 (get-in (get-runner) [:hand-size :mod])) "Hand-size increased from Logos and Origami")
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
        (is (zero? (get-in (get-runner) [:hand-size :mod])) "Hand-size reset with Logos and Origami facedown")
        (is (= 4 (core/available-mu state)) "Memory reset with Logos and Origami facedown"))))
(testing "Turn Runner cards facedown without firing their trash effects"
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
      (core/trash state :runner tmc)
      (is (= 3 (count (:hand (get-runner))))
          "No meat damage dealt by trashing facedown Tri-maf")))))

(deftest because-i-can
  ;; make a successful run on a remote to shuffle its contents into R&D
  (do-game
    (new-game {:corp {:deck ["Sand Storm" "PAD Campaign" "Project Atlas" (qty "Shell Corporation" 2)]}
               :runner {:deck [(qty "Because I Can" 2)]}})
    (play-from-hand state :corp "Shell Corporation" "New remote")
    (play-from-hand state :corp "Shell Corporation" "Server 1")
    (play-from-hand state :corp "Project Atlas" "Server 1")
    (take-credits state :corp)
    (let [n (count (get-in @state [:corp :deck]))]
      (play-from-hand state :runner "Because I Can")
      (click-prompt state :runner "Server 1")
      (is (= 3 (count (get-in @state [:corp :servers :remote1 :content])))
          "3 cards in server 1 before successful run")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= (+ n 3) (count (get-in @state [:corp :deck]))) "3 cards were shuffled into R&D")
      (is (zero? (count (get-in @state [:corp :servers :remote1 :content]))) "No cards left in server 1"))
    (take-credits state :runner)
    (play-from-hand state :corp "Sand Storm" "New remote")
    (play-from-hand state :corp "PAD Campaign" "New remote")
    (take-credits state :corp)
    (let [n (count (get-in @state [:corp :deck]))
          sand-storm (get-ice state :remote2 0)]
      (play-from-hand state :runner "Because I Can")
      (click-prompt state :runner "Server 2")
      (core/rez state :corp sand-storm)
      (is (= :remote2 (first (get-in @state [:run :server]))))
      (card-subroutine state :corp sand-storm 0)
      (click-prompt state :corp "Server 3")
      (is (= :remote3 (first (get-in @state [:run :server]))))
      (is (= 1 (count (get-in @state [:corp :servers :remote3 :content]))) "1 cards in server 3 before successful run")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= (inc n) (count (get-in @state [:corp :deck]))) "1 card was shuffled into R&D")
      (is (zero? (count (get-in @state [:corp :servers :remote3 :content]))) "No cards left in server 3"))))

(deftest black-hat
  ;; Black Hat
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
                 :runner {:deck [(qty "Black Hat" 3)]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Black Hat")
      (click-prompt state :corp "0")
      (click-prompt state :runner "4")
      (run-on state :rd)
      (run-successful state)
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Card from deck")))
  (testing "Kitsune interaction"
    (do-game
      (new-game {:corp {:deck [(qty "Kitsune" 10)]}
                 :runner {:deck [(qty "Black Hat" 3)]}})
      (starting-hand state :corp ["Kitsune" "Kitsune" "Kitsune" "Kitsune" "Kitsune"])
      (play-from-hand state :corp "Kitsune" "R&D")
      (let [kitsune (get-ice state :rd 0)]
        (core/rez state :corp kitsune)
        (take-credits state :corp)
        (core/gain state :runner :credit 10)
        (play-from-hand state :runner "Black Hat")
        (click-prompt state  :corp "0")
        (click-prompt state :runner "4")
        (run-on state :rd)
        (card-subroutine state :corp kitsune 0)
        (click-card state :corp (find-card "Kitsune" (:hand (get-corp))))
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")))))

(deftest blackmail
  ;; Prevent rezzing of ice for one run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Blackmail" 3)]}})
      (is (= 1 (get-in @state [:corp :bad-publicity])) "Corp has 1 bad-publicity")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Blackmail")
      (click-prompt state :runner "HQ")
      (let [iwall1 (get-ice state :hq 0)
            iwall2 (get-ice state :hq 1)]
        (core/rez state :corp iwall1)
        (is (not (:rezzed (refresh iwall1))) "First Ice Wall is not rezzed")
        (run-continue state)
        (core/rez state :corp iwall2)
        (is (not (:rezzed (refresh iwall2))) "Second Ice Wall is not rezzed")
        (core/jack-out state :runner nil)
        ;; Do another run, where the ice should rez
        (run-on state "HQ")
        (core/rez state :corp iwall1)
        (is (:rezzed (refresh iwall1)) "First Ice Wall is rezzed"))))
  (testing "Regression test for a rezzed tmi breaking game state on a blackmail run"
    (do-game
      (new-game {:corp {:deck [(qty "TMI" 3)]}
                 :runner {:id "Valencia Estevez: The Angel of Cayambe"
                          :deck [(qty "Blackmail" 3)]}})
      (is (= 1 (get-in @state [:corp :bad-publicity])) "Corp has 1 bad-publicity")
      (play-from-hand state :corp "TMI" "HQ")
      (let [tmi (get-ice state :hq 0)]
        (core/rez state :corp tmi)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (:rezzed (refresh tmi)) "TMI is rezzed")
        (take-credits state :corp)
        (play-from-hand state :runner "Blackmail")
        (click-prompt state :runner "HQ")
        (run-continue state)
        (run-jack-out state)
        (run-on state "Archives")))))

(deftest blueberry-diesel
  ;; Blueberry Diesel
  (testing "Selecting a card"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:hand ["Blueberry Diesel" "Sure Gamble" "Easy Mark" "Daily Casts"]}})
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Daily Casts" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "Blueberry Diesel")
      (is (= "Daily Casts" (-> (get-runner) :deck last :title)))
      (click-prompt state :runner "Sure Gamble")
      (is (find-card "Daily Casts" (:hand (get-runner))))))
  (testing "Selecting no card"
    (do-game
      (new-game {:options {:start-as :runner}
                 :runner {:hand ["Blueberry Diesel" "Sure Gamble" "Easy Mark" "Daily Casts"]}})
      (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Easy Mark" (:hand (get-runner))) :deck)
      (core/move state :runner (find-card "Daily Casts" (:hand (get-runner))) :deck)
      (play-from-hand state :runner "Blueberry Diesel")
      (is (= "Daily Casts" (-> (get-runner) :deck last :title)))
      (click-prompt state :runner "Cancel")
      (is (not (find-card "Daily Casts" (:hand (get-runner))))))))

(deftest by-any-means
  ;; By Any Means
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Ice Wall" "Paper Trail" "PAD Campaign"
                               "Project Junebug"]}
                 :runner {:deck ["By Any Means" (qty "Sure Gamble" 5)]}})
      (take-credits state :corp)
      (run-empty-server state "Archives")
      ; (play-from-hand state :runner "By Any Means")
      (is (= 3 (:click (get-runner))) "Card not played, priority restriction")
      (take-credits state :runner)
      (starting-hand state :corp ["Paper Trail" "Hedge Fund" "PAD Campaign" "Project Junebug"])
      (play-from-hand state :corp "Paper Trail" "New remote")
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (play-from-hand state :corp "Project Junebug" "New remote")
      (core/add-counter state :corp (get-content state :remote3 0) :advancement 2)
      (take-credits state :corp)
      (core/gain state :runner :click 2)
      (core/draw state :runner)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "HQ")
      (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
      (is (= 4 (count (:hand (get-runner)))) "Took 1 meat damage")
      (run-empty-server state "R&D")
      (is (= 2 (count (:discard (get-corp)))) "ICE was trashed")
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
  (testing "vs Controlling the Message"
    (do-game
      (new-game {:corp {:id "NBN: Controlling the Message"
                        :deck ["Paper Trail"]}
                 :runner {:deck [(qty "By Any Means" 2)]}})
      (play-from-hand state :corp "Paper Trail" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "Server 1")
      (click-prompt state :corp "No") ;; Don't trigger CTM trace
      (is (empty? (:prompt (get-runner))) "No prompt to steal since agenda was trashed")
      (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))
  (testing "alongside Film Critic: should get the option to trigger either"
    (do-game
      (new-game {:corp {:deck [(qty "Hostile Takeover" 2)]}
                 :runner {:deck ["By Any Means" "Film Critic" (qty "Sure Gamble" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "By Any Means")
      (play-from-hand state :runner "Film Critic")
      (is (= 1 (count (:discard (get-runner)))) "By Any Means has been played")
      (run-empty-server state "HQ")
      (is (= #{"Film Critic" "By Any Means"}
             (->> (get-runner) :prompt first :choices (into #{}))) "A choice of which to trigger first")
      (click-prompt state :runner "Film Critic")
      (click-prompt state :runner "No")
      (is (= 1 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (= 1 (count (:hand (get-runner)))) "Took 1 meat damage")
      (take-credits state :runner)
      (take-credits state :corp)
      (core/move state :runner (find-card "By Any Means" (:discard (get-runner))) :hand)
      (play-from-hand state :runner "By Any Means")
      (run-empty-server state "HQ")
      (is (= #{"Film Critic" "By Any Means"}
             (->> (get-runner) :prompt first :choices (into #{}))) "A choice of which to trigger first")
      (click-prompt state :runner "By Any Means")
      (is (nil? (->> (get-runner) :prompt first :choices)) "By Any Means trashes with no prompt")
      (is (= 2 (count (:discard (get-corp)))) "Agenda was trashed")
      (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))
  (testing "Effect persists when moved from discard"
    (do-game
     (new-game {:corp {:id "Skorpios Defense Systems: Persuasive Power"}
                :runner {:deck [(qty "By Any Means" 2)]}})
     (take-credits state :corp)
     (play-from-hand state :runner "By Any Means")
     (card-ability state :corp (get-in @state [:corp :identity]) 0)
     (click-prompt state :corp (find-card "By Any Means" (:discard (get-runner))))
     (is (= 1 (count (get-in @state [:runner :rfg]))) "By Any Means RFGed")
     (is (zero? (count (:discard (get-corp)))) "Nothing trashed yet")
     (is (= 1 (count (:hand (get-runner)))) "No damage yet")
     (run-empty-server state "HQ")
     (is (= 1 (count (:discard (get-corp)))) "Operation was trashed")
     (is (zero? (count (:hand (get-runner)))) "Took 1 meat damage")))
  (testing "Effect does not persist between turns"
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
     (is (= 1 (count (:hand (get-runner)))) "No damage"))))

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
      (is (:prompt (get-runner)) "Can't target card in central server")
      (click-card state :runner v2)
      (core/rez state :corp v2)
      (is (not (:rezzed (refresh v2))) "Prevented remote ICE from rezzing")
      (take-credits state :runner)
      (core/rez state :corp (refresh v2))
      (is (:rezzed (refresh v2)) "Rez prevention of ICE ended")
      (take-credits state :corp)
      (play-from-hand state :runner "Careful Planning")
      (click-card state :runner pad)
      (core/rez state :corp pad)
      (is (not (:rezzed (refresh pad))) "Prevented remote server contents from rezzing")
      (take-credits state :runner)
      (core/rez state :corp (refresh pad))
      (is (:rezzed (refresh pad)) "Rez prevention of asset ended"))))

(deftest cbi-raid
  ;; CBI Raid - Full test
  (do-game
    (new-game {:corp {:deck ["Caprice Nisei" "Adonis Campaign" "Quandary"
                             "Jackson Howard" "Global Food Initiative"]}
               :runner {:deck ["CBI Raid"]}})
    (take-credits state :corp)
    (is (= 5 (count (:hand (get-corp)))))
    (play-from-hand state :runner "CBI Raid")
    (is (= :hq (get-in @state [:run :server 0])))
    (run-successful state)
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

(deftest cold-read
  ;; Make a run, and place 4 on this card, which you may use only during this run.
  ;; When this run ends, trash 1 program (cannot be prevented) used during this run.
  (do-game
    (new-game {:corp {:deck [(qty "Blacklist" 3)]}
               :runner {:deck ["Imp" (qty "Cold Read" 2)]}})
    (play-from-hand state :corp "Blacklist" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [bl (get-content state :remote1 0)]
      (play-from-hand state :runner "Cold Read")
      (click-prompt state :runner "HQ")
      (is (= 4 (get-counters (find-card "Cold Read" (get-in @state [:runner :play-area])) :recurring)) "Cold Read has 4 counters")
      (run-successful state)
      (click-prompt state :runner "[Imp]: Trash card")
      (click-card state :runner (get-program state 0))
      (is (= 2 (count (:discard (get-runner)))) "Imp and Cold Read in discard")
      ; Cold Read works when Blacklist rezzed - #2378
      (core/rez state :corp bl)
      (play-from-hand state :runner "Cold Read")
      (click-prompt state :runner "HQ")
      (is (= 4 (get-counters (find-card "Cold Read" (get-in @state [:runner :play-area])) :recurring)) "Cold Read has 4 counters")
      (run-successful state))))

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
        (is (= (inc deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
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
          (is (= (inc deck) (count (:deck (get-runner)))) "Gordian Blade should be back in stack")
          (is (nil? (get-program state 0))))))))

(deftest contaminate
  ;; Contaminate - add 3 virus counters to an installed runner card with no virus counters
  (testing "Basic test"
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
        (click-card state :runner (refresh yus))
        (click-prompt state :runner "Done")
        (is (= 3 (get-counters (refresh cp) :virus)) "Yusuf isn't selectable by Contaminate"))))
  (testing "Hivemind makes virus programs act like they have a virus counter"
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
        (is (zero? (get-counters (refresh aum) :virus)) "Aumakua ends with 0 virus counters (not counting Hivemind)")))))

(deftest corporate-grant
  ;; Corporate "Grant" - First time runner installs a card, the corp loses 1 credit
  (testing "Basic test"
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
  (testing "with Hayley Kaplan. Issue #3162"
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
      (is (= 7 (:credit (get-corp))) "Corp only loses 1 credit"))))

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
    (is (empty? (:prompt (get-runner))) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :runner "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-resource state 1))) "IJ able to be installed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-runner))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :runner "Activist Support")
    (take-credits state :runner)
    (let [em (get-content state :remote1 0)]
      (core/rez state :corp em)
      (is (= 1 (:has-bad-pub (get-corp))) "Corp still has BP")
      (take-credits state :corp)
      (is (zero? (:bad-publicity (get-corp))) "Corp has BP, didn't take 1 from Activist Support"))))

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
        (is (:icon (refresh iw)) "Ice Wall has an icon")))
    (is (= 1 (count-tags state)) "Runner gained a tag")))

(deftest data-breach
  ;; Data Breach
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck [(qty "Data Breach" 3)]}})
      (starting-hand state :corp ["Hedge Fund"])
      (take-credits state :corp)
      (play-from-hand state :runner "Data Breach")
      (core/no-action state :corp nil)
      (run-successful state)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
      (core/no-action state :corp nil)
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "No prompt to run a third time")
      (is (not (:run @state)) "Run is over")
      (play-from-hand state :runner "Data Breach")
      (run-jack-out state)
      (is (empty? (:prompt (get-runner))) "No option to run again on unsuccessful run")))
  (testing "FAQ 4.1 - ensure runner gets choice of activation order"
    (do-game
      (new-game {:runner {:deck ["Doppelgänger" (qty "Data Breach" 3)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Doppelgänger")
      (play-from-hand state :runner "Data Breach")
      (core/no-action state :corp nil)
      (run-successful state)
      ; (click-prompt state :runner "No action")
      (click-prompt state :runner "Doppelgänger")
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "HQ")
      (is (:run @state) "New run started")
      (is (= [:hq] (:server (:run @state))) "Running on HQ via Doppelgänger")
      (core/no-action state :corp nil)
      (run-successful state)
      (click-prompt state :runner "No action")
      (click-prompt state :runner "Yes")
      (is (= [:rd] (get-in @state [:run :server])) "Second Data Breach run on R&D triggered")
      (core/no-action state :corp nil)
      (run-successful state))))

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
    (is (empty? (:prompt (get-runner))) "Recurring a non-virus card stops Déjà Vu prompting further")
    (is (= 2 (count (:hand (get-runner)))) "Two cards in after playing Déjà Vu")
    (play-from-hand state :runner "Déjà Vu")
    (click-prompt state :runner (find-card "Cache" (:discard (get-runner))))
    (is (seq (:prompt (get-runner))) "Recurring a virus card causes Déjà Vu to prompt for second virus to recur")
    (click-prompt state :runner (find-card "Datasucker" (:discard (get-runner))))
    (is (= 3 (count (:hand (get-runner)))) "Three cards in after playing second Déjà Vu")))

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
    (run-successful state)
    (click-prompt state :runner "Unrezzed upgrade in R&D")
    (click-prompt state :runner "[Demolition Run]: Trash card")
    (is (= 3 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
    (click-prompt state :runner "Card from deck")
    (click-prompt state :runner "[Demolition Run]: Trash card")
    (is (zero? (:agenda-point (get-runner))) "Didn't steal False Lead")
    (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
    (is (empty? (:prompt (get-runner))) "Run concluded")))

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
    (is (empty? (:prompt (get-runner))) "Deuces Wild not showing a third choice option")
    (play-from-hand state :runner "Deuces Wild")
    (click-prompt state :runner "Expose 1 ice and make a run")
    (click-card state :runner (get-ice state :remote1 0))
    (click-prompt state :runner "HQ")
    (is (empty? (:prompt (get-runner))) "Deuces prompt not queued")
    (run-continue state)
    (run-successful state)
    (is (= 2 (count (:prompt (get-runner)))) "Deuces prompt not queued")
    (click-prompt state :corp "0 [Credits]")
    (click-prompt state :runner "0 [Credits]")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "TFP stolen")
    (core/gain-tags state :runner 1)
    (is (= 1 (count-tags state)) "Runner has 1 tag")
    (click-prompt state :runner "Remove 1 tag")
    (is (zero? (count-tags state)))))

(deftest dirty-laundry
  ;; Dirty Laundry - Gain 5 credits at the end of the run if it was successful
  (do-game
    (new-game {:runner {:deck [(qty "Dirty Laundry" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Dirty Laundry")
    (click-prompt state :runner "Archives")
    (run-successful state)
    (is (= 8 (:credit (get-runner))) "Gained 5 credits")
    (play-from-hand state :runner "Dirty Laundry")
    (click-prompt state :runner "Archives")
    (run-jack-out state)
    (is (= 6 (:credit (get-runner))) "Run unsuccessful; gained no credits")))

(deftest diversion-of-funds
  ;; Diversion of Funds
  (testing "Use ability"
    (do-game
      (new-game {:runner {:deck [(qty "Diversion of Funds" 3)]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Diversion of Funds, use ability
      (play-from-hand state :runner "Diversion of Funds")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= 9 (:credit (get-runner))) "Runner netted 4 credits")
      (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")
      (is (not (:run @state)) "Run finished")))
  (testing "Access"
    (do-game
      (new-game {:runner {:deck [(qty "Diversion of Funds" 3)]}})
      (take-credits state :corp) ; pass to runner's turn by taking credits
      (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
      ;; play Diversion, do not use ability
      (play-from-hand state :runner "Diversion of Funds")
      (run-successful state)
      (click-prompt state :runner "Access cards")
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "Prompt is closed")
      (is (= 4 (:credit (get-runner))) "Runner is down a credit")
      (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")
      (is (not (:run @state)) "Run finished"))))

(deftest divide-and-conquer
  ;; Divide and Conquer
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Hostile Takeover" (qty "Ice Wall" 100)]}
                 :runner {:deck ["Divide and Conquer"]}})
      (starting-hand state :corp ["Hostile Takeover" "Ice Wall" "Ice Wall"])
      (trash-from-hand state :corp "Ice Wall")
      (trash-from-hand state :corp "Ice Wall")
      (take-credits state :corp)
      (play-from-hand state :runner "Divide and Conquer")
      (run-successful state)
      (click-prompt state :runner "Steal")
      (click-prompt state :runner "No action")
      (is (= 4 (-> (get-runner) :register :last-run core/total-cards-accessed)) "Runner should access 2 cards in Archives, 1 in R&D, and 1 in HQ")))
  (testing "with The Turning Wheel counters"
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
        (core/add-counter state :runner ttw :power 4)
        (play-from-hand state :runner "Divide and Conquer")
        (card-ability state :runner ttw 0)
        (card-ability state :runner ttw 1)
        (run-successful state)
        ;; HQ
        (dotimes [_ 2]
          (click-prompt state :runner "Card from hand")
          (click-prompt state :runner (-> (prompt-map :runner) :choices first)))
        ;; R&D
        (dotimes [_ 2]
          (click-prompt state :runner "Card from deck")
          (click-prompt state :runner "No action"))
        (is (empty? (:prompt (get-runner))) "No prompts after all accesses are complete")
        (is (= 1 (-> (get-runner) :register :last-run (core/access-bonus-count :rd)))
            "The Turning Wheel should provide 1 additional access on R&D")
        (is (= 1 (-> (get-runner) :register :last-run (core/access-bonus-count :hq)))
            "The Turning Wheel should provide 1 additional access on HQ")
        (is (= 6 (-> (get-runner) :register :last-run core/total-cards-accessed))
            "Runner should access 2 cards in Archives, 1 + 1 in R&D, and 1 + 1 in HQ"))))
  (testing "The Turning Wheel gains counters after using D&C. Issue #3810"
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
        (play-from-hand state :runner "Divide and Conquer")
        (run-successful state)
        (click-prompt state :runner "No action")
        (click-prompt state :runner "No action")
        (is (= counters (get-counters (refresh ttw) :power)) "Gains no counters")
        (run-empty-server state "R&D")
        (click-prompt state :runner "No action")
        (is (= (+ 1 counters) (get-counters (refresh ttw) :power)) "Gains 1 counters"))))
  (testing "vs Nisei Mk II. Issue #3803"
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
        (run-phase-43 state)
        (card-ability state :corp (refresh scored-nisei) 0)
        (click-prompt state :corp "Done") ; close 4.3 corp
        (is (nil? (-> @state :runner :prompt first)) "No access prompts for runner")
        (is (not (:run @state)) "Run ended by using Nisei counter")
        (is (zero? (-> (get-runner) :register :last-run core/total-cards-accessed))
            "Runner should access 0 cards"))))
  (testing "interaction with Black Hat. Issue #3798"
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
      (play-from-hand state :runner "Divide and Conquer")
      (run-successful state)
      (dotimes [_ 3]
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner (-> (prompt-map :runner) :choices first)))
      (dotimes [_ 3]
        (click-prompt state :runner "Card from deck")
        (click-prompt state :runner (-> (prompt-map :runner) :choices first)))
      (is (empty? (:prompt (get-runner))) "No prompts after all accesses are complete")
      (is (= 7 (-> (get-runner) :register :last-run core/total-cards-accessed))))))

(deftest drive-by
  ;; Drive By - Expose card in remote server and trash if asset or upgrade
  (testing "Basic test"
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
        (core/rez state :corp eve1)
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
  (testing "Psychic Field trashed after psi game. Issue #2127."
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
  (testing "Turn on reprisal cards. Issue #3755."
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Drive By"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (play-from-hand state :runner "Drive By")
      (click-card state :runner "PAD Campaign")
      (is (empty? (get-content state :remote1)) "PAD Campaign trashed")
      (is (get-in (get-runner) [:register :trashed-card]) "Registered as runner trashed a card"))))

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

(deftest embezzle
  ;; Embezzle
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Archer"]}
                 :runner {:deck ["Embezzle"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "ICE")
      (is (= 2 (count (:discard (get-corp)))) "HQ card trashed")
      (is (= 12 (:credit (get-runner))))))
  (testing "Check that trashed cards are trashed face-up"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Embezzle"]}})
      (take-credits state :corp)
      (is (= 5 (:credit (get-runner))))
      (play-run-event state (first (:hand (get-runner))) :hq)
      (click-prompt state :runner "ICE")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed")
      (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen")
      (is (= 8 (:credit (get-runner)))))))

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

(deftest employee-strike
  ;; Employee Strike
  (testing "vs Blue Sun, suppress Step 1.2"
    (do-game
      (new-game {:corp {:id "Blue Sun: Powering the Future"
                        :deck ["Ice Wall"]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Employee Strike")
      (take-credits state :runner)
      (is (not (:corp-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))
  (testing "vs PU/Philotic - test for #2688"
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
      (score-agenda state :corp (get-content state :remote3 0))
      (is (= 3 (count (:discard (get-runner))))
          "Discard is 3 cards - 2 from Philotic, 1 EStrike.  Nothing from PU mill"))))

(deftest encore
  ;; Encore - Run all 3 central servers successfully to take another turn.  Remove Encore from game.
  (testing "Basic test"
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
  (testing "2 encores in a 5 click turn results in 2 extra turns"
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
      (is (= 13 (:credit (get-runner)))))))

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

(deftest exploratory-romp
  ;; Exploratory Romp - Remove advancements from card instead of accessing
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["TGTBT"]}
                 :runner {:deck ["Exploratory Romp"]}})
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [tg (get-content state :remote1 0)]
        (advance state tg 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Exploratory Romp")
        (click-prompt state :runner "Server 1")
        (run-successful state)
        (click-prompt state :runner "Replacement effect")
        (click-prompt state :runner "2")
        (click-card state :runner (refresh tg))
        (is (zero? (count-tags state)) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed"))))
  (testing "Don't remove more than the existing number of advancement tokens"
    (do-game
      (new-game {:corp {:deck ["TGTBT"]}
                 :runner {:deck ["Exploratory Romp"]}})
      (play-from-hand state :corp "TGTBT" "New remote")
      (let [tg (get-content state :remote1 0)]
        (advance state tg 2)
        (take-credits state :corp)
        (play-from-hand state :runner "Exploratory Romp")
        (click-prompt state :runner "Server 1")
        (run-successful state)
        (click-prompt state :runner "Replacement effect")
        (click-prompt state :runner "3")
        (click-card state :runner (refresh tg))
        (is (zero? (count-tags state)) "No tags, didn't access TGTBT")
        (is (zero? (get-counters (refresh tg) :advancement)) "Advancements removed")))))

(deftest falsified-credentials
  ;; Falsified Credentials - Expose card in remote
  ;; server and correctly guess its type to gain 5 creds
  (testing "Basic test"
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
        (core/rez state :corp eve1)
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
        (core/rez state :corp pp2)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (is (= 17 (:credit (get-runner)))
            "Gained 5 credits for guessing agenda correctly, even with rezzed card in server"))))
  (testing "vs Zaibatsu Loyalty. If Falsified Credentials fails to expose, it grants no credits."
    (do-game
      (new-game {:corp {:deck ["Zaibatsu Loyalty" "Project Atlas"]}
                 :runner {:deck [(qty "Falsified Credentials" 2)]}})
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Zaibatsu Loyalty" "New remote")
      (take-credits state :corp)
      (let [atl (get-content state :remote1 0)
            zaibatsu (get-content state :remote2 0)]
        (core/rez state :corp zaibatsu)
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (click-prompt state :corp "Done")
        (is (= 9 (:credit (get-runner))) "An unprevented expose gets credits")
        (play-from-hand state :runner "Falsified Credentials")
        (click-prompt state :runner "Agenda")
        (click-card state :runner atl)
        (card-ability state :corp (refresh zaibatsu) 0) ; prevent the expose!
        (click-prompt state :corp "Done")
        (is (= 8 (:credit (get-runner))) "A prevented expose does not")))))

(deftest feint
  ;; Feint - bypass 2 pieces of ice on HQ, but access no cards
  (do-game
    (new-game {:runner {:deck ["Feint"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Feint")
    (run-successful state)
    (click-prompt state :runner "OK")
    (is (not (:run @state)) "Run is over")))

(deftest frantic-coding
  ;; Frantic Coding - Install 1 program, other 9 cards are trashed
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]}})
      (starting-hand state :runner ["Frantic Coding"])
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (let [get-prompt (fn [] (first (#(get-in @state [:runner :prompt]))))
            prompt-names (fn [] (map :title (:choices (get-prompt))))]
        (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
        (is (= 2 (:credit (get-runner))))
        (is (= 1 (count (:discard (get-runner)))))
        (click-prompt state :runner (find-card "Magnum Opus" (:deck (get-runner))))
        (is (= 1 (count (get-program state))))
        (is (= 2 (:credit (get-runner))) "Magnum Opus installed for free")
        (is (= 10 (count (:discard (get-runner))))))))
  (testing "Don't install anything, all 10 cards are trashed"
    (do-game
      (new-game {:runner {:deck ["Frantic Coding" "Torch" "Corroder"
                                 "Magnum Opus" (qty "Daily Casts" 2) (qty "Sure Gamble" 2)
                                 "John Masanori" "Amped Up" "Wanton Destruction"]}})
      (starting-hand state :runner ["Frantic Coding"])
      (take-credits state :corp)
      (play-from-hand state :runner "Frantic Coding")
      (click-prompt state :runner "OK")
      (let [get-prompt (fn [] (first (#(get-in @state [:runner :prompt]))))
            prompt-names (fn [] (map :title (:choices (get-prompt))))]
        (is (= (list "Corroder" "Magnum Opus" nil) (prompt-names)) "No Torch in list because can't afford")
        (is (= 1 (count (:discard (get-runner)))))
        (click-prompt state :runner "No install")
        (is (zero? (count (get-program state))))
        (is (= 11 (count (:discard (get-runner)))))))))

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
    (run-empty-server state "Server 2")
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "\"Freedom Through Equality\"")
    (play-from-hand state :runner "\"Freedom Through Equality\"")
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
    (is (= 7 (core/hand-size state :runner)) "Runner hand size is 7")
    (play-from-hand state :runner "Game Day")
    (is (= 7 (count (:hand (get-runner)))) "Drew up to 7 cards")))

(deftest glut-cipher
  (do-game
    (new-game {:corp {:deck [(qty "Ice Wall" 3) (qty "Wraparound" 2) "Hedge Fund"]}
               :runner {:deck [(qty "Glut Cipher" 3)]}})
    (trash-from-hand state :corp "Ice Wall")
    (take-credits state :corp)
    (trash-from-hand state :corp "Ice Wall")
    (trash-from-hand state :corp "Hedge Fund")
    (is (= 3 (count (:discard (get-corp)))) "There are 3 cards in Archives")
    (play-from-hand state :runner "Glut Cipher")
    (is (= 3 (count (:discard (get-corp)))) "Glut Cipher did not fire when < 5 cards")
    (is (zero? (count (filter :seen (:discard (get-corp))))) "There are no faceup cards in Archives")
    (run-on state :archives)
    (run-successful state)
    (is (= 3 (count (filter :seen (:discard (get-corp))))) "There are 3 faceup cards in Archives")
    (trash-from-hand state :corp "Wraparound")
    (trash-from-hand state :corp "Wraparound")
    (trash-from-hand state :corp "Ice Wall")
    (is (= 3 (count (filter :seen (:discard (get-corp))))) "There are 3 faceup cards in Archives")
    (is (= 6 (count (:discard (get-corp)))) "There are 6 cards in Archives")
    (play-run-event state "Glut Cipher" :archives)
    (click-card state :corp (get-discarded state :corp 0))
    (click-card state :corp (get-discarded state :corp 1))
    (click-card state :corp (get-discarded state :corp 3))
    (is (:prompt (get-corp)) "There is still a prompt")
    (click-card state :corp (get-discarded state :corp 4))
    (click-card state :corp (get-discarded state :corp 5))
    (is (nil? (-> (get-corp) :prompt first)) "Selecting 5 cards closed prompt")
    (let [discard (:discard (get-corp))]
      (is (find-card "Hedge Fund" discard) "Hedge Fund is still in Archives")
      (is (= 6 (count discard)) "There are 6 cards in Archives")
      (is (= 1 (count (filter :seen discard))) "There is 1 seen card in Archives"))
    (is (zero? (count (:hand (get-corp)))) "There are no cards in hand")))

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
                             (qty "Jackson Howard" 2)]}
               :runner {:deck ["Street Peddler"
                               (qty "Hacktivist Meeting" 3)]}})
    (take-credits state :corp)
    (starting-hand state :runner ["Street Peddler" "Hacktivist Meeting"])
    (play-from-hand state :runner "Street Peddler")
    (take-credits state :runner)
    (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
    (play-from-hand state :corp "Jackson Howard" "New remote")
    (let [jeeves (get-content state :remote1 0)
          jackson (get-content state :remote2 0)]
      (core/rez state :corp jeeves)
      (is (zero? (count (:discard (get-corp)))) "Nothing discarded to rez Jeeves - Hacktivist not active")
      (take-credits state :corp)
      (play-from-hand state :runner "Hacktivist Meeting")
      (core/rez state :corp jackson)
      (is (= 1 (count (:discard (get-corp)))) "Card discarded to rez Jackson - Hacktivist active"))))

(deftest high-stakes-job
  ;; High Stakes Job - run on server with at least 1 piece of unrezzed ice, gains 12 credits if successful
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["High-Stakes Job"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 1)
    (is (= 6 (:credit (get-runner))) "Runner starts with 6 credits")
    (play-from-hand state :runner "High-Stakes Job")
    (click-prompt state :runner "HQ")
    (run-successful state)
    (is (= 12 (:credit (get-runner))) "Runner gains 12 credits")))

(deftest hot-pursuit
  ;; Hot Pursuit
  (do-game
    (new-game {:runner {:deck ["Hot Pursuit"]}})
    (take-credits state :corp)
    (play-run-event state (first (:hand (get-runner))) :hq)
    (is (= (+ 5 -2 9) (:credit (get-runner))) "Gained 9 credits on successful run")
    (is (= 1 (count-tags state)) "Took 1 tag on successful run")
    (is (prompt-map :runner) "Still have access prompt")))

(deftest isolation
  ;; Isolation - A resource must be trashed, gain 7c
  (do-game
    (new-game {:runner {:deck ["Kati Jones" "Isolation"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Isolation")
    (is (= 2 (count (get-in @state [:runner :hand]))) "Isolation could not be played because no resource is installed")
    (is (zero? (count (get-in (get-runner) [:rig :resource]))) "Kati Jones is not installed")
    (play-from-hand state :runner "Kati Jones")
    (is (= 1 (count (get-resource state))) "Kati Jones was installed")
    (let [kj (get-resource state 0)]
      (play-from-hand state :runner "Isolation")
        (click-card state :runner kj)
        (is (zero? (count (get-in (get-runner) [:rig :resource]))) "Kati Jones was trashed")
        (is (= 8 (:credit (get-runner))) "Gained 7 credits")
        (is (= 2 (count (:discard (get-runner)))) "Kati Jones and Isolation are in the discard"))))

(deftest i-ve-had-worse
  ;; I've Had Worse - Draw 3 cards when lost to net/meat damage; don't trigger if flatlined
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Scorched Earth" 3) (qty "Pup" 3)]}
                 :runner {:deck [(qty "I've Had Worse" 2) (qty "Sure Gamble" 3) (qty "Imp" 2)]}})
      (core/gain state :runner :tag 1)
      (core/gain state :corp :credit 5)
      (starting-hand state :runner ["I've Had Worse"])
      (play-from-hand state :corp "Pup" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (card-subroutine state :corp (get-ice state :hq 0) 0)
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 3 (count (:hand (get-runner)))) "I've Had Worse triggered and drew 3 cards")
      (starting-hand state :runner ["I've Had Worse" "Imp" "Imp"])
      (play-from-hand state :corp "Scorched Earth")
      (is (zero? (count (:hand (get-runner)))) "Runner has 0 cards in hand")
      (is (= :corp (:winner @state)) "Corp wins")
      (is (= "Flatline" (:reason @state)) "Win condition reports flatline")
      (is (= 4 (count (:discard (get-runner)))) "All 3 cards in Grip trashed by Scorched Earth")
      (is (= 3 (count (:deck (get-runner)))) "No cards drawn from I've Had Worse")))
  (testing "Will save you if you apocalypse away a lot of cards vs Hostile Infrastructure"
    (do-game
      (new-game {:corp {:deck ["Hostile Infrastructure" (qty "Ice Wall" 2)]}
                 :runner {:deck [(qty "I've Had Worse" 3) (qty "Sure Gamble" 3) (qty "Apocalypse" 2)]}})
      (starting-hand state :runner ["I've Had Worse" "Apocalypse"])
      (starting-hand state :corp ["Hostile Infrastructure" "Ice Wall" "Ice Wall"])
      (play-from-hand state :corp "Hostile Infrastructure" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (play-from-hand state :corp "Ice Wall" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (run-empty-server state "HQ")
      (run-empty-server state "Archives")
      (run-empty-server state "R&D")
      (play-from-hand state :runner "Apocalypse")
      (is (not= "Flatline" (:reason @state)) "Win condition does not report flatline"))))

(deftest independent-thinking
  ;; Independent Thinking - Trash 2 installed cards, including a facedown directive, and draw 2 cards
  (do-game
    (new-game {:runner {:id "Apex: Invasive Predator"
                        :deck ["Neutralize All Threats" (qty "Independent Thinking" 2)
                               (qty "Fan Site" 3) (qty "Street Magic" 3)]}})
    (starting-hand state :runner ["Fan Site" "Fan Site" "Neutralize All Threats"
                                  "Independent Thinking" "Independent Thinking"])
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
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
    (play-from-hand state :runner "Indexing")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (click-prompt state :runner "Replacement effect")
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

(deftest information-sifting
  ;; Information Sifting - complicated interactions with damage prevention
  (do-game
    (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                      :deck ["Snare!" "PAD Campaign" "Hostile Infrastructure"
                             "Braintrust" "Hedge Fund" "Power Shutdown"]}
               :runner {:deck [(qty "Information Sifting" 2) (qty "Deus X" 2) "Sure Gamble"]}})
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (core/gain state :corp :credit 10)
    (core/rez state :corp (get-content state :remote1 0))
    (core/gain state :runner :credit 10)
    (take-credits state :corp)
    (play-from-hand state :runner "Deus X")
    (play-from-hand state :runner "Deus X")
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (click-card state :corp (find-card "Snare!" (:hand (get-corp))))
    (click-card state :corp (find-card "PAD Campaign" (:hand (get-corp))))
    (click-prompt state :corp "Done")
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Corp is waiting for Runner selection")
    (click-prompt state :runner "Pile 1 (2 cards)")
    (click-prompt state :runner "Card from pile 1")
    ;; the cards are selected randomly :(
    (letfn [(prevent-snare [existing-dmg]
              (click-prompt state :corp "Yes")
              (card-ability state :runner (get-program state 0) 1)
              (click-prompt state :runner "Done")
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Damage from Snare! prevented")
              (click-prompt state :runner (-> (prompt-map :runner) :choices first))
              (when (-> (prompt-map :runner) :choices first)
                (click-prompt state :runner "Done")) ; don't prevent Hostile dmg
              ;; chronos prompt
              (click-prompt state :corp "Yes")
              (click-prompt state :corp (find-card "Sure Gamble" (:hand (get-runner))))
              (is (= (+ 2 existing-dmg) (count (:discard (get-runner)))) "Damage from Hostile Inf not prevented"))
            (allow-pad [existing-dmg]
              (click-prompt state :runner (-> (prompt-map :runner) :choices first))
              (card-ability state :runner (get-program state 0) 1)
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Runner prevented damage from Hostile Inf")
              (click-prompt state :runner "Done"))]
      (if (= :waiting (-> (get-runner) :prompt first :prompt-type)) ; hit the snare
        ;; prevent the damage
        (do (prevent-snare (count (:discard (get-runner))))
            (click-prompt state :runner "Card from pile 1")
            (allow-pad (count (:discard (get-runner)))))
        (do (allow-pad (count (:discard (get-runner))))
            (click-prompt state :runner "Card from pile 1")
            (prevent-snare (count (:discard (get-runner)))))))
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (click-card state :corp (find-card "Power Shutdown" (:hand (get-corp))))
    (click-card state :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Selecting max cards closed the selection prompt")
    (click-prompt state :runner "Pile 2 (1 card)")
    (click-prompt state :runner "Card from pile 2")
    (click-prompt state :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Runner stole agenda")))

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
    (is (= 2 (:current-strength (get-program state 0))) "Corroder at 2 strength")
    (click-card state :runner (get-program state 0))
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (is (= 4 (:current-strength (get-program state 0))) "Corroder at 4 strength")
    (run-continue state)
    (run-successful state)
    (is (= 2 (:current-strength (get-program state 0))) "Corroder reset to 2 strength")))

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
    (is (= :waiting (-> (get-runner) :prompt first :prompt-type)) "Runner is waiting for Corp to reorder")
    (click-prompt state :corp (find-card "Director Haas" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Elizabeth Mills" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Jackson Howard" (:deck (get-corp))))
    (click-prompt state :corp (find-card "Caprice Nisei" (:deck (get-corp))))
    (click-prompt state :corp "Done")
    (is (not= :waiting (-> (get-runner) :prompt first :prompt-type)) "Waiting prompt done")
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
      (core/rez state :corp jeeves)
      (is (:rezzed (refresh jeeves)) "Jeeves is rezzed.  Interdiction not active when on Peddler")
      (play-from-hand state :runner "Interdiction")
      (core/rez state :corp jackson)
      (is (not (:rezzed (refresh jackson))) "Jackson is not rezzed"))))

(deftest knifed
  ;; Knifed - Make a run, trash a barrier if all subs broken
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Knifed"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Knifed")
    (click-prompt state :runner "HQ")
    (run-successful state)))

(deftest labor-rights
  ;; Labor Rights - trash 3 cards, shuffle 3 cards from heap->stack, draw 1 card, rfg Labor Rights
  (testing "Basic behavior"
    (do-game
      (new-game {:runner {:hand ["Labor Rights"] :deck ["Sure Gamble" "Lawyer Up" "Knifed"]}})
      (take-credits state :corp)
      (is (empty? (:discard (get-runner))) "Starts with no cards in discard")
      (is (= 3 (count (:deck (get-runner)))) "Starts with 3 cards in deck")
      (play-from-hand state :runner "Labor Rights")
      (is (empty? (:deck (get-runner))) "Milled 3 cards")
      (is (= 4 (count (:discard (get-runner)))) "4 cards in deck - 3x trashed, 1x Labor Rights")
      (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
      (click-card state :runner (find-card "Knifed" (:discard (get-runner))))
      (click-card state :runner (find-card "Lawyer Up" (:discard (get-runner))))
      (is (= 2 (count (:deck (get-runner)))) "2 cards in deck")
      (is (= 1 (count (:hand (get-runner)))) "1 card in hand")
      (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg")))
  (testing "Less than 3 cards"
    (do-game
      (new-game {:runner {:hand ["Labor Rights"] :deck ["Sure Gamble"]}})
      (take-credits state :corp)
      (is (empty? (:discard (get-runner))) "Starts with no cards in discard")
      (is (= 1 (count (:deck (get-runner)))) "Starts with 1 card in deck")
      (play-from-hand state :runner "Labor Rights")
      (is (empty? (:deck (get-runner))) "Milled 1 cards")
      (is (= 2 (count (:discard (get-runner)))) "2 cards in deck - 1x trashed, 1x Labor Rights")
      (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
      (is (empty? (:deck (get-runner))) "No cards in deck")
      (is (= 1 (count (:hand (get-runner)))) "1 card in hand")
      (is (= 1 (count (:rfg (get-runner)))) "1 card in rfg"))))

(deftest lawyer-up
  ;; Lawyer Up - Lose 2 tags and draw 3 cards
  (do-game
    (new-game {:runner {:deck ["Lawyer Up" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/gain-tags state :runner 3)
    (play-from-hand state :runner "Lawyer Up")
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (= 2 (:click (get-runner))) "Spent 2 clicks")
    (is (= 1 (count-tags state)) "Lost 2 tags")))

(deftest leave-no-trace
  ;; Leave No Trace should derez ICE that was rezzed during the run
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Leave No Trace"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Leave No Trace")
      (click-prompt state :runner "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (run-successful state)
      (is (not (:rezzed (get-ice state :hq 0))) "Inner Ice Wall should not be rezzed")
      (is (:rezzed (get-ice state :hq 1)) "Outer Ice Wall should be rezzed still")))
  (testing "should not derez ICE that has changed during a run"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Leave No Trace"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (is (:rezzed (get-ice state :hq 0)) "Ice Wall should be rezzed initially")
      (play-from-hand state :runner "Leave No Trace")
      (click-prompt state :runner "Archives")
      (core/add-prop state :corp (get-ice state :hq 0) :advance-counter 1)
      (run-successful state)
      (is (= 1 (get-counters (get-ice state :hq 0) :advancement)))
      (is (:rezzed (get-ice state :hq 0)) "Ice Wall should still be rezzed")))
  (testing "Should trigger :derez events for Runner, not Corp (#3919)"
    (do-game
      (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                 :runner {:deck ["Leave No Trace" "Keros Mcintyre"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 1))
      (take-credits state :corp)
      (play-from-hand state :runner "Keros Mcintyre")
      (play-from-hand state :runner "Leave No Trace")
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "HQ")
        (core/rez state :corp (get-ice state :hq 0))
        (run-successful state)
        (is (= (+ credits 2) (:credit (get-runner))) "Keros should trigger off derez")
        (is (not (:rezzed (get-ice state :hq 0))) "Inner Ice Wall should not be rezzed")
        (is (:rezzed (get-ice state :hq 1)) "Outer Ice Wall should be rezzed still")))))

(deftest mad-dash
  ;; Mad Dash - Make a run. Move to score pile as 1 point if steal agenda.  Take 1 meat if not
  (do-game
    (new-game {:corp {:deck ["Project Atlas"]}
               :runner {:deck [(qty "Mad Dash" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Mad Dash")
    (click-prompt state :runner "Archives")
    (run-successful state)
    (is (= 2 (count (:discard (get-runner)))) "Took a meat damage")
    (play-from-hand state :runner "Mad Dash")
    (click-prompt state :runner "HQ")
    (run-successful state)
    (click-prompt state :runner "Steal")
    (is (= 2 (count (:scored (get-runner)))) "Mad Dash moved to score area")
    (is (= 3 (:agenda-point (get-runner))) "Mad Dash scored for 1 agenda point")))

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
    (is (= 1 (count (:discard (get-runner)))))
    (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
    (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
    (is (= 3 (count (:discard (get-runner)))))
    (click-prompt state :runner "None")
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
    (core/draw state :runner)
    (is (= "Making an Entrance" (:title (first (:hand (get-runner))))))
    (is (= 1 (count (:hand (get-runner)))))
    (play-from-hand state :runner "Making an Entrance")
    (is (= 1 (count (:hand (get-runner)))) "Can only play on first click")))

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
    (core/gain-tags state :runner 4)
    (is (= 5 (count-tags state)) "+1 tag from Jarogniew Mercs")
    (is (= 1 (count (:hand (get-runner)))))
    (is (= 2 (:credit (get-runner))))
    (play-from-hand state :runner "Mars for Martians")
    (is (= 3 (count (:hand (get-runner)))) "3 clan resources, +3 cards but -1 for playing Mars for Martians")
    (is (= 7 (:credit (get-runner))) "5 tags, +5 credits")))

(deftest mobius
  ;; Mobius
  (do-game
    (new-game {:runner {:deck [(qty "Möbius" 3)]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Möbius")
    (core/no-action state :corp nil)
    (run-successful state)
    (is (= 5 (:credit (get-runner))))
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Yes")
    (is (= [:rd] (get-in @state [:run :server])) "Second run on R&D triggered")
    (core/no-action state :corp nil)
    (run-successful state)
    (click-prompt state :runner "No action")
    (is (= 9 (:credit (get-runner))))
    (is (empty? (:prompt (get-runner))) "No prompt to run a third time")
    (is (not (:run @state)) "Run is over")
    (play-from-hand state :runner "Möbius")
    (run-jack-out state)
    (is (empty? (:prompt (get-runner))) "No option to run again on unsuccessful run")))

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

(deftest on-the-lam
  ;; On the Lam
  (testing "vs tags"
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
      (card-ability state :runner (-> (get-resource state 0) :hosted first) 0)
      (click-prompt state :runner "Done")
      (is (zero? (count-tags state)) "Runner should avoid tag")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap")))
  (testing "vs damage"
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
      (card-ability state :runner (-> (get-resource state 0) :hosted first) 1)
      (click-prompt state :runner "Done")
      (is (zero? (count-tags state)) "Runner should avoid all meat damage")
      (is (= 1 (-> (get-runner) :discard count)) "Runner should have 1 card in Heap"))))

(deftest out-of-the-ashes
  ;; Out of the Ashes - ensure card works when played/trashed/milled
  (do-game
    (new-game {:corp {:deck ["Kala Ghoda Real TV" "Underway Renovation"]}
               :runner {:deck [(qty "Out of the Ashes" 6)]}})
    (play-from-hand state :corp "Underway Renovation" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Out of the Ashes")
    (click-prompt state :runner "Archives")
    (is (:run @state))
    (run-successful state)
    (trash-from-hand state :runner "Out of the Ashes")
    (trash-from-hand state :runner "Out of the Ashes")
    (trash-from-hand state :runner "Out of the Ashes")
    (trash-from-hand state :runner "Out of the Ashes")
    (is (zero? (count (:hand (get-runner)))))
    (is (= 5 (count (:discard (get-runner)))))
    (take-credits state :runner)
    (let [underway (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh underway)}))
    (is (= 6 (count (:discard (get-runner)))))
    (take-credits state :corp)
    ;; remove 5 Out of the Ashes from the game
    (dotimes [_ 5]
      (is (seq (get-in @state [:runner :prompt])))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner "Archives")
      (is (:run @state))
      (run-successful state))
    (click-prompt state :runner "No")
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 5 (count (:rfg (get-runner)))))
    (take-credits state :runner)
    (take-credits state :corp)
    ;; ensure that if you decline the rfg, game will still ask the next turn
    (is (seq (get-in @state [:runner :prompt])))
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Archives")
    (is (:run @state))
    (run-successful state)
    (is (zero? (count (:discard (get-runner)))))
    (is (= 6 (count (:rfg (get-runner)))))))

(deftest peace-in-our-time
  ;; Peace in Our Time - runner gains 10, corp gains 5. No runs allowed during turn.
  (do-game
    (new-game {:runner {:deck ["Peace in Our Time"]}})
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp starts with 8 credits")
    (is (= 5 (:credit (get-runner))) "Runner starts with 5 credits")
    (play-from-hand state :runner "Peace in Our Time")
    (is (= 13 (:credit (get-corp))) "Corp gains 5 credits")
    (is (= 14 (:credit (get-runner))) "Runner gains 10 credits")
    (run-on state "HQ")
    (is (not (:run @state)) "Not allowed to make a run")))

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
  (testing "Forfeiting agenda with Political Graffiti does not refund double points. Issue #2765"
    (do-game
      (new-game {:corp {:deck ["Project Kusanagi" "Corporate Town"]}
                 :runner {:deck ["Political Graffiti"]}})
      (play-from-hand state :corp "Corporate Town" "New remote")
      (play-and-score state "Project Kusanagi")
      (is (zero? (:agenda-point (get-corp))))
      (take-credits state :corp)
      (play-from-hand state :runner "Political Graffiti")
      (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (let [project-kusanagi (get-scored state :corp 0)
            corporate-town (get-content state :remote1 0)]
        (click-card state :runner project-kusanagi)
        (is (= -1 (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
        (take-credits state :runner)
        (core/rez state :corp corporate-town)
        (click-card state :corp (refresh project-kusanagi)))
      (is (zero? (:agenda-point (get-corp))) "Forfeiting agenda did not refund extra agenda points")
      (is (= 1 (count (:discard (get-runner)))) "Political Graffiti is in the Heap"))))

(deftest power-to-the-people
  ;; Power to the People - Gain 7c the first time you access an agenda
  (do-game
    (new-game {:corp {:deck ["NAPD Contract" "Hostile Takeover"]}
               :runner {:deck ["Power to the People"]}})
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (take-credits state :corp)
    (core/lose state :runner :credit 2)
    (play-from-hand state :runner "Power to the People")
    (is (= 3 (:credit (get-runner))) "Can't afford to steal NAPD")
    (run-empty-server state "Server 1")
    (is (= 10 (:credit (get-runner))) "Gained 7c on access, can steal NAPD")
    (click-prompt state :runner "Pay 4 [Credits] to steal")
    (is (= 2 (:agenda-point (get-runner))) "Stole agenda")
    (is (= 6 (:credit (get-runner))))
    (run-empty-server state "HQ")
    (click-prompt state :runner "Steal")
    (is (= 6 (:credit (get-runner))) "No credits gained from 2nd agenda access")))

(deftest push-your-luck
  ;; Push Your Luck
  (testing "Corp guesses correctly"
    (do-game
      (new-game {:runner {:deck ["Push Your Luck"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Push Your Luck")
      (click-prompt state :corp "Odd")
      (click-prompt state :runner "3")
      (is (zero? (:credit (get-runner))) "Corp guessed correctly")))
  (testing "Corp guesses incorrectly"
    (do-game
      (new-game {:runner {:deck ["Push Your Luck"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Push Your Luck")
      (click-prompt state :corp "Even")
      (click-prompt state :runner "3")
      (is (= 6 (:credit (get-runner))) "Corp guessed incorrectly"))))

(deftest pushing-the-envelope
  ;; Run. Add 2 strength to each installer breaker.
  (do-game
    (new-game {:runner {:deck [(qty "Pushing the Envelope" 3) (qty "Corroder" 2) "Atman"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 20)
    (core/gain state :runner :click 10)
    (core/draw state :runner)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Atman")
    (click-prompt state :runner "0")
    (let [atman (get-program state 1)
          corr (get-program state 0)]
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :runner "Pushing the Envelope")
      (click-prompt state :runner "Archives")
      ; 3 cards in hand - no boost
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (play-from-hand state :runner "Pushing the Envelope")
      (click-prompt state :runner "Archives")
      (run-continue state)
      ; 2 cards in hand - boost
      (is (= 2 (:current-strength (refresh atman))) "Atman 2 current strength")
      (is (= 4 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength"))))

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

;; rebirth
(let [akiko "Akiko Nisei: Head Case"
      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"]
  (deftest rebirth
    ;; Rebirth - Kate's discount applies after rebirth
    (testing "Kate"
      (do-game
        (new-game {:runner {:deck ["Magnum Opus" "Rebirth"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "Rebirth")
        (is (= (first (prompt-titles :runner)) akiko) "List is sorted")
        (is (every? #(some #{%} (prompt-titles :runner))
                    [kate kit]))
        (is (not-any? #(some #{%} (prompt-titles :runner))
                      [professor whizzard jamie]))
        (click-prompt state :runner kate)
        (is (= kate (-> (get-runner) :identity :title)))
        (is (= 1 (:link (get-runner))) "1 link")
        (is (empty? (:discard (get-runner))))
        (is (= "Rebirth" (-> (get-runner) :rfg first :title)))
        (is (changes-credits (get-runner) -4
                             (play-from-hand state :runner "Magnum Opus")))))
    (testing "Whizzard works after rebirth"
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
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-ice state :rd 0)))
            "Reina is no longer active")))
    (testing "Lose link from ID"
      (do-game
        (new-game {:runner {:id kate
                            :deck ["Rebirth" "Access to Globalsec"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "Access to Globalsec")
        (is (= 2 (:link (get-runner))) "2 link before rebirth")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner chaos)
        (is (= 1 (:link (get-runner))) "1 link after rebirth")))
    (testing "Gain link from ID"
      (do-game
        (new-game {:runner {:deck ["Rebirth" "Access to Globalsec"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "Access to Globalsec")
        (is (= 1 (:link (get-runner))) "1 link before rebirth")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner kate)
        (is (= 2 (:link (get-runner))) "2 link after rebirth")))
    (testing "Implementation notes are kept, regression test for #3722"
      (do-game
        (new-game {:runner {:deck ["Rebirth"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner chaos)
        (is (= :full (get-in (get-runner) [:identity :implementation])) "Implementation note kept as `:full`"))))
  (testing "Rebirth into Kate twice"
    ;; Rebirth - Kate does not give discount after rebirth if Hardware or Program already installed
    (testing "Installing Hardware before does prevent discount"
      (do-game
        (new-game {:runner {:deck ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "Clone Chip")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner kate)
        (is (= kate (get-in (get-runner) [:identity :title])) "Rebirthed into Kate")
        (is (changes-credits (get-runner) -1
                             (play-from-hand state :runner "Akamatsu Mem Chip"))
            "Discount not applied for 2nd install")))
    (testing "Installing Resource before does not prevent discount"
      (do-game
        (new-game {:runner {:deck ["Akamatsu Mem Chip" "Rebirth" "Same Old Thing"]}
                   :options {:start-as :runner}})
        (play-from-hand state :runner "Same Old Thing")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner kate)
        (is (= kate (get-in (get-runner) [:identity :title])) "Rebirthed into Kate")
        (is (changes-credits (get-runner) 0
                             (play-from-hand state :runner "Akamatsu Mem Chip"))
            "Discount is applied for 2nd install (since it is the first Hardware / Program)"))))
  (testing "Rebirth into Reina twice"
    ;; Rebirth - Reina does not increase rez cost after rebirth if Ice already rezzed
    (testing "Rezzing Ice before does prevent cost"
      (do-game
        (new-game {:corp {:deck [(qty "Ice Wall" 2)]}
                   :runner {:id whizzard
                            :deck ["Rebirth"]}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Ice Wall" "R&D")
        (take-credits state :corp)
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-ice state :hq 0)))
            "Only pay 1 to rez ice wall when against Whizzard")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner reina)
        (is (= reina (get-in (get-runner) [:identity :title])) "Rebirthed into Reina")
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-ice state :rd 0)))
            "Additional cost from Reina not applied for 2nd ice rez")))
    (testing "Rezzing Asset before does not prevent additional cost"
      (do-game
        (new-game {:corp {:deck ["Ice Wall" "Mark Yale"]}
                   :runner {:id whizzard
                            :deck ["Rebirth"]}})
        (play-from-hand state :corp "Ice Wall" "HQ")
        (play-from-hand state :corp "Mark Yale" "New remote")
        (take-credits state :corp)
        (is (changes-credits (get-corp) -1
                             (core/rez state :corp (get-content state :remote1 0)))
            "Only pay 1 to rez Mark Yale")
        (play-from-hand state :runner "Rebirth")
        (click-prompt state :runner reina)
        (is (= reina (get-in (get-runner) [:identity :title])) "Rebirthed into Reina")
        (is (changes-credits (get-corp) -2
                             (core/rez state :corp (get-ice state :hq 0)))
            "Additional cost from Reina applied for 1st ice rez")))))

(deftest reboot
  ;; Reboot - run on Archives, install 5 cards from head facedown
  (do-game
    (new-game {:runner {:deck ["Reboot" "Sure Gamble" "Paperclip" "Clot"]}})
    (take-credits state :corp)
    (trash-from-hand state :runner "Sure Gamble")
    (trash-from-hand state :runner "Paperclip")
    (trash-from-hand state :runner "Clot")
    (is (empty? (core/all-installed state :runner)) "Runner starts with no installed cards")
    (is (= 3 (count (:discard (get-runner)))) "Runner starts with 3 cards in trash")
    (is (empty? (:rfg (get-runner))) "Runner starts with no discarded cards")
    (play-from-hand state :runner "Reboot")
    (run-successful state)
    (click-card state :runner (find-card "Sure Gamble" (:discard (get-runner))))
    (click-card state :runner (find-card "Paperclip" (:discard (get-runner))))
    (click-card state :runner (find-card "Clot" (:discard (get-runner))))
    (click-prompt state :runner "Done")
    (is (= 3 (count (filter :facedown (core/all-installed state :runner)))) "Runner has 3 facedown cards")
    (is (= 3 (count (core/all-installed state :runner))) "Runner has no other cards installed")
    (is (empty? (:discard (get-runner))) "Runner has empty trash")
    (is (= 1 (count (:rfg (get-runner)))) "Runner has 1 card in RFG")))

(deftest rejig
  ;; Rejig
  (do-game
    (new-game {:options {:start-as :runner}
               :runner {:credits 10
                        :hand ["Rejig" "Corroder" "Adept"]}})
    (play-from-hand state :runner "Adept")
    (play-from-hand state :runner "Rejig")
    (click-card state :runner "Corroder")
    (is (find-card "Corroder" (:hand (get-runner))) "Corroder shouldn't be installed")
    (click-card state :runner "Adept")
    (is (= 2 (count (:hand (get-runner)))))
    (let [credits (:credit (get-runner))
          cost (:cost (find-card "Adept" (:hand (get-runner))))]
      (click-card state :runner "Adept")
      (is (= (- credits (- cost (quot cost 2))) (:credit (get-runner))) "Runner should only pay half for Adept"))))

(deftest reshape
  ;; Reshape - Swap 2 pieces of unrezzed ICE
  (do-game
    (new-game {:corp {:deck [(qty "Vanilla" 2) "Paper Wall"]}
               :runner {:deck ["Reshape"]}})
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Vanilla" "HQ")
    (play-from-hand state :corp "Vanilla" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Reshape")
    (click-card state :runner (get-ice state :rd 0))
    (click-card state :runner (get-ice state :hq 0))
    (is (:prompt (get-runner)) "Can't target rezzed Vanilla, prompt still open")
    (click-card state :runner (get-ice state :hq 1))
    (is (empty? (:prompt (get-runner))))
    (is (= "Vanilla" (:title (get-ice state :rd 0))) "Vanilla swapped to R&D")
    (is (= "Paper Wall" (:title (get-ice state :hq 1))) "Paper Wall swapped to HQ outer position")))

(deftest retrieval-run
  ;; Retrieval Run - Run Archives successfully and install a program from Heap for free
  (do-game
    (new-game {:runner {:deck ["Retrieval Run" "Morning Star"]}})
    (take-credits state :corp)
    (trash-from-hand state :runner "Morning Star")
    (play-from-hand state :runner "Retrieval Run")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (click-prompt state :runner "Replacement effect")
    (let [ms (first (:discard (get-runner)))]
      (click-prompt state :runner ms)
      (is (= "Morning Star" (:title (first (get-program state))))
          "Morning Star installed")
      (is (= 2 (:credit (get-runner))) "Morning Star installed at no cost")
      (is (= 2 (core/available-mu state)) "Morning Star uses 2 memory"))))

(deftest rigged-results
  ;; Rigged Results - success and failure
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck [(qty "Rigged Results" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Rigged Results")
    (click-prompt state :runner "0")
    (click-prompt state :corp "0")
    (is (empty? (:prompt (get-runner))) "Rigged Results failed for runner")
    (is (empty? (:prompt (get-corp))) "Rigged Results failed for runner")
    (play-from-hand state :runner "Rigged Results")
    (click-prompt state :runner "2")
    (click-prompt state :corp "1")
    (click-card state :runner (get-ice state :hq 0))
    (is (= [:hq] (:server (:run @state))) "Runner is running on HQ")
    (is (= 3 (:credit (get-runner))) "Rigged results spends credits")))

(deftest rip-deal
  ;; Rip Deal - replaces number of HQ accesses with heap retrieval
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Vanilla"]}
                 :runner {:deck ["Rip Deal" "Easy Mark"]}})
      (trash-from-hand state :runner "Easy Mark")
      (take-credits state :corp)
      (play-from-hand state :runner "Rip Deal")
      (run-successful state)
      (click-prompt state :runner "Replacement effect")
      (is (= "Choose 1 card to move from the Heap to your Grip" (:msg (prompt-map :runner))))
      (click-card state :runner "Easy Mark")
      (is (= 1 (-> (get-runner) :hand count)))
      (is (= "Easy Mark" (-> (get-runner) :hand first :title)))
      (is (nil? (prompt-map :corp)) "Corp should have no more prompts")
      (is (nil? (prompt-map :runner)) "Runner should have no more prompts")
      (is (nil? (get-run)) "Run is ended")))
  (testing "with Gauntlet #2942"
    (do-game
      (new-game {:corp {:deck [(qty "Vanilla" 3)]}
                 :runner {:deck ["The Gauntlet" "Rip Deal" "Easy Mark" "Sure Gamble"]}})
      (trash-from-hand state :runner "Easy Mark")
      (trash-from-hand state :runner "Sure Gamble")
      (play-from-hand state :corp "Vanilla" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "The Gauntlet")
      (play-from-hand state :runner "Rip Deal")
      (run-successful state)
      (click-prompt state :runner "1")
      (click-prompt state :runner "Replacement effect")
      (is (= "Choose 2 cards to move from the Heap to your Grip" (:msg (prompt-map :runner))))
      (click-card state :runner "Easy Mark")
      (click-card state :runner "Sure Gamble")
      (is (= 2 (-> (get-runner) :hand count)))
      (is (= ["Sure Gamble" "Easy Mark"] (->> (get-runner) :hand (map :title) (into []))))
      (is (nil? (prompt-map :corp)) "Corp should have no more prompts")
      (is (nil? (prompt-map :runner)) "Runner should have no more prompts")
      (is (nil? (get-run)) "Run is ended"))))

(deftest rumor-mill
  ;; Rumor Mill - interactions with rez effects, additional costs, general event handlers, and trash-effects
  (testing "Full test"
    (do-game
      (new-game {:corp {:deck [(qty "Project Atlas" 2)
                               "Caprice Nisei" "Chairman Hiro" "Cybernetics Court"
                               "Elizabeth Mills" "Ibrahim Salem"
                               "Housekeeping" "Director Haas" "Oberth Protocol"]}
                 :runner {:deck ["Rumor Mill"]}})
      (core/gain state :corp :credit 100 :click 100 :bad-publicity 1)
      (core/draw state :corp 100)
      (play-from-hand state :corp "Caprice Nisei" "New remote")
      (play-from-hand state :corp "Chairman Hiro" "New remote")
      (play-from-hand state :corp "Cybernetics Court" "New remote")
      (play-from-hand state :corp "Elizabeth Mills" "New remote")
      (play-from-hand state :corp "Project Atlas" "New remote")
      (play-from-hand state :corp "Ibrahim Salem" "New remote")
      (play-from-hand state :corp "Oberth Protocol" "New remote")
      (core/move state :corp (find-card "Director Haas" (:hand (get-corp))) :deck)
      (core/rez state :corp (get-content state :remote2 0))
      (core/rez state :corp (get-content state :remote3 0))
      (score-agenda state :corp (get-content state :remote5 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 100 :click 100)
      (is (= 4 (get-in (get-corp) [:hand-size :mod])) "Corp has +4 hand size")
      (is (= -2 (get-in (get-runner) [:hand-size :mod])) "Runner has -2 hand size")
      (play-from-hand state :runner "Rumor Mill")
      ;; Additional costs to rez should NOT be applied
      (core/rez state :corp (get-content state :remote6 0))
      (is (= 1 (count (:scored (get-corp)))) "No agenda was auto-forfeit to rez Ibrahim Salem")
      ;; In-play effects
      (is (zero? (get-in (get-corp) [:hand-size :mod])) "Corp has original hand size")
      (is (zero? (get-in (get-runner) [:hand-size :mod])) "Runner has original hand size")
      ;; "When you rez" effects should not apply
      (core/rez state :corp (get-content state :remote4 0))
      (is (= 1 (:bad-publicity (get-corp))) "Corp still has 1 bad publicity")
      ;; Run events (Caprice)
      ;; Make sure Rumor Mill applies even if card is rezzed after RM is put in play.
      (core/rez state :corp (get-content state :remote1 0))
      (run-on state :remote1)
      (run-continue state)
      (is (empty? (:prompt (get-corp))) "Caprice prompt is not showing")
      (run-jack-out state)
      ;; Trashable execs
      (run-empty-server state :remote2)
      (click-prompt state :runner "Pay 6 [Credits] to trash")
      (is (empty? (:scored (get-runner))) "Chairman Hiro not added to runner's score area")
      (run-jack-out state)
      (run-on state "R&D")
      (run-successful state)
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (is (empty? (:scored (get-runner))) "Director Haas not added to runner's score area")
      (take-credits state :runner)
      ;; Trash RM, make sure everything works again
      (play-from-hand state :corp "Housekeeping")
      (is (= 4 (get-in (get-corp) [:hand-size :mod])) "Corp has +4 hand size")
      (is (zero? (get-in (get-runner) [:hand-size :mod])) "Runner has +0 hand size")
      ;; Additional costs to rez should now be applied again
      (core/rez state :corp (get-content state :remote7 0))
      (click-card state :corp (get-in (get-corp) [:scored 0]))
      (is (zero? (count (:scored (get-corp)))) "Agenda was auto-forfeit to rez Oberth")
      (core/derez state :corp (get-content state :remote4 0))
      (core/rez state :corp (get-content state :remote4 0))
      (is (zero? (:bad-publicity (get-corp))) "Corp has 0 bad publicity")
      (card-ability state :corp (get-content state :remote4 0) 0) ; Elizabeth Mills, should show a prompt
      (is (:prompt (get-corp)) "Elizabeth Mills ability allowed")))
  (testing "Make sure Rumor Mill is not active when hosted on Peddler"
    (do-game
      (new-game {:corp {:deck ["Jeeves Model Bioroids"]}
                 :runner {:deck ["Street Peddler" (qty "Rumor Mill" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Street Peddler"])
      (play-from-hand state :runner "Street Peddler")
      (take-credits state :runner)
      (play-from-hand state :corp "Jeeves Model Bioroids" "New remote")
      (let [jeeves (get-content state :remote1 0)]
        (core/rez state :corp jeeves)
        (card-ability state :corp jeeves 0)
        (is (= 3 (:click (get-corp))) "Corp has 3 clicks - Jeeves working ok")))))

(deftest scrubbed
  ;; First piece of ice encountered each turn has -2 Strength for remainder of the run
  (do-game
    (new-game {:corp {:deck ["Turing"]}
               :runner {:deck ["Street Peddler"
                               (qty "Scrubbed" 3)]}})
    (starting-hand state :runner ["Street Peddler" "Scrubbed"])
    (play-from-hand state :corp "Turing" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (let [turing (get-ice state :hq 0)]
      (core/rez state :corp turing)
      (is (= 2 (:current-strength (refresh turing))))
      (run-on state "HQ")
      (run-continue state)
      (is (= 2 (:current-strength (refresh turing))) "Scrubbed not active when on Peddler")
      (play-from-hand state :runner "Scrubbed")
      (run-on state "HQ")
      (run-continue state)
      (is (zero? (:current-strength (refresh turing))) "Scrubbed reduces strength by 2")
      (run-successful state))))

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
    (is (= 2 (:click (get-runner))) "Runner spends 2 clicks on double event")
    (is (= 1 (:credit (get-runner))) "Runner pays 4 credits for Singularity")
    (run-successful state)
    (is (= 3 (count (:discard (get-corp)))) "All 3 cards trashed from Server 1")
    (is (= 1 (:credit (get-runner))) "No credits paid for trashing")
    (is (nil? (get-in @state [:corp :servers :remote1 :content])) "Server 1 no longer exists")))

(deftest stimhack
  ;; Stimhack - Gain 9 temporary credits and take 1 brain damage after the run
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"]}
               :runner {:deck ["Stimhack" "Sure Gamble"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Stimhack")
    (click-prompt state :runner "HQ")
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-successful state)
    (is (= 14 (:credit (get-runner))))
    (is (= 9 (:run-credit (get-runner))) "Gained 9 credits for use during the run")
    (click-prompt state :runner "Pay 5 [Credits] to trash") ; choose to trash Eve
    (is (and (zero? (count (:hand (get-corp))))
             (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Eve in Archives")
    (is (= 5 (:credit (get-runner))))
    (is (zero? (count (:hand (get-runner)))) "Lost card from Grip to brain damage")
    (is (= 4 (core/hand-size state :runner)))
    (is (= 1 (:brain-damage (get-runner))))))

(deftest sure-gamble
  ;; Sure Gamble
  (do-game
    (new-game {:runner {:deck ["Sure Gamble"]}})
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Sure Gamble")
    (is (= 9 (:credit (get-runner))))))

(deftest surge
  ;; Surge - Add counters if target is a virus and had a counter added this turn
  (testing "Valid target"
    (do-game
      (new-game {:runner {:deck ["Imp" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
        (play-from-hand state :runner "Surge")
        (click-card state :runner imp)
        (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))
  (testing "Don't fire surge if target is not a virus"
    (do-game
      (new-game {:runner {:deck ["Security Testing" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Security Testing")
      (let [st (get-resource state 0)]
        (play-from-hand state :runner "Surge")
        (click-card state :runner st)
        (is (not (contains? st :counter)) "Surge does not fire on Security Testing"))))
  (testing "Don't fire surge if target does not have virus counter flag set"
    (do-game
      (new-game {:runner {:deck ["Imp" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Imp")
      (let [imp (get-program state 0)]
        (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (play-from-hand state :runner "Surge")
        (click-card state :runner imp)
        (is (= 2 (get-counters (refresh imp) :virus)) "Surge does not fire on Imp turn after install"))))
  (testing "Don't allow surging Gorman Drip, since it happens on the corp turn"
    (do-game
      (new-game {:runner {:deck ["Gorman Drip v1" "Surge"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gorman Drip v1")
      (let [gd (get-program state 0)]
        (is (zero? (get-counters gd :virus)) "Gorman Drip starts without counters")
        (take-credits state :runner 3)
        (take-credits state :corp)
        (is (= 3 (get-counters (refresh gd) :virus))
            "Gorman Drip gains 3 counters after Corp clicks 3 times for credits")
        (play-from-hand state :runner "Surge")
        (click-card state :runner gd)
        (is (= 3 (get-counters (refresh gd) :virus)) "Surge does not trigger on Gorman Drip")))))

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
    (core/click-draw state :corp 1)
    (is (= 8 (:credit (get-corp))) "1st card drawn for free - System Outage on Peddler")
    (core/click-draw state :corp 1)
    (is (= 8 (:credit (get-corp))) "2nd card drawn for free - System Outage on Peddler")
    (take-credits state :corp) ; corp at 9cr
    (is (= 9 (:credit (get-corp))) "Corp at 9")
    (play-from-hand state :runner "System Outage")
    (take-credits state :runner)
    (core/click-draw state :corp 1)
    (is (= 8 (:credit (get-corp))) "1st card drawn cost 1cr - System Outage active")
    (core/click-draw state :corp 1)
    (is (= 7 (:credit (get-corp))) "2nd card drawn cost 1cr - System Outage active")))

(deftest system-seizure
  ;; System Seizure - First icebreaker boosted keeps strength for remainder of that run.
  (do-game
    (new-game {:corp {:deck ["Wraparound"]}
               :runner {:deck [(qty "Corroder" 2) "System Seizure"]}})
    (play-from-hand state :corp "Wraparound" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 3)
    (core/gain state :runner :click 2)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "System Seizure")
    (let [c1 (get-program state 0)
          c2  (get-program state 1)]
      (run-empty-server state "R&D") ;; Check that System Seizure triggers even if another run has been made
      (run-on state "HQ") ;; Check that System Seizure only keeps strength on one of the breakers
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c1 1)
      (card-ability state :runner c2 1)
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-successful state)
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-on state "HQ") ;; Check that System Seizure does not keep strength on 2nd run
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (card-ability state :runner c1 1)
      (card-ability state :runner c2 1)
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 3 strength")
      (is (= 3 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 3 strength")
      (run-continue state)
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength")
      (run-successful state)
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c1))) "Corroder 1 has 2 strength")
      (is (= 2 (core/breaker-strength state :runner (core/get-card state c2))) "Corroder 2 has 2 strength"))))

(deftest test-run
  ;; Test Run
  (testing "Programs hosted after install get returned to Stack. Issue #1081"
    (do-game
      (new-game {:corp {:deck ["Wraparound"]}
                 :runner {:deck [(qty "Test Run" 2) "Morning Star"
                                 "Knight" "Leprechaun"]}})
      (play-from-hand state :corp "Wraparound" "HQ")
      (let [wrap (get-ice state :hq 0)]
        (core/rez state :corp wrap))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (trash-from-hand state :runner "Morning Star")
      (trash-from-hand state :runner "Knight")
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (play-from-hand state :runner "Leprechaun")
        (play-from-hand state :runner "Test Run")
        (click-prompt state :runner "Heap")
        (click-prompt state :runner ms))
      (let [lep (get-program state 0)
            ms (get-program state 1)]
        (card-ability state :runner lep 1)
        (click-card state :runner ms)
        (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep"))
      (take-credits state :runner)
      (is (= "Morning Star" (:title (first (:deck (get-runner))))) "Morning Star returned to Stack from host")
      (take-credits state :corp)
      (let [kn (find-card "Knight" (:discard (get-runner)))]
        (play-from-hand state :runner "Test Run")
        (click-prompt state :runner "Heap")
        (click-prompt state :runner kn))
      (let [wrap (get-ice state :hq 0)
            kn (get-program state 1)]
        (card-ability state :runner kn 0)
        (click-card state :runner wrap)
        (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
        (take-credits state :runner)
        (is (= "Knight" (:title (first (:deck (get-runner))))) "Knight returned to Stack from host ICE"))))
  (testing "Make sure program remains installed if Scavenged"
    (do-game
      (new-game {:runner {:deck ["Test Run" "Morning Star"
                                 "Scavenge" "Inti"]}})
      (take-credits state :corp)
      (core/move state :runner (find-card "Morning Star" (:hand (get-runner))) :discard)
      (play-from-hand state :runner "Test Run")
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (click-prompt state :runner "Heap")
        (click-prompt state :runner ms)
        (is (= 2 (:credit (get-runner))) "Program installed for free")
        (let [ms (get-program state 0)]
          (play-from-hand state :runner "Scavenge")
          (click-card state :runner ms)
          (click-card state :runner (find-card "Morning Star" (:discard (get-runner))))
          (take-credits state :runner)
          (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
          (is (= "Morning Star" (:title (get-program state 0))) "Morning Star still installed"))))))

(deftest the-maker-s-eye
  (do-game
    (new-game {:corp {:deck [(qty "Quandary" 5)]}
               :runner {:deck ["The Maker's Eye"]}})
    (dotimes [_ 5] (core/move state :corp (first (:hand (get-corp))) :deck))
    (take-credits state :corp)
    (play-from-hand state :runner "The Maker's Eye")
    (is (= :rd (get-in @state [:run :server 0])))
    (run-successful state)
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "1st quandary")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "2nd quandary")
    (click-prompt state :runner "No action")
    (click-prompt state :runner "Card from deck")
    (is (= "You accessed Quandary." (-> (get-runner) :prompt first :msg)) "3rd quandary")
    (click-prompt state :runner "No action")
    (is (not (:run @state)))))

(deftest the-noble-path
  ;; The Noble Path - Prevents damage during run
  (do-game
    (new-game {:runner {:deck ["The Noble Path" (qty "Sure Gamble" 2)]}})
    (let [hand-count #(count (:hand (get-runner)))]
      (starting-hand state :runner ["The Noble Path" "Sure Gamble"])
      (take-credits state :corp)
      ;; Play The Noble Path and confirm it trashes remaining cards in hand
      (is (= 2 (hand-count)) "Start with 2 cards")
      (play-from-hand state :runner "The Noble Path")
      (is (zero? (hand-count)) "Playing Noble Path trashes the remaining cards in hand")
      ;; Put a card into hand so I can confirm it's not discarded by damage
      ;; Don't want to dealing with checking damage on a zero card hand
      (starting-hand state :runner ["Sure Gamble"])
      (core/damage state :runner :net 1)
      (is (= 1 (hand-count)) "Damage was prevented")
      ;; Finish the run and check that damage works again
      (click-prompt state :runner "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (core/damage state :runner :net 1)
      (is (zero? (hand-count)) "Damage works again after run"))))

(deftest the-price-of-freedom
  ;; The Price of Freedom - A connection must be trashed, the card is removed from game, then the corp can't advance cards next turn
  (do-game
    (new-game {:corp {:deck ["NAPD Contract"]}
               :runner {:deck ["Kati Jones" "The Price of Freedom"]}})
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp has 7 credits (play NAPD + 2 clicks for credit")
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
      (is (= 1 (count (get-in (get-runner) [:discard]))) "The Price of Freedom was removed from game, and only Kati Jones is in the discard"))
    (take-credits state :runner)
    (let [napd (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh napd)})
      (is (= 7 (:credit (get-corp))) "NAPD contract could not be advanced because of The Price of Freedom")
      (take-credits state :corp)
      (is (= 10 (:credit (get-corp))) "Corp has 10 credits now (3 clicks for credit, no click charged for failed advancing)")
      (take-credits state :runner)
      (core/advance state :corp {:card (refresh napd)})
      (core/advance state :corp {:card (refresh napd)})
      (core/advance state :corp {:card (refresh napd)})
      (is (= 7 (:credit (get-corp))) "NAPD could be advanced (3 credits charged for advancing)"))))

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
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (core/rez state :corp (refresh iwall))
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (has-subtype? (refresh iwall) "Code Gate") "Ice Wall has Code Gate")
      (is (has-subtype? (refresh iwall) "Sentry") "Ice Wall has Sentry")
      (take-credits state :runner)
      (is (has-subtype? (refresh iwall) "Barrier") "Ice Wall has Barrier")
      (is (not (has-subtype? (refresh iwall) "Code Gate")) "Ice Wall does not have Code Gate")
      (is (not (has-subtype? (refresh iwall) "Sentry")) "Ice Wall does not have Sentry"))))

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
        (is (= 2 (count (:discard (get-runner)))) "Trade-In and Astrolabe in discard")
        (is (= (dec runner-credits) (:credit (get-runner)))
            "Paid 1 credit to play Trade-In and gained 0 credits from trashing Astrolabe")))
    (testing "Trade-In lets runner search for Hardware and add it to Grip"
      (is (= 1 (count (:hand (get-runner)))) "Only 1 Trade-In in Grip")
      ;; Add sports hopper to hand
      (click-prompt state :runner (-> (get-runner) :prompt first :choices first))
      (is (= 2 (count (:hand (get-runner)))) "Sports Hopper added to Grip"))
    (testing "Gain credits when install cost is greater than 1"
      (let [runner-credits (:credit (get-runner))]
        (play-from-hand state :runner "Trade-In")
        (click-card state :runner (get-hardware state 0))
        (is (= runner-credits (:credit (get-runner)))
            "Paid 1 credit to play Trade-In and gained 1 credits from trashing Sports Hopper")
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
      (core/score state :corp {:card (refresh tg)})
      (is (= 2 (:agenda-point (get-corp))) "Last TGTBT not scored")
      (is (= 1 (count (get-content state :remote3))))
      (advance state (refresh tg) 1)
      (is (= 4 (get-counters (refresh tg) :advancement)))
      (core/score state :corp {:card (refresh tg)})
      (is (= 2 (:agenda-point (get-corp))) "Not scored with 4 advancements")
      (advance state (refresh tg) 1)
      (is (= 5 (get-counters (refresh tg) :advancement)))
      (core/score state :corp {:card (refresh tg)})
      (is (= 3 (:agenda-point (get-corp))) "Took 5 advancements to score"))))

(deftest unscheduled-maintenance
  ;; Unscheduled Maintenance - prevent Corp from installing more than 1 ICE per turn
  (do-game
    (new-game {:corp {:deck [(qty "Vanilla" 2) "Breaking News"]}
               :runner {:deck ["Unscheduled Maintenance"]}})
    (play-from-hand state :corp "Breaking News" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Unscheduled Maintenance")
    (take-credits state :runner)
    (play-from-hand state :corp "Vanilla" "HQ")
    (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "First ICE install of turn allowed")
    (play-from-hand state :corp "Vanilla" "R&D")
    (is (empty? (get-in @state [:corp :servers :rd :ices])) "Second ICE install of turn blocked")
    (score-agenda state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Vanilla" "R&D")
    (is (= 1 (count (get-in @state [:corp :servers :rd :ices]))) "Current trashed; second ICE install of turn allowed")))

(deftest vamp
  ;; Vamp - Run HQ and use replace access to pay credits to drain equal amount from Corp
  (do-game
    (new-game {:runner {:deck ["Vamp" (qty "Sure Gamble" 3)]}})
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))))
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 13 (:credit (get-runner))))
    (play-run-event state (find-card "Vamp" (:hand (get-runner))) :hq)
    (click-prompt state :runner "Replacement effect")
    (click-prompt state :runner "8")
    (is (= 1 (count-tags state)) "Took 1 tag")
    (is (= 5 (:credit (get-runner))) "Paid 8 credits")
    (is (zero? (:credit (get-corp))) "Corp lost all 8 credits")))

(deftest watch-the-world-burn
  ;; Watch the World Burn - run a remote to RFG the first card accessed
  ;; and all future copies
  (testing "Standard usage"
    (do-game
     (new-game {:corp {:deck [(qty "PAD Campaign" 2) (qty "Launch Campaign" 3)]}
                :runner {:deck ["Watch the World Burn" (qty "Same Old Thing" 2)]}})
     (play-from-hand state :corp "PAD Campaign" "New remote")
     (play-from-hand state :corp "PAD Campaign" "New remote")
     (play-from-hand state :corp "Launch Campaign" "New remote")
     (take-credits state :corp)
     (changes-val-macro 1 (count (get-in @state [:corp :rfg]))
                        "Server 1 PAD Campaign RFGed"
                        (play-from-hand state :runner "Watch the World Burn")
                        (click-prompt state :runner "Server 1")
                        (run-successful state))
     (is (zero? (:click (get-runner))) "Terminal event ends the action phase")
     (take-credits state :runner)
     (take-credits state :corp)
     (changes-val-macro 1 (count (get-in @state [:corp :rfg]))
                        "Server 2 PAD Campaign RFGed even on the next turn"
                        (run-empty-server state "Server 2"))
     (changes-val-macro 0 (count (get-in @state [:corp :rfg]))
                        "Server 3 Launch Campaign not RFGed"
                        (run-empty-server state "Server 3"))
     (click-prompt state :runner "No action")
     (core/move state :runner (find-card "Watch the World Burn" (:discard (get-runner))) :hand)
     (core/gain state :runner :credit 3) ;need to be able to play the card
     (changes-val-macro 1 (count (get-in @state [:corp :rfg]))
                        "Server 3 Launch Campaign RFGed"
                        (play-from-hand state :runner "Watch the World Burn")
                        (click-prompt state :runner "Server 3")
                        (run-successful state))
     (is (zero? (:click (get-runner))) "Terminal event ends the action phase")
     (take-credits state :runner)
     (take-credits state :corp)
     (changes-val-macro 1 (count (get-in @state [:corp :rfg]))
                        "HQ Launch Campaign RFGed"
                        (run-empty-server state "HQ"))))
  (testing "Mid-run accesses"
    (do-game
     (new-game {:corp {:deck [(qty "Mumbad Virtual Tour" 2)
                              "Kitsune"
                              "Ice Wall"]}
                :runner {:deck ["Watch the World Burn" (qty "Same Old Thing" 2)]}})
     (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
     (play-from-hand state :corp "Mumbad Virtual Tour" "New remote")
     (play-from-hand state :corp "Kitsune" "Server 1")
     (take-credits state :corp)
     (play-from-hand state :runner "Watch the World Burn")
     (click-prompt state :runner "Server 1")
     (let [kitsune (get-ice state :remote1 0)]
        (core/rez state :corp kitsune)
        (card-subroutine state :corp kitsune 0)
        (changes-val-macro 0 (count (get-in @state [:corp :rfg]))
                           "HQ Ice Wall not RFGed"
                           (click-card state :corp (find-card "Ice Wall" (:hand (get-corp)))))
        (click-prompt state :runner "No action"))
     (core/gain state :runner :credit 5)
     (is (>= (:credit (get-runner)) 5) "Runner can trash MVT if they want to")
     (changes-val-macro 0 (:credit (get-runner))
                        "Server 1 MVT doesn't trigger"
                        (run-successful state))
     (is (= 1 (count (get-in @state [:corp :rfg]))) "MVT was RFGed")
     (take-credits state :runner)
     (take-credits state :corp)
     (changes-val-macro 0 (:credit (get-runner))
                        "Server 2 MVT doesn't trigger"
                        (run-empty-server state "Server 2"))
     (is (= 2 (count (get-in @state [:corp :rfg]))) "MVT was RFGed"))))

(deftest white-hat
  ;; White Hat
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Fire Wall" "Enigma"]}
               :runner {:deck ["White Hat"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (play-from-hand state :runner "White Hat")
    (is (= :waiting (-> (get-runner) :prompt first :prompt-type)) "Runner is waiting for Corp to boost")
    (click-prompt state :corp "0")
    (click-prompt state :runner "4")
    (click-prompt state :runner (find-card "Ice Wall" (:hand (get-corp))))
    (click-prompt state :runner (find-card "Enigma" (:hand (get-corp))))
    (is (= #{"Ice Wall" "Enigma"} (->> (get-corp) :deck (map :title) (into #{}))))))
