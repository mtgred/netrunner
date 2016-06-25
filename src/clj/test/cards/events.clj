(ns test.cards.events
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest account-siphon-ability
  "Account Siphon - Use ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    ;; play Account Siphon, use ability
    (play-run-event state (first (:hand (get-runner))) :hq)
    (prompt-choice :runner "Run ability")
    (is (= 2 (:tag (get-runner))) "Runner took 2 tags")
    (is (= 15 (:credit (get-runner))) "Runner gained 10 credits")
    (is (= 3 (:credit (get-corp))) "Corp lost 5 credits")))

(deftest account-siphon-access
  "Account Siphon - Access"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :corp) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))) "Corp has 8 credits")
    ;; play another Siphon, do not use ability
    (play-run-event state (first (get-in @state [:runner :hand])) :hq)
    (prompt-choice :runner "Access")
    (is (= 0 (:tag (get-runner))) "Runner did not take any tags")
    (is (= 5 (:credit (get-runner))) "Runner did not gain any credits")
    (is (= 8 (:credit (get-corp))) "Corp did not lose any credits")))

(deftest amped-up
  "Amped Up - Gain 3 clicks and take 1 unpreventable brain damage"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Amped Up" 1)
                               (qty "Feedback Filter" 1)
                               (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Feedback Filter")
    (play-from-hand state :runner "Amped Up")
    (is (empty? (:prompt (get-runner)))
        "Feedback Filter brain damage prevention opportunity not given")
    (is (= 5 (:click (get-runner))) "Runner gained 2 clicks from Amped Up")
    (is (= 2 (count (:discard (get-runner)))) "Runner discarded 1 card from damage")
    (is (= 4 (core/hand-size state :runner)) "Runner handsize decreased by 1")
    (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")))

(deftest apocalypse-hosting
  "Apocalypse - Ensure MU is correct and no duplicate cards in heap"
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-runner [(qty "Scheherazade" 1) (qty "Corroder" 1)
                               (qty "Hyperdriver" 1) (qty "Apocalypse" 2)]))
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :click 3)
    (play-from-hand state :runner "Scheherazade")
    (let [scheherazade (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner scheherazade 0)
      (prompt-select :runner (find-card "Corroder" (:hand (get-runner))))
      (is (= 3 (:memory (get-runner))) "Memory at 3 (-1 from Corroder)"))
    (play-from-hand state :runner "Hyperdriver")
    (is (= 0 (:memory (get-runner))) "Memory at 0 (-1 from Corroder, -3 from Hyperdriver)")
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Apocalypse")
    (is (= 0 (count (core/all-installed state :corp))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (is (= 3 (count (get-in @state [:runner :rig :facedown]))) "Scheherazade, Corroder, Hyperdriver facedown")
    (is (= 1 (count (:discard (get-runner)))) "Only Apocalypse is in the heap")
    (is (= 4 (:memory (get-runner))) "Memory back to 4")))

(deftest apocalypse-in-play-ability
  "Apocalypse - Turn Runner cards facedown and reduce memory and hand-size gains"
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-runner [(qty "Logos" 3) (qty "Apocalypse" 3)]))
    (play-from-hand state :corp "Ice Wall" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Logos")
    (is (= 1 (:hand-size-modification (get-runner))) "Hand-size increased from Logos")
    (is (= 5 (:memory (get-runner))) "Memory increased from Logos")
    (core/gain state :runner :click 1 :credit 2)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Apocalypse")
    (is (= 0 (count (core/all-installed state :corp))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (let [logos (get-in @state [:runner :rig :facedown 0])]
      (is (:facedown (refresh logos)) "Logos is facedown")
      (is (= 0 (:hand-size-modification (get-runner))) "Hand-size reset with Logos facedown")
      (is (= 4 (:memory (get-runner))) "Memory reset with Logos facedown"))))

(deftest apocalypse-turn-facedown
  "Apocalypse - Turn Runner cards facedown without firing their leave play effects"
  (do-game
    (new-game (default-corp [(qty "Launch Campaign" 2) (qty "Ice Wall" 1)])
              (default-runner [(qty "Tri-maf Contact" 3) (qty "Apocalypse" 3)]))
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
    (is (= 0 (count (core/all-installed state :corp))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (let [tmc (get-in @state [:runner :rig :facedown 0])]
      (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
      (is (= 3 (count (:hand (get-runner))))
          "No meat damage dealt by Tri-maf's leave play effect"))))

(deftest blackmail
  "Prevent rezzing of ice for one run"
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is (= 1 (get-in @state [:corp :bad-publicity])) "Corp has 1 bad-publicity")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Blackmail")
    (prompt-choice :runner "HQ")
    (let [iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :hq 1)]
      (core/rez state :corp iwall1)
      (is (not (get-in (refresh iwall1) [:rezzed])) "First Ice Wall is not rezzed")
      (run-continue state)
      (core/rez state :corp iwall2)
      (is (not (get-in (refresh iwall2) [:rezzed])) "Second Ice Wall is not rezzed")
      (core/jack-out state :runner nil)
      ;; Do another run, where the ice should rez
      (run-on state "HQ")
      (core/rez state :corp iwall1)
      (is (get-in (refresh iwall1) [:rezzed]) "First Ice Wall is rezzed"))))

(deftest blackmail-tmi-interaction
  "Regression test for a rezzed tmi breaking game state on a blackmail run"
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is (= 1 (get-in @state [:corp :bad-publicity])) "Corp has 1 bad-publicity")
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :corp tmi)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (get-in (refresh tmi) [:rezzed]) "TMI is rezzed")
      (take-credits state :corp)
      (play-from-hand state :runner "Blackmail")
      (prompt-choice :runner "HQ")
      (run-continue state)
      (run-jack-out state)
      (run-on state "Archives"))))

(deftest corporate-scandal
  "Corporate Scandal - Corp has 1 additional bad pub even with 0"
  (do-game
    (new-game (default-corp [(qty "Elizabeth Mills" 1)])
              (default-runner [(qty "Corporate Scandal" 1) (qty "Activist Support" 1)
                               (qty "Raymond Flint" 1) (qty "Investigative Journalism" 1)]))
    (play-from-hand state :corp "Elizabeth Mills" "New remote")
    (take-credits state :corp)
    (core/gain state :runner :credit 5 :click 1)
    (play-from-hand state :runner "Raymond Flint")
    (play-from-hand state :runner "Corporate Scandal")
    (is (empty? (:prompt (get-runner))) "No BP taken, so no HQ access from Raymond")
    (play-from-hand state :runner "Investigative Journalism")
    (is (= "Investigative Journalism" (:title (get-in @state [:runner :rig :resource 1]))) "IJ able to be installed")
    (run-on state "HQ")
    (is (= 1 (:run-credit (get-runner))) "1 run credit from bad publicity")
    (run-jack-out state)
    (play-from-hand state :runner "Activist Support")
    (take-credits state :runner)
    (let [em (get-content state :remote1 0)]
      (core/rez state :corp em)
      (is (= 1 (:has-bad-pub (get-corp))) "Corp still has BP")
      (take-credits state :corp)
      (is (= 0 (:bad-publicity (get-corp))) "Corp has BP, didn't take 1 from Activist Support"))))

(deftest demolition-run
  "Demolition Run - Trash at no cost"
  (do-game
    (new-game (default-corp [(qty "False Lead" 1)
                             (qty "Shell Corporation" 1)
                             (qty "Hedge Fund" 3)])
              (default-runner [(qty "Demolition Run" 1)]))
    (core/move state :corp (find-card "False Lead" (:hand (get-corp))) :deck) ; put False Lead back in R&D
    (play-from-hand state :corp "Shell Corporation" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :corp 2) ; pass to runner's turn by taking credits
    (play-from-hand state :runner "Demolition Run")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits for the event")
    (prompt-choice :runner "R&D")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (prompt-choice :runner "OK") ; dismiss instructional prompt for Demolition Run
    (run-successful state)
    (let [demo (get-in @state [:runner :play-area 0])] ; Demolition Run "hack" is to put it out in the play area
      (prompt-choice :runner "Unrezzed upgrade in R&D")
      (card-ability state :runner demo 0)
      (is (= 3 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
      (prompt-choice :runner "Card from deck")
      (card-ability state :runner demo 0)  ; trash False Lead instead of stealing
      (is (= 0 (:agenda-point (get-runner))) "Didn't steal False Lead")
      (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
      (is (empty? (:prompt (get-runner))) "Run concluded"))))

(deftest dirty-laundry
  "Dirty Laundry - Gain 5 credits at the end of the run if it was successful"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Dirty Laundry" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Dirty Laundry")
    (prompt-choice :runner "Archives")
    (run-successful state)
    (is (= 8 (:credit (get-runner))) "Gained 5 credits")
    (play-from-hand state :runner "Dirty Laundry")
    (prompt-choice :runner "Archives")
    (run-jack-out state)
    (is (= 6 (:credit (get-runner))) "Run unsuccessful; gained no credits")))

(deftest drive-by
  "Drive By - Expose card in remote server and trash if asset or upgrade"
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 2)
                             (qty "Product Placement" 1)
                             (qty "Project Atlas" 1)])
              (default-runner [(qty "Drive By" 2)]))
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
      (prompt-select :runner pp)
      (is (= 1 (count (get-in @state [:corp :servers :hq :content])))
          "Upgrades in root of central servers can't be targeted")
      (prompt-select :runner (refresh eve1))
      (is (= 1 (count (get-in @state [:corp :servers :remote1 :content])))
          "Rezzed cards can't be targeted")
      (prompt-select :runner eve2)
      (is (= 2 (:click (get-runner))) "Spent 2 clicks")
      (is (and (= 1 (count (:discard (get-corp))))
               (= 5 (:credit (get-runner))))
          "Eve trashed at no cost")
      (is (nil? (get-in @state [:corp :servers :remote2 :content])) "Server 2 no longer exists")
      (play-from-hand state :runner "Drive By")
      (prompt-select :runner atl)
      (is (= 0 (:click (get-runner))) "Runner has 0 clicks left")
      (is (= 1 (count (get-in @state [:corp :servers :remote3 :content])))
          "Project Atlas not trashed from Server 3"))))

(deftest employee-strike-blue-sun
  "Employee Strike - vs Blue Sun, suppress Step 1.2"
  (do-game
    (new-game (make-deck "Blue Sun: Powering the Future" [(qty "Ice Wall" 1)])
              (default-runner [(qty "Employee Strike" 1) (qty "Scrubbed" 1)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (core/rez state :corp (get-ice state :hq 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Employee Strike")
    (take-credits state :runner)
    (is (not (:corp-phase-12 @state)) "Employee Strike suppressed Blue Sun step 1.2")))

    (deftest freelance-coding-contract
  "Freelance Coding Contract - Gain 2 credits per program trashed from Grip"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Freelance Coding Contract" 1)
                               (qty "Paricia" 1)
                               (qty "Cloak" 1)
                               (qty "Inti" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Freelance Coding Contract")
    (prompt-select :runner (find-card "Cloak" (:hand (get-runner))))
    (prompt-select :runner (find-card "Paricia" (:hand (get-runner))))
    (prompt-select :runner (find-card "Inti" (:hand (get-runner))))
    (prompt-choice :runner "Done")
    (is (= 3 (count (filter #(= (:type %) "Program") (:discard (get-runner)))))
        "3 programs in Heap")
    (is (= 11 (:credit (get-runner))) "Gained 6 credits from 3 trashed programs")))

(deftest game-day
  "Game Day - draw until at handsize"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Game Day" 3)
                              (qty "Public Sympathy" 3)
                              (qty "Sure Gamble" 3)
                              (qty "Easy Mark" 3)]))
   (take-credits state :corp)
   ;; move needed cards to hand -- in case they were not drawn
   (core/move state :runner (find-card "Game Day" (:deck (get-runner))) :hand)
   (core/move state :runner (find-card "Public Sympathy" (:deck (get-runner))) :hand)
   (play-from-hand state :runner "Public Sympathy")
   (is (= 7 (core/hand-size state :runner)) "Runner hand size is 7")
   (play-from-hand state :runner "Game Day")
   (is (= 7 (count (:hand (get-runner)))) "Drew up to 7 cards")))

(deftest independent-thinking
  "Independent Thinking - Trash 2 installed cards, including a facedown directive, and draw 2 cards"
  (do-game
    (new-game
      (default-corp)
      (make-deck "Apex: Invasive Predator" [(qty "Neutralize All Threats" 1)
                                            (qty "Independent Thinking" 2)
                                            (qty "Fan Site" 3)
                                            (qty "Street Magic" 3)]))
    (starting-hand state :runner ["Fan Site"
                                  "Fan Site"
                                  "Neutralize All Threats"
                                  "Independent Thinking"
                                  "Independent Thinking"])
    (take-credits state :corp)
    (prompt-select :runner (find-card "Neutralize All Threats" (:hand (get-runner))))
    (play-from-hand state :runner "Fan Site")
    (let [fs (get-in @state [:runner :rig :resource 0])
          nat (get-in @state [:runner :rig :facedown 0])]
      (play-from-hand state :runner "Independent Thinking")
      (prompt-select :runner fs)
      (prompt-select :runner nat)
      (prompt-choice :runner "Done")
      (is (= 4 (count (:hand (get-runner)))) "Trashing 2 cards draws 2 card"))))


(deftest information-sifting
  "Information Sifting - complicated interactions with damage prevention"
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping"
                         [(qty "Snare!" 1) (qty "PAD Campaign" 1) (qty "Hostile Infrastructure" 1)
                          (qty "Braintrust" 1) (qty "Hedge Fund" 1) (qty "Power Shutdown" 1)])
              (default-runner [(qty "Information Sifting" 2) (qty "Deus X" 2) (qty "Sure Gamble" 1)]))
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")

    (core/gain state :corp :credit 10)
    (core/rez state :corp (get-content state :remote1 0))
    (core/gain state :runner :credit 10)
    (take-credits state :corp)
    (play-from-hand state :runner "Deus X")
    (play-from-hand state :runner "Deus X")
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (prompt-select :corp (find-card "Snare!" (:hand (get-corp))))
    (prompt-select :corp (find-card "PAD Campaign" (:hand (get-corp))))
    (prompt-choice :corp "Done")
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Corp is waiting for Runner selection")
    (prompt-choice :runner "Pile 1")
    (prompt-choice :runner "Card from Pile 1")
    ; the cards are selected randomly :(
    (letfn [(prevent-snare [existing-dmg]
              (prompt-choice :corp "Yes")
              (card-ability state :runner (get-program state 0) 1)
              (prompt-choice :runner "Done")
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Damage from Snare! prevented")
              (prompt-choice :runner "Yes")
              (prompt-choice :runner "Done") ; don't prevent Hostile dmg
              ; chronos prompt
              (prompt-choice :corp "Yes")
              (prompt-choice :corp (find-card "Sure Gamble" (:hand (get-runner))))
              (is (= (+ 2 existing-dmg) (count (:discard (get-runner)))) "Damage from Hostile Inf not prevented"))
            (allow-pad [existing-dmg]
              (prompt-choice :runner "Yes")
              (card-ability state :runner (get-program state 0) 1)
              (prompt-choice :runner "Done")
              (is (= (inc existing-dmg) (count (:discard (get-runner)))) "Runner prevented damage from Hostile Inf"))]
      (if (= :waiting (-> (get-runner) :prompt first :prompt-type)) ; hit the snare
        ; prevent the damage
        (do (prevent-snare (count (:discard (get-runner))))
            (prompt-choice :runner "Card from Pile 1")
            (allow-pad (count (:discard (get-runner)))))
        (do (allow-pad (count (:discard (get-runner))))
            (prompt-choice :runner "Card from Pile 1")
            (prevent-snare (count (:discard (get-runner)))))))
    (play-run-event state (find-card "Information Sifting" (:hand (get-runner))) :hq)
    (prompt-select :corp (find-card "Power Shutdown" (:hand (get-corp))))
    (prompt-select :corp (find-card "Hedge Fund" (:hand (get-corp))))
    (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Selecting max cards closed the selection prompt")
    (prompt-choice :runner "Pile 2")
    (prompt-choice :runner "Card from Pile 2")
    (prompt-choice :runner "Steal")
    (is (= 1 (count (:scored (get-runner)))) "Runner stole agenda")))

(deftest inject
  "Inject - Draw 4 cards from Stack and gain 1 credit per trashed program"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Inject" 1) (qty "Imp" 2) (qty "Sure Gamble" 2)]))
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

(deftest lawyer-up
  "Lawyer Up - Lose 2 tags and draw 3 cards"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Lawyer Up" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/gain state :runner :tag 3)
    (play-from-hand state :runner "Lawyer Up")
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (= 2 (:click (get-runner))) "Spent 2 clicks")
    (is (= 1 (:tag (get-runner))) "Lost 2 tags")))

(deftest modded
  "Modded - Install a program or piece of hardware at a 3 credit discount"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Modded" 2)
                               (qty "HQ Interface" 1)
                               (qty "Nerve Agent" 1)
                               (qty "Earthrise Hotel" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Modded")
    (prompt-select :runner (find-card "Earthrise Hotel" (:hand (get-runner))))
    (is (empty? (get-in @state [:runner :rig :resource])) "Can't install resources with Modded")
    (prompt-select :runner (find-card "HQ Interface" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rig :hardware]))) "Installed HQ Interface")
    (is (= 4 (:credit (get-runner))) "Paid 1 credit instead of 4")
    (play-from-hand state :runner "Modded")
    (prompt-select :runner (find-card "Nerve Agent" (:hand (get-runner))))
    (is (= 1 (count (get-in @state [:runner :rig :program]))) "Installed Nerve Agent")
    (is (= 4 (:credit (get-runner))) "Paid 0 credits")))

(deftest noble-path
  "The Noble Path - Prevents damage during run"
  (do-game
    (new-game (default-corp) (default-runner [(qty "The Noble Path" 1) (qty "Hedge Fund" 2)]))
    (let [hand-count #(count (:hand (get-runner)))]
      (starting-hand state :runner ["The Noble Path" "Hedge Fund"])
      (take-credits state :corp)

      ; Play The Noble Path and confirm it trashes remaining cards in hand
      (is (= 2 (hand-count)) "Start with 2 cards")
      (play-from-hand state :runner "The Noble Path")
      (is (= 0 (hand-count)) "Playing Noble Path trashes the remaining cards in hand")

      ; Put a card into hand so I can confirm it's not discarded by damage
      ; Don't want to dealing with checking damage on a zero card hand
      (starting-hand state :runner ["Hedge Fund"])

      (core/damage state :runner :net 1)
      (is (= 1 (hand-count)) "Damage was prevented")

      ; Finish the run and check that damage works again
      (prompt-choice :runner "HQ")
      (run-successful state)
      (prompt-choice :runner "OK")
      (core/damage state :runner :net 1)
      (is (= 0 (hand-count)) "Damage works again after run"))))

(deftest notoriety
  "Notoriety - Run all 3 central servers successfully and play to gain 1 agenda point"
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 1)])
              (default-runner [(qty "Notoriety" 1)]))
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (run-empty-server state "Archives")
    (run-empty-server state "R&D")
    (run-empty-server state "HQ")
    (play-from-hand state :runner "Notoriety")
    (is (= 1 (count (:scored (get-runner)))) "Notoriety moved to score area")
    (is (= 1 (:agenda-point (get-runner))) "Notoriety scored for 1 agenda point")))

(deftest political-graffiti
  "Political Graffiti - swapping with Turntable works / purging viruses restores points"
  (do-game
    (new-game (default-corp [(qty "Breaking News" 1) (qty "Chronos Project" 1)])
              (default-runner [(qty "Turntable" 1) (qty "Political Graffiti" 1)]))
    (play-from-hand state :corp "Breaking News" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (is (= 1 (:agenda-point (get-corp))))
    (take-credits state :corp)
    (play-from-hand state :runner "Political Graffiti")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :runner "Run ability")
    (prompt-select :runner (find-card "Breaking News" (:scored (get-corp))))
    (is (= 0 (:agenda-point (get-corp))) "Political Dealings lowered agenda points by 1")
    (play-from-hand state :runner "Turntable")
    (run-empty-server state "HQ")
    (prompt-choice :runner "Steal")
    (let [tt (get-in @state [:runner :rig :hardware 0])]
      (prompt-choice :runner "Yes")
      (prompt-select :runner (find-card "Breaking News" (:scored (get-corp))))
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 0 (:agenda-point (get-runner))))
      (take-credits state :runner)
      (core/purge state :corp)
      (is (= 1 (:agenda-point (get-corp))))
      (is (= 1 (:agenda-point (get-runner)))))))

(deftest push-your-luck-correct-guess
  "Push Your Luck - Corp guesses correctly"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Push Your Luck" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Push Your Luck")
    (prompt-choice :corp "Odd")
    (prompt-choice :runner 3)
    (is (= 0 (:credit (get-runner))) "Corp guessed correctly")))

(deftest push-your-luck-incorrect-guess
  "Push Your Luck - Corp guesses incorrectly"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Push Your Luck" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Push Your Luck")
    (prompt-choice :corp "Even")
    (prompt-choice :runner 3)
    (is (= 6 (:credit (get-runner))) "Corp guessed incorrectly")))

;; Rebirth
(let [choose-runner (fn [name state prompt-map]
                      (let [kate-choice (some #(when (= name (:title %)) %) (:choices (prompt-map :runner)))]
                        (core/resolve-prompt state :runner {:choice kate-choice})))

      kate "Kate \"Mac\" McCaffrey: Digital Tinker"
      kit "Rielle \"Kit\" Peddler: Transhuman"
      professor "The Professor: Keeper of Knowledge"
      jamie "Jamie \"Bzzz\" Micken: Techno Savant"
      chaos "Chaos Theory: Wünderkind"
      whizzard "Whizzard: Master Gamer"
      reina "Reina Roja: Freedom Fighter"]

  (deftest rebirth-kate
    "Rebirth - Kate's discount applies after rebirth"
    (do-game 
      (new-game (default-corp) (default-runner ["Magnum Opus" "Rebirth"]) {:start-as :runner})

      (play-from-hand state :runner "Rebirth")
      (is (= (first (prompt-titles :runner)) chaos) "List is sorted")
      (is (every?   #(some #{%} (prompt-titles :runner))
                    [kate kit]))
      (is (not-any? #(some #{%} (prompt-titles :runner))
                    [professor whizzard jamie]))

      (choose-runner kate state prompt-map)

      (is (= kate (-> (get-runner) :identity :title)))
      (is (= 1 (:link (get-runner))) "1 link")

      (is (empty? (:discard (get-runner))))
      (is (= "Rebirth" (-> (get-runner) :rfg first :title)))

      (is (changes-credits (get-runner) -4
        (play-from-hand state :runner "Magnum Opus")))))

  (deftest-pending rebirth-kate-twice
    "Rebirth - Kate's discount does not after rebirth if something already installed"
    (do-game
      (new-game (default-corp) (default-runner ["Akamatsu Mem Chip" "Rebirth" "Clone Chip"]) {:start-as :runner})

      (play-from-hand state :runner "Clone Chip")
      (play-from-hand state :runner "Rebirth")
      (choose-runner kate state prompt-map)

      (is (changes-credits (get-corp) -1
        (play-from-hand state :runner "Akamatsu Mem Chip"))
        "Discount not applied for 2nd install")))

  (deftest rebirth-whizzard
    "Rebirth - Whizzard works after rebirth"
    (do-game
      (new-game (default-corp ["Ice Wall"]) (make-deck reina ["Rebirth"]))
      (play-from-hand state :corp "Ice Wall" "R&D")
      (take-credits state :corp)

      (play-from-hand state :runner "Rebirth")
      (choose-runner whizzard state prompt-map)

      (card-ability state :runner (:identity (get-runner)) 0)
      (is (= 6 (:credit (get-runner))) "Took a Whizzard credit")

      (is (changes-credits (get-corp) -1
        (core/rez state :corp (get-ice state :rd 0)))
        "Reina is no longer active"))))

(deftest rigged-results
  "Rigged Results - success and failure"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1)])
              (default-runner [(qty "Rigged Results" 3)]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Rigged Results")
    (prompt-choice :runner "0")
    (prompt-choice :corp "0")
    (is (empty? (:prompt (get-runner))) "Rigged Results failed for runner")
    (is (empty? (:prompt (get-corp))) "Rigged Results failed for runner")
    (play-from-hand state :runner "Rigged Results")
    (prompt-choice :runner "0")
    (prompt-choice :corp "1")
    (prompt-select :runner (get-ice state :hq 0))
    (is (= [:hq] (:server (:run @state))) "Runner is running on HQ")))

(deftest retrieval-run
  "Retrieval Run - Run Archives successfully and install a program from Heap for free"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Retrieval Run" 1) (qty "Morning Star" 1)]))
    (take-credits state :corp)
    (trash-from-hand state :runner "Morning Star")
    (play-from-hand state :runner "Retrieval Run")
    (is (= [:archives] (get-in @state [:run :server])) "Run initiated on Archives")
    (run-successful state)
    (prompt-choice :runner "Run ability")
    (let [ms (first (:discard (get-runner)))]
      (prompt-choice :runner ms)
      (is (= "Morning Star" (:title (first (get-in @state [:runner :rig :program]))))
          "Morning Star installed")
      (is (= 2 (:credit (get-runner))) "Morning Star installed at no cost")
      (is (= 2 (:memory (get-runner))) "Morning Star uses 2 memory"))))

(deftest singularity
  "Singularity - Run a remote; if successful, trash all contents at no cost"
  (do-game
    (new-game (default-corp [(qty "Caprice Nisei" 1)
                             (qty "Breaker Bay Grid" 1)
                             (qty "Eve Campaign" 1)])
              (default-runner [(qty "Singularity" 1)]))
    (play-from-hand state :corp "Breaker Bay Grid" "New remote")
    (play-from-hand state :corp "Caprice Nisei" "Server 1")
    (play-from-hand state :corp "Eve Campaign" "Server 1")
    (take-credits state :corp)
    (play-from-hand state :runner "Singularity")
    (prompt-choice :runner "Server 1")
    (is (= 2 (:click (get-runner))) "Runner spends 2 clicks on double event")
    (is (= 1 (:credit (get-runner))) "Runner pays 4 credits for Singularity")
    (run-successful state)
    (is (= 3 (count (:discard (get-corp)))) "All 3 cards trashed from Server 1")
    (is (= 1 (:credit (get-runner))) "No credits paid for trashing")
    (is (nil? (get-in @state [:corp :servers :remote1 :content])) "Server 1 no longer exists")))

(deftest stimhack
  "Stimhack - Gain 9 temporary credits and take 1 brain damage after the run"
  (do-game
    (new-game (default-corp [(qty "Eve Campaign" 1)])
              (default-runner [(qty "Stimhack" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Stimhack")
    (prompt-choice :runner "HQ")
    (is (= [:hq] (get-in @state [:run :server])) "Run initiated on HQ")
    (run-successful state)
    (is (= 14 (:credit (get-runner))))
    (is (= 9 (:run-credit (get-runner))) "Gained 9 credits for use during the run")
    (prompt-choice :runner "Yes") ; choose to trash Eve
    (is (and (= 0 (count (:hand (get-corp))))
             (= 1 (count (:discard (get-corp)))))
        "Corp hand empty and Eve in Archives")
    (is (= 5 (:credit (get-runner))))
    (is (= 0 (count (:hand (get-runner)))) "Lost card from Grip to brain damage")
    (is (= 4 (core/hand-size state :runner)))
    (is (= 1 (:brain-damage (get-runner))))))

(deftest sure-gamble
  "Sure Gamble"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Sure Gamble" 1)]))
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (play-from-hand state :runner "Sure Gamble")
    (is (= 9 (:credit (get-runner))))))

;; Surge and virus counter flag tests
(deftest surge-valid-target
  "Add counters if target is a virus and had a counter added this turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
      (play-from-hand state :runner "Surge")
      (prompt-select :runner imp)
      (is (= 4 (get-counters (refresh imp) :virus)) "Imp has 4 counters after surge"))))

(deftest surge-target-not-virus
  "Don't fire surge if target is not a virus"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Security Testing" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Security Testing")
    (let [st (get-in @state [:runner :rig :resource 0])]
      (play-from-hand state :runner "Surge")
      (prompt-select :runner st)
      (is (not (contains? st :counter)) "Surge does not fire on Security Testing"))))

(deftest surge-target-no-token-this-turn
  "Don't fire surge if target does not have virus counter flag set"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (= 2 (get-counters imp :virus)) "Imp has 2 counters after install")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (play-from-hand state :runner "Surge")
      (prompt-select :runner imp)
      (is (= 2 (get-counters (refresh imp) :virus))
          "Surge does not fire on Imp turn after install"))))

(deftest surge-target-gorman-drip
  "Don't allow surging Gorman Drip, since it happens on the corp turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Gorman Drip v1" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Gorman Drip v1")
    (let [gd (get-in @state [:runner :rig :program 0])]
      (is (= 0 (get-counters gd :virus)) "Gorman Drip starts without counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 3 (get-counters (refresh gd) :virus))
          "Gorman Drip gains 3 counters after Corp clicks 3 times for credits")
      (play-from-hand state :runner "Surge")
      (prompt-select :runner gd)
      (is (= 3 (get-counters (refresh gd) :virus)) "Surge does not trigger on Gorman Drip"))))

(deftest test-run
  "Test Run - Programs hosted after install get returned to Stack. Issue #1081"
  (do-game
    (new-game (default-corp [(qty "Wraparound" 1)])
              (default-runner [(qty "Test Run" 2) (qty "Morning Star" 1)
                               (qty "Knight" 1) (qty "Leprechaun" 1)]))
    (play-from-hand state :corp "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :corp wrap)
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (core/move state :runner (find-card "Morning Star" (:hand (get-runner))) :discard)
      (core/move state :runner (find-card "Knight" (:hand (get-runner))) :discard)
      (let [ms (find-card "Morning Star" (:discard (get-runner)))]
        (play-from-hand state :runner "Leprechaun")
        (play-from-hand state :runner "Test Run")
        (prompt-choice :runner "Heap")
        (prompt-choice :runner ms)
        (let [lep (get-in @state [:runner :rig :program 0])
              ms (get-in @state [:runner :rig :program 1])]
          (card-ability state :runner lep 1)
          (prompt-select :runner ms)
          (is (= "Morning Star" (:title (first (:hosted (refresh lep))))) "Morning Star hosted on Lep")
          (take-credits state :runner)
          (is (= "Morning Star" (:title (first (:deck (get-runner))))) "Morning Star returned to Stack from host")
          (take-credits state :corp)
          (let [kn (find-card "Knight" (:discard (get-runner)))]
            (play-from-hand state :runner "Test Run")
            (prompt-choice :runner "Heap")
            (prompt-choice :runner kn)
            (let [kn (get-in @state [:runner :rig :program 1])]
              (card-ability state :runner kn 0)
              (prompt-select :runner wrap)
              (is (= "Knight" (:title (first (:hosted (refresh wrap))))) "Knight hosted on Wraparound")
              (take-credits state :runner)
              (is (= "Knight" (:title (first (:deck (get-runner))))) "Knight returned to Stack from host ICE"))))))))

(deftest test-run-scavenge
  "Test Run - Make sure program remains installed if Scavenged"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Test Run" 1) (qty "Morning Star" 1)
                               (qty "Scavenge" 1) (qty "Inti" 1)]))
    (take-credits state :corp)
    (core/move state :runner (find-card "Morning Star" (:hand (get-runner))) :discard)
    (play-from-hand state :runner "Test Run")
    (let [ms (find-card "Morning Star" (:discard (get-runner)))]
      (prompt-choice :runner "Heap")
      (prompt-choice :runner ms)
      (is (= 2 (:credit (get-runner))) "Program installed for free")
      (let [ms (get-in @state [:runner :rig :program 0])]
        (play-from-hand state :runner "Scavenge")
        (prompt-select :runner ms)
        (prompt-select :runner (find-card "Morning Star" (:discard (get-runner))))
        (take-credits state :runner)
        (is (empty? (:deck (get-runner))) "Morning Star not returned to Stack")
        (is (= "Morning Star" (:title (get-in @state [:runner :rig :program 0]))) "Morning Star still installed")))))

(deftest the-price-of-freedom
  "The Price of Freedom - A connection must be trashed, the card is removed from game, then the corp can't advance cards next turn"
  (do-game
    (new-game (default-corp [(qty "NAPD Contract" 1)])
              (default-runner [(qty "Kati Jones" 1) (qty "The Price of Freedom" 1)]))
    (play-from-hand state :corp "NAPD Contract" "New remote")
    (take-credits state :corp)
    (is (= 7 (:credit (get-corp))) "Corp has 7 credits (play NAPD + 2 clicks for credit")
    (play-from-hand state :runner "The Price of Freedom")
    (is (= 2 (count (get-in @state [:runner :hand]))) "The Price of Freedom could not be played because no connection is installed")
    (is (= 0 (count (get-in (get-runner) [:rig :resource]))) "Kati Jones is not installed")
    (play-from-hand state :runner "Kati Jones")
    (is (= 1 (count (get-in @state [:runner :rig :resource]))) "Kati Jones was installed")
    (play-from-hand state :runner "The Price of Freedom")
    (is (= 0 (count (get-in @state [:runner :hand]))) "The Price of Freedom can be played because a connection is in play")
    (let [kj (find-card "Kati Jones" (:resource (:rig (get-runner))))]
      (prompt-select :runner kj)
      (is (= 0 (count (get-in (get-runner) [:rig :resource]))) "Kati Jones was trashed wth The Price of Freedom")
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


(deftest vamp
  "Vamp - Run HQ and use replace access to pay credits to drain equal amount from Corp"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Vamp" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (is (= 8 (:credit (get-corp))))
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Sure Gamble")
    (is (= 13 (:credit (get-runner))))
    (play-run-event state (find-card "Vamp" (:hand (get-runner))) :hq)
    (prompt-choice :runner "Run ability")
    (prompt-choice :runner 8)
    (is (= 1 (:tag (get-runner))) "Took 1 tag")
    (is (= 5 (:credit (get-runner))) "Paid 8 credits")
    (is (= 0 (:credit (get-corp))) "Corp lost all 8 credits")))

(deftest virus-counter-flag-on-enter
  "Set counter flag when virus card enters play with counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Surge" 1) (qty "Imp" 1) (qty "Crypsis" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (get-in imp [:added-virus-counter]) "Counter flag was not set on Imp"))))

(deftest virus-counter-flag-on-add-prop
  "Set counter flag when add-prop is called on a virus"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Crypsis" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner crypsis 2) ;click to add a virus counter
      (is (= 1 (get-counters (refresh crypsis) :virus)) "Crypsis added a virus token")
      (is (get-in (refresh crypsis) [:added-virus-counter])
          "Counter flag was set on Crypsis"))))

(deftest virus-counter-flag-clear-on-end-turn
  "Clear the virus counter flag at the end of each turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Crypsis" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner crypsis 2) ; click to add a virus counter
      (take-credits state :runner 2)
      (take-credits state :corp 1)
      (is (not (get-in (refresh crypsis) [:added-virus-counter]))
          "Counter flag was cleared on Crypsis"))))
