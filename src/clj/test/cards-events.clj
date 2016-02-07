(in-ns 'test.core)

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
    (is (= 3 (:credit (get-corp))))) "Corp lost 5 credits")

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
    (prompt-choice :runner "Run ability")
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
      (is (= 2 (get-in imp [:counter])) "Imp has 2 counters after install")
      (play-from-hand state :runner "Surge")
      (prompt-select :runner imp)
      (is (= 4 (get-in (refresh imp) [:counter])) "Imp has 4 counters after surge"))))

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
      (is (= 2 (get-in imp [:counter])) "Imp has 2 counters after install")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (play-from-hand state :runner "Surge")
      (prompt-select :runner imp)
      (is (= 2 (get-in (refresh imp) [:counter]))
          "Surge does not fire on Imp turn after install"))))

(deftest surge-target-gorman-drip
  "Don't allow surging Gorman Drip, since it happens on the corp turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Gorman Drip v1" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Gorman Drip v1")
    (let [gd (get-in @state [:runner :rig :program 0])]
      (is (= nil (get-in gd [:counter])) "Gorman Drip starts without counters")
      (take-credits state :runner 3)
      (take-credits state :corp)
      (is (= 3 (get-in (refresh gd) [:counter]))
          "Gorman Drip gains 3 counters after Corp clicks 3 times for credits")
      (play-from-hand state :runner "Surge")
      (prompt-select :runner gd)
      (is (= 3 (get-in (refresh gd) [:counter])) "Surge does not trigger on Gorman Drip"))))

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
      (is (= 1 (get-in (refresh crypsis) [:counter])) "Crypsis added a virus token")
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
