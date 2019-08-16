(ns game-test.cards.hardware
  (:require [game.core :as core]
            [game.core.card :refer :all]
            [game.utils :as utils]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest acacia
  ;; Acacia - Optionally gain credits for number of virus tokens then trash
  (testing "Basic test"
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
      (core/purge state :corp)
      (click-prompt state :runner "Yes")
      (is (= 9 (:credit (get-runner))) "Runner gained 9 credits")
      (is (= 1 (count (:discard (get-runner)))) "Acacia has trashed")))
  (testing "Issue #4280: Interaction with LLDS Energy Regulator"
    (do-game
      (new-game {:runner {:deck ["Acacia" "LLDS Energy Regulator" "Datasucker"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Acacia")
      (play-from-hand state :runner "Datasucker")
      (play-from-hand state :runner "LLDS Energy Regulator")
      (core/add-counter state :runner (get-program state 0) :virus 3)
      (take-credits state :runner)
      (let [llds (get-program state 1)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Runner didn't get credits before deciding on LLDS"
                           (core/purge state :corp)
                           (click-prompt state :runner "Yes"))
        (changes-val-macro -3 (:credit (get-runner))
                           "Runner pays 3 for LLDS"
                           (card-ability state :runner (refresh llds) 0))
        (changes-val-macro 3 (:credit (get-runner))
                           "Runner got Acacia credits"
                           (click-prompt state :runner "Done"))
        (is (= 0 (count (:discard (get-runner)))) "Acacia has not been trashed")))))

(deftest akamatsu-mem-chip
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game {:runner {:deck [(qty "Akamatsu Mem Chip" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")))

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
    (play-from-hand state :corp "Snare!" "New remote")
    (is (= 5 (count (:hand (get-runner)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (play-from-hand state :corp "Snare!" "Server 0")
    (is (= 5 (count (:hand (get-runner)))) "Did not draw")
    (is (= 1 (count (:deck (get-runner)))) "1 card left in deck")))

(deftest autoscripter
  ;; Autoscripter - gain 1 [Click] first time Runner installs program from Grip during their turn.
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
      (core/trash state :runner (get-program state 0))
      (card-ability state :runner (get-hardware state 1) 0)
      (click-card state :runner (first (:discard (get-runner))))
      (is (= 3 (count (get-program state))) "Three Intis installed")
      (is (= 3 (:click (get-runner))) "Did not gain a click from installing a Program from heap"))
    (testing "Trashing on unsuccessful run"
      (run-on state :hq)
      (run-jack-out state)
      (is (= "Autoscripter" (:title (last (:discard (get-runner))))) "Autoscripter was trashed after successful run"))))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (testing "Basic test"
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
      (let [snitch (get-program state 0)
            iwall (get-ice state :archives 0)]
        (run-on state :archives)
        (card-ability state :runner snitch 0)
        (is (rezzed? (refresh iwall)) "Ice Wall was rezzed"))))
  (testing "Additional cost handling, issue #1244"
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
        (click-prompt state :corp "No")))))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
    (new-game {:runner {:deck ["Box-E"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Box-E")
    (is (= 6 (core/available-mu state)))
    (is (= 7 (hand-size :runner)))))

(deftest bookmark
  ;; Bookmark
  (do-game
    (new-game {:corp {:deck [(qty "Hedge Fund" 10)]}
               :runner {:hand ["Bookmark" "Sure Gamble" "Daily Casts" "Brain Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Bookmark")
    (let [bm (get-hardware state 0)]
    (card-ability state :runner bm 0)
    (click-card state :runner "Sure Gamble")
    (click-card state :runner "Daily Casts")
    (click-card state :runner "Brain Chip")
    (is (= 3 (count (:hosted (refresh bm)))) "Bookmark can host cards of any type")
    (card-ability state :runner bm 2)
    (is (nil? (refresh bm)) "Bookmark is now trashed")
    (is (= 3 (count (:hand (get-runner)))) "Bookmark moved all hosted card into the grip"))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
    (new-game {:runner {:deck ["Brain Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Brain Chip")
    (swap! state assoc-in [:runner :agenda-point] -2) ; hard set ap
    (is (= 5 (hand-size :runner)) "Hand size unaffected")
    (is (= 4 (core/available-mu state)) "Memory limit unaffected")
    (swap! state assoc-in [:runner :agenda-point] 2)
    (is (= 7 (hand-size :runner)) "Hand size increased by 2")
    (is (= 6 (core/available-mu state)) "Memory limit increased by 2")
    (core/move state :runner (get-hardware state 0) :discard)
    (is (= 5 (hand-size :runner)) "Hand size reset")
    (is (= 4 (core/available-mu state)) "Memory limit reset")))

(deftest chop-bot-3000
  ;; Chop Bot 3000 - when your turn beings trash 1 card, then draw or remove tag
  (do-game
    (new-game {:runner {:deck ["Chop Bot 3000", "Spy Camera"]}})
    (take-credits state :corp)
    (core/gain state :runner :tag 2)
    (play-from-hand state :runner "Chop Bot 3000")
    (play-from-hand state :runner "Spy Camera")
    (is (= 2 (count-tags state)) "Runner has 2 tags")
    (take-credits state :runner)
    (take-credits state :corp)
    (is (:runner-phase-12 @state) "Runner in Step 1.2")
    (let [cb (get-hardware state 0)]
      (is (empty? (:discard (get-runner))) "No cards in trash")
      (card-ability state :runner cb 0)
      (click-card state :runner (find-card "Spy Camera" (get-hardware state)))
      (click-prompt state :runner "Remove 1 tag")
      (is (= 1 (count (:discard (get-runner)))) "Spy Camera trashed")
      (is (= 1 (count-tags state)) "Runner lost 1 tag")
      (core/end-phase-12 state :runner nil))))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Datasucker" (qty "Clone Chip" 2)]}})
      (take-credits state :corp)
      (trash-from-hand state :runner "Datasucker")
      (play-from-hand state :runner "Clone Chip")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (click-card state :runner "Datasucker")
        (let [ds (get-program state 0)]
          (is (not (nil? ds)))
          (is (= (:title ds) "Datasucker"))))))
  (testing "don't show invalid choices"
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
          (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left"))))))

(deftest comet
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game {:runner {:deck [(qty "Comet" 3) (qty "Easy Mark" 2)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Comet")
    (let [comet (get-hardware state 0)]
      (play-from-hand state :runner "Easy Mark")
      (is (true? (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :runner comet 0)
      (is (= (:cid comet) (-> @state :runner :prompt first :card :cid)))
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 7 (:credit (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of an ICE until end of turn
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
      (core/rez state :corp quan)
      (is (= 4 (:credit (get-corp))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cyberfeeder
  ;; Cyberfeeder
  (testing "Pay-credits prompt on install"
    (do-game
      (new-game {:runner {:deck ["Cyberfeeder" "Clot" "Equivocation"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cyberfeeder")
      (play-from-hand state :runner "Equivocation")
      (is (= 1 (:credit (get-runner))) "No pay-credits prompt if non-virus program installed")
      (is (empty? (:prompt (get-runner))) "No pay-credits prompt if non-virus program installed")
      (play-from-hand state :runner "Clot")
      (let [cyb (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from Cyberfeeder"
                           (click-card state :runner cyb)))))
  (testing "Pay-credits prompt on using icebreaker"
    (do-game
      (new-game {:runner {:deck ["Cyberfeeder" "Corroder"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cyberfeeder")
      (play-from-hand state :runner "Corroder")
      (let [cyb (get-hardware state 0)
            co (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Cyberfeeder"
                           (card-ability state :runner co 1)
                           (click-card state :runner cyb))))))

(deftest cybsoft-macrodrive
  ;; Cybsoft MacroDrive
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Cybsoft MacroDrive" "Equivocation"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Cybsoft MacroDrive")
      (play-from-hand state :runner "Equivocation")
      (let [cyb (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from Cybsoft MacroDrive"
                           (click-card state :runner cyb))))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game {:runner {:deck [(qty "CyberSolutions Mem Chip" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "CyberSolutions Mem Chip")
    (is (= 6 (core/available-mu state)) "Gain 2 memory")))

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
    (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")
    (run-jack-out state)
    (run-on state "Archives")
    (is (= 2 (count (:hand (get-runner)))) "No cards drawn")))

(deftest demolisher
  ;; Demolisher
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["PAD Campaign"]}
                 :runner {:deck ["Demolisher"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 2)
      (play-from-hand state :runner "Demolisher")
      (let [demolisher (get-hardware state 0)]
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 3 [Credits] to trash")
        (is (= 1 (:credit (get-runner))) "Trashed for 3c and gained 1c"))))
  (testing "Trash with Imp"
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
        (is (= (:credit (get-runner)) (+ 1 credits)) "Demolisher earns a credit when trashing with Imp")))))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game {:runner {:deck [(qty "Desperado" 3)]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (is (= 3 (:credit (get-runner))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus
  ;; Dinosaurus
  (testing "Hosting a breaker with strength based on unused MU should calculate correctly"
    (do-game
      (new-game {:runner {:deck ["Adept" "Dinosaurus"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dinosaurus")
      (play-from-hand state :runner "Adept")
      (is (= 2 (core/available-mu state)) "2 MU used")
      (let [dino (get-hardware state 0)
            adpt (get-program state 0)]
        (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
        (card-ability state :runner dino 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh dino)))]
          (is (= 4 (core/available-mu state)) "0 MU used")
          (is (= 8 (:current-strength (refresh hosted-adpt))) "Adept at 8 strength hosted")))))
  (testing "Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker"
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
          (is (= 5 (:current-strength (refresh ram)))
              "Dinosaurus giving +2 strength to Battering Ram")
          ;; Trash Battering Ram
          (core/move state :runner (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
          (is (= 4 (core/available-mu state))
              "Battering Ram 2 MU not added to available MU when Battering Ram was trashed"))))))

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

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game {:corp {:deck ["Snare!"]}
               :runner {:deck ["Dorm Computer"]}})
    (play-from-hand state :corp "Snare!" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-hardware state 0)]
      (card-ability state :runner dorm 0)
      (click-prompt state :runner "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (is (zero? (count-tags state)) "Runner has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))))

(deftest dyson-fractal-generator
  ;; Dyson Fractal Generator
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Dyson Fractal Generator" "Blackstone"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Dyson Fractal Generator")
      (play-from-hand state :runner "Blackstone")
      (let [dfg (get-hardware state 0)
            bs (get-program state 0)]
        (changes-val-macro -2 (:credit (get-runner))
                           "Used 1 credit from Dyson Fractal Generator"
                           (card-ability state :runner bs 1)
                           (click-card state :runner dfg))))))

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
        (core/rez state :corp dm)
        (card-subroutine state :corp dm 0)
        (card-ability state :runner ff 0)
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (is (= 4 (:credit (get-runner))))
        (run-successful state)
        (click-prompt state :corp "Yes") ; pay 3 to fire Overwriter
        (card-ability state :runner ff 1)
        (click-prompt state :runner "Done")
        (click-prompt state :runner "Pay 0 [Credits] to trash") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-runner))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-runner)))))
        (is (empty? (get-hardware state)) "Feedback Filter trashed")))))

(deftest flame-out
  ;; Flame-out - start with 9 credits, use for hosted program, trash hosted program at end of turn when credits used
  (testing "Basic behavior"
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
  (testing "Corp turn usage"
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
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]
                        :hand ["Owl"]}
                 :runner {:deck ["Flame-out" "Mimic"]}})
      (play-from-hand state :corp "Owl" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (click-card state :runner "Mimic")
        (take-credits state :runner)
        (take-credits state :corp)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (run-on state :hq)
        (card-ability state :runner (first (:hosted (refresh fo))) 0)
        (click-prompt state :runner "Add installed program to the top of the Runner's Stack")
        (click-card state :runner fo)
        (is (= 2 (:credit (get-runner))) "Runner has not paid any credits from their credit pool")
        (take-credits state :runner)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed")))))

(deftest flip-switch
  ;; Flip Switch
  (testing "Trace reaction ability"
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
      (is (= 3 (-> (get-corp) :prompt first :base)) "Base trace should be base 3")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (= 1 (count-tags state)) "Runner should gain a tag from not beating trace")
      (take-credits state :corp)
      (run-on state "HQ")
      (let [ip (get-ice state :hq 0)]
        (core/rez state :corp ip)
        (card-subroutine state :corp ip 0))
      (is (prompt-is-type? state :corp :waiting) "Corp should now be waiting on Runner for Flip Switch")
      (click-prompt state :runner "Yes")
      (is (zero? (-> (get-corp) :prompt first :base)) "Base trace should now be 0")
      (is (find-card "Flip Switch" (:discard (get-runner))) "Flip Switch should be in Heap")
      (click-prompt state :corp "0")
      (click-prompt state :runner "3")
      (is (= 1 (count-tags state)) "Runner should not gain a tag from beating trace")))
  (testing "Jack out ability"
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
  (testing "Tag losing ability"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 5)]}
                 :runner {:hand ["Flip Switch"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Flip Switch")
      (is (zero? (count-tags state)) "Runner starts with 0 tags")
      (let [flip (get-hardware state 0)]
        (card-ability state :runner flip 1)
        (is (refresh flip) "Flip Switch hasn't been trashed")
        (core/gain-tags state :runner 1)
        (is (= 1 (count-tags state)) "Runner starts with 0 tags")
        (card-ability state :runner flip 1)
        (is (zero? (count-tags state)) "Runner has lost 1 tag")
        (is (nil? (refresh flip)) "Flip Switch has been trashed")
        (is (find-card "Flip Switch" (:discard (get-runner))))))))

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
      (run-on state "Server 1")
      (run-successful state)
      (click-prompt state :runner "Pay 3 [Credits] to trash") ; trash Adonis Campaing
      (click-prompt state :runner "Yes") ; gain virus counter
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip gains a counter on trash")
      (is (zero? (get-counters (refresh aum) :virus)) "Aumakua doesn't gain a counter")
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip doesn't gain a counter on non-trash")
      (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gains a counter on non-trash")
      (take-credits state :runner)
      (take-credits state :corp)
      (click-card state :runner aum)
      (is (= 2 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
      (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip lost 1 counter"))))

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
    (new-game {:corp {:deck ["Pup" "Neural Katana"]}
               :runner {:id "Apex: Invasive Predator"
                        :deck [(qty "Heartbeat" 2) (qty "Sure Gamble" 2) "Cache"]}})
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Neural Katana" "R&D")
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
    (play-from-hand state :runner "Heartbeat")
    (is (= 5 (core/available-mu state)) "Gained 1 MU")
    (play-from-hand state :runner "Cache")
    (let [hb (get-hardware state 0)
          cache (get-program state 0)
          hbdown (get-runner-facedown state 0)
          pup (get-ice state :hq 0)
          nk (get-ice state :rd 0)]
      (core/rez state :corp pup)
      (core/rez state :corp nk)
      (card-subroutine state :corp (refresh pup) 0)
      (click-prompt state :runner "Suffer 1 net damage")
      (is (= (-> (get-runner) :prompt first :msg)
             "Prevent any of the 1 net damage?")
          "Damage prevention message correct.")
      (card-ability state :runner hb 0)
      (click-card state :runner cache)
      (is (= 1 (count (:discard (get-runner)))) "Prevented 1 net damage")
      (is (= 2 (count (:hand (get-runner)))))
      (is (second-last-log-contains? state "Runner trashes 1 installed card \\(Cache\\) to use Heartbeat to prevent 1 damage\\."))
      (card-subroutine state :corp (refresh nk) 0)
      (is (= (-> (get-runner) :prompt first :msg)
             "Prevent any of the 3 net damage?")
          "Damage prevention message correct.")
      (card-ability state :runner hb 0)
      (click-card state :runner hbdown)
      (is (= (-> (get-runner) :prompt first :msg)
             "Prevent any of the 3 net damage? (1/3 prevented)")
          "Damage prevention message correct.")
      (click-prompt state :runner "Done")
      (is (= 4 (count (:discard (get-runner)))) "Prevented 1 of 3 net damage; used facedown card")
      (is (last-n-log-contains? state 2 "Runner trashes 1 installed card \\(a facedown card\\) to use Heartbeat to prevent 1 damage\\.")))))

(deftest hijacked-router
  ;; Hijacked Router
  (testing "Run on Archives"
    (do-game
      (new-game {:runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp ends turn with 8 credits")
      (play-from-hand state :runner "Hijacked Router")
      (run-empty-server state :archives)
      (is (not-empty (get-hardware state)) "Hijacked Router installed")
      (is (-> (get-runner) :prompt first :card :title (= "Hijacked Router")) "Prompt for using Hijacked Router")
      (click-prompt state :runner "Yes")
      (is (empty? (get-hardware state)) "Hijacked Router is not installed")
      (is (find-card "Hijacked Router" (:discard (get-runner))) "Hijacked Router was trashed")
      (is (= 5 (:credit (get-corp))) "Corp lost 3 credits")
      (is (not (:run @state)) "Run is finished")))
  (testing "Run on HQ"
    (do-game
      (new-game {:runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (is (= 8 (:credit (get-corp))) "Corp ends turn with 8 credits")
      (play-from-hand state :runner "Hijacked Router")
      (run-empty-server state :hq)
      (is (not-empty (get-hardware state)) "Hijacked Router installed")
      (is (-> (get-runner) :prompt first :card :title (= "Hedge Fund")) "No prompt to use Hijacked Router")
      (is (not-empty (get-hardware state)) "Hijacked Router is installed")
      (is (not (find-card "Hijacked Router" (:discard (get-runner)))) "Hijacked Router was not trashed")
      (is (= 8 (:credit (get-corp))) "Corp has not lost 3 credits")))
  (testing "Credit loss on server creation"
    (do-game
      (new-game {:corp {:deck ["Elective Upgrade"]}
                 :runner {:deck ["Hijacked Router"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hijacked Router")
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))) "Corp starts turn with 8 credits")
      (play-from-hand state :corp "Elective Upgrade" "New remote")
      (is (= 7 (:credit (get-corp))) "Corp lost 1 credit from server creation"))))

(deftest hippo
  ;; Hippo - remove from game to trash outermost piece of ice if all subs broken
  (testing "No ice"
    (do-game
      (new-game {:runner {:deck ["Hippo"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (empty? (:rfg (get-runner))) "Hippo not RFGed")
      (is (not-empty (get-hardware state)) "Hippo still installed")))
  (testing "Single ice"
    (do-game
      (new-game {:corp {:deck ["Ice Wall"]}
                 :runner {:deck ["Hippo"]}})
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 1 (count (get-ice state :hq))) "Ice Wall installed")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (empty? (get-ice state :hq)) "Ice Wall removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall trashed")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed")))
  (testing "Multiple ice"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "Enigma"]}
                 :runner {:deck ["Hippo"]}})
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 2 (count (get-ice state :hq))) "2 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall outermost")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma innermost")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (= 1 (count (get-ice state :hq))) "Ice removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice trashed")
      (is (= "Ice Wall" (:title (first (:discard (get-corp))))) "Ice Wall in trash")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma still innermost")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed"))))

(deftest knobkierie
  ;; Knobkierie - first successful run, place a virus counter on a virus program
  (do-game
    (new-game {:runner {:deck ["Knobkierie" "Hivemind" "Eater"]}})
    (core/gain state :runner :credit 20)
    (take-credits state :corp)
    (play-from-hand state :runner "Knobkierie")
    (play-from-hand state :runner "Eater")
    (run-on state "HQ")
    (run-successful state)
    (click-prompt state :runner "No action")
    (is (empty? (:prompt (get-runner))) "No prompt if not virus program installed")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Hivemind")
    (let [hv (find-card "Hivemind" (get-program state))]
      (is (= 1 (get-counters (refresh hv) :virus)) "Hivemind starts with 1 virus counters")
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "Yes") ; gain virus counter
      (click-card state :runner (find-card "Hivemind" (get-program state)))
      (click-prompt state :runner "No action")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind gains a counter on successful run")
      (run-on state "HQ")
      (run-successful state)
      (click-prompt state :runner "No action")
      (is (empty? (:prompt (get-runner))) "No prompt after first run")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind doesn't gain a counter after first run"))))

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
      (is (= 2 (:current-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (:current-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :runner)
      (is (= 1 (:current-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (:current-strength (refresh pass))) "Strength reduced to default"))))

(deftest lockpick
  ;; Lockpick
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Lockpick" "Refractor"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Lockpick")
      (play-from-hand state :runner "Refractor")
      (let [lp (get-hardware state 0)
            refr (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Lockpick"
                           (card-ability state :runner refr 1)
                           (click-card state :runner lp))))))

(deftest lucky-charm
  (testing "No interference with runs ending successfully or by jacking out, and Batty/normal ETR/Border Control interaction"
    (do-game
     (new-game {:runner {:deck [(qty "Lucky Charm" 3)]}
                :corp {:deck ["Ice Wall" "Border Control" "Marcus Batty"]}})
     (play-from-hand state :corp "Border Control" "Server 1")
     (play-from-hand state :corp "Ice Wall" "New remote")
     (play-from-hand state :corp "Marcus Batty" "Server 1")
     (core/gain state :corp :credit 20)
     (take-credits state :corp)
     (core/gain state :runner :click 100)
     (play-from-hand state :runner "Lucky Charm")
     (run-on state "HQ")
     (run-successful state)
     (is (empty? (:prompt (get-runner))) "No prompt messing with runner when run naturally ends successfully")
     (doseq [serv ["Archives" "HQ" "R&D" "Server 1"]]
       (run-on state serv)
       (is (:run @state) "Run is ongoing")
       (run-jack-out state)
       (is (empty? (:prompt (get-runner))) (str "No prompt messing with runner when jacking out from run on " serv))
       (is (not (:run @state)) "Run is finished"))
     (doseq [serv ["Archives" "HQ"]]    ; R&D, server 1 has cards
       (run-on state serv)
       (is (:run @state) "Run is ongoing")
       (run-successful state)
       (is (not (:run @state)) "Run is ended")
       (is (empty? (:prompt (get-runner))) (str "No prompt messing with runner on a successful run on " serv)))
     (run-on state "Server 1")
     (run-continue state)
     (run-continue state)
     (run-successful state)
     (click-prompt state :runner "No action") ;access batty
     (is (not (:run @state)) "Run is ended")
     (is (empty? (:prompt (get-runner))) "No prompt messing with runner on a successful run on remote")
     (let [bc (get-ice state :remote1 0)
           iw (get-ice state :remote1 1)
           mb (get-content state :remote1 0)]
       (core/rez state :corp (refresh iw))
       (core/rez state :corp (refresh bc))
       (core/rez state :corp (refresh mb))
       ;; run into ice wall, have it ETR, do not use lucky charm
       (run-on state "Server 1")
       (card-subroutine state :corp (refresh iw) 0)
       (is (:run @state) "Run not ended yet")
       (is (:prompt (get-runner)) "Runner prompted to ETR")
       (click-prompt state :runner "Done")
       (is (not (:run @state)) "Run ended yet")
       (is (empty? (:prompt (get-runner))) "Prevent prompt gone")
       ;; run into border control, have its subroutine ETR, do use lucky charm
       (run-on state "Server 1")
       (run-continue state)
       (card-subroutine state :corp (refresh bc) 1)
       (is (:run @state) "Run not ended yet")
       (is (:prompt (get-runner)) "Runner prompted to ETR")
       (card-ability state :runner (get-hardware state 0) 0)
       (click-prompt state :runner "Done")
       (is (= 1 (count (:rfg (get-runner)))) "Lucky Charm RFGed")
       (is (:run @state) "Run prevented from ending")
       (is (empty? (:prompt (get-runner))) "Prevent prompt gone")
       ;; trigger border control
       (play-from-hand state :runner "Lucky Charm")
       (card-ability state :corp (refresh bc) 0)
       (is (= 1 (count (:discard (get-corp)))) "Border Control trashed")
       (is (:run @state) "Run not ended yet")
       (is (:prompt (get-runner)) "Runner prompted to ETR")
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
       (is (:prompt (get-runner)) "Runner prompted to ETR")
       (card-ability state :runner (get-hardware state 0) 0)
       (click-prompt state :runner "Done")
       (is (:run @state) "Run prevented from ending")))))


(deftest mache
  ;; Mâché
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Imp" "Mâché" "Cache"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (starting-hand state :runner ["Imp" "Mâché"])
      (play-from-hand state :runner "Imp")
      (play-from-hand state :runner "Mâché")
      (let [imp (get-program state 0)
            mache (get-hardware state 0)
            counters (get-counters (refresh mache) :power)
            hand (-> (get-runner) :hand count)]
        (run-empty-server state :hq)
        (click-prompt state :runner "[Imp] Hosted virus counter: Trash card")
        (is (= counters (get-counters (refresh mache) :power)) "Mache should gain no counters from trashing a card with no trash cost")
        (run-empty-server state :remote1)
        (click-prompt state :runner "Pay 4 [Credits] to trash")
        (is (= (+ counters 4) (get-counters (refresh mache) :power)) "Mache should gain 4 counters for trashing a card with a trash cost of 4")
        (card-ability state :runner mache 0)
        (is (= (inc hand) (-> (get-runner) :hand count)) "Runner should draw one card for using Mache's ability")
        (is (= 1 (get-counters (refresh mache) :power)) "Mache ability should cost 3 counters"))))
  (testing "with Political Operative"
    (do-game
      (new-game {:corp {:deck ["Ice Wall" "PAD Campaign"]}
                 :runner {:deck ["Mâché" "Political Operative" "Cache"]}})
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
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
        (is (zero? (get-counters (refresh mache) :power)) "Mache should gain no counters from a trash outside of an access")))))

(deftest masterwork-v37
  ;; Masterwork (v37)
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 10)]
                          :hand ["Masterwork (v37)" "Acacia" "Capstone"]
                          :credits 10}})
      (take-credits state :corp)
      (play-from-hand state :runner "Masterwork (v37)")
      (is (= 3 (count (:hand (get-runner)))) "Runner should draw a card for playing a hardware")
      (run-on state "HQ")
      (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Corp should be waiting on Runner")
      (let [credits (:credit (get-runner))]
        (click-prompt state :runner "Yes")
        (is (= credits (:credit (get-runner))) "Runner shouldn't spend any credits until hardware is actually installed")
        (click-card state :runner "Acacia")
        (is (= (- credits 2) (:credit (get-runner))) "Runner should spend 1 for Masterwork and 1 for Acacia"))
      (is (empty? (:prompt (get-corp))) "Corp shouldn't be waiting anymore")
      (is (empty? (:prompt (get-runner))))
      (run-successful state)
      (click-prompt state :runner "No action")
      (run-on state "HQ")
      (is (= :waiting (-> (get-corp) :prompt first :prompt-type)) "Corp should be waiting on Runner")
      (is (seq (:prompt (get-runner))) "Runner should get a prompt every run")))
  (testing "Should only grant bonus on the first Hardware install. Issue #4097"
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
  (testing "Interactions with multiple triggers. Issue #4256"
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
      (card-ability state :runner (get-resource state 0) 0)
      (is (= 5 (count (:hand (get-runner)))) "Starts with 5 cards in hand")
      (is (= "Street Peddler" (:title (:card (first (:prompt (get-runner)))))))
      (click-prompt state :runner "Clone Chip")
      (is (= 5 (count (:hand (get-runner)))) "Geist's draw triggers The Class Act")
      (is (= "The Class Act" (:title (:card (first (:prompt (get-runner)))))))
      (click-prompt state :runner "Clone Chip")
      (is (= 6 (count (:hand (get-runner)))) "Geist draw finishes")
      (is (= "Choose a trigger to resolve" (:msg (first (:prompt (get-runner))))))
      (click-prompt state :runner "Masterwork (v37)")
      (is (= 7 (count (:hand (get-runner)))) "Masterwork draw")
      (is (= "Hayley Kaplan: Universal Scholar" (:title (:card (first (:prompt (get-runner)))))))
      (click-prompt state :runner "Yes")
      (click-card state :runner (find-card "Clone Chip" (:hand (get-runner))))
      (is (= 6 (count (:hand (get-runner)))) "Hayley install")
      (is (empty? (:prompt (get-runner)))))))

(deftest maui
  ;; Maui (Māui)
  (testing "Pay-credits prompt"
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
        (is (empty? (:prompt (get-runner))) "Not enough money to pay for Inti pump")
        (run-on state "HQ")
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credits from Maui"
                           (card-ability state :runner inti 1)
                           (click-card state :runner maui)
                           (click-card state :runner maui))))))

(deftest maw
  ;; Maw - Once per turn, first time runner declines to steal or trash, trash a HQ card at random
  (testing "Basic test"
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
  (testing "Check trashed card is trashed face-up if it's the card that is accessed, issue #2695"
    ;; Also checks Maw auto-trashes on Operation with no trash cost
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
  (testing "with Hiro in hand - Hiro not moved to runner scored area on trash decline. #2638"
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
  (testing "Maw shouldn't trigger on stolen agenda. #3433"
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
  (testing "Maw shouldn't trigger when accessing a card in archives. #3388"
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
      (click-prompt state :corp (find-card "Ice Wall" (:hand (get-corp)))) ;; Alice's ability
      (click-prompt state :runner "Cyberdex Virus Suite")
      (click-prompt state :corp "Yes")
      (run-empty-server state :rd)
      (click-prompt state :runner "Pay 1 [Credits] to trash")
      (is (= 3 (count (:discard (get-corp)))) "Ice Wall, CVS, and Rashida")
      (is (empty? (:prompt (get-runner))) "No more prompts for runner")))
  (testing "Maw should trigger when declining to steal. #3388"
    (do-game
      (new-game {:corp {:deck [(qty "Obokata Protocol" 2) (qty "Ice Wall" 4)]}
                 :runner {:id "Alice Merchant: Clan Agitator"
                          :deck ["Maw" "Archives Interface"]}})
      (trash-from-hand state :corp "Ice Wall")
      (starting-hand state :corp ["Obokata Protocol" "Obokata Protocol"])
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Archives Interface")
      (run-empty-server state :archives)
      (click-prompt state :corp (find-card "Obokata Protocol" (:hand (get-corp))))
      (click-prompt state :runner "Yes")
      (click-prompt state :runner (find-card "Ice Wall" (:discard (get-corp))))
      (click-prompt state :runner "No action")
      (run-empty-server state :hq)
      (click-prompt state :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "Ice Wall and Obokata"))))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2) (qty "Snare!" 2) "Hostile Takeover" "Scorched Earth"]}
                 :runner {:deck ["Maya" (qty "Sure Gamble" 3)]}})
      (core/move state :corp (find-card "Hostile Takeover" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (play-from-hand state :runner "Maya")
      (let [maya (get-hardware state 0)
            accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :runner maya 0)
        (is (empty? (:prompt (get-runner))) "No more prompts for runner")
        (is (not (:run @state)) "Run is ended")
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
        (take-credits state :runner)
        (core/draw state :corp)
        (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
        (take-credits state :corp)
        (let [accessed (first (:deck (get-corp)))]
          (run-empty-server state :rd)
          (click-prompt state :corp "Yes")
          (is (zero? (count (:hand (get-runner)))) "Runner took Snare! net damage")
          (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
          (card-ability state :runner maya 0)
          (is (empty? (:prompt (get-runner))) "No more prompts for runner")
          (is (not (:run @state)) "Run is ended")
          (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")))))
  (testing "Does not interrupt multi-access"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)]}
                 :runner {:deck ["Maya" (qty "Sure Gamble" 3) "R&D Interface"]}})
      (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Maya")
      (play-from-hand state :runner "R&D Interface")
      (let [maya (get-hardware state 0)
            accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (click-prompt state :runner "Card from deck")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :runner maya 0)
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
        (is (:prompt (get-runner)) "Runner has next access prompt")))))

(deftest mind-s-eye
  ;; Mind's Eye - Gain power tokens on R&D runs, and for 3 tokens and a click, access the top card of R&D
  (testing "Interaction with RDI + Aeneas"
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
            (click-prompt state :runner "Card from deck")
            (click-prompt state :runner "No action")
            (click-prompt state :runner "Yes")) ;Aeneas
          (is (= (+ num-creds 2) (:credit (get-runner))) "Runner has gained 2 from Aeneas"))))))

(deftest net-ready-eyes
  ;; Net-Ready Eyes
  (testing "Basic test"
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
        (is (= 3 (:current-strength (refresh pea))) "Peacock strength boosted")
        (run-continue state)
        (run-successful state)
        (click-prompt state :runner "No action")
        (is (= 2 (:current-strength (refresh pea))) "Peacock strength back to default"))))
  (testing "Do not display prompt without an installed icebreaker"
    (do-game
      (new-game {:runner {:deck [(qty "Sure Gamble" 3) (qty "Net-Ready Eyes" 2)]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Net-Ready Eyes")
      (is (= 3 (count (:discard (get-runner)))) "Took 2 damage on NRE install")
      (run-on state :hq)
      (is (empty? (:prompt (get-runner))) "No NRE prompt"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Obelus" "Nerve Agent"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]}})
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus" "Nerve Agent"])
      (core/gain state :runner :credit 10 :click 3)
      (play-from-hand state :runner "Nerve Agent")
      (let [nerve (get-program state 0)]
        (run-empty-server state :hq)
        (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
        (click-prompt state :runner "No action")
        (play-from-hand state :runner "Obelus")
        (core/gain-tags state :runner 1)
        (is (= 6 (hand-size :runner)) "Max hand size is 6")
        (core/lose-tags state :runner 1)
        (is (= 5 (hand-size :runner)) "Max hand size is 5")
        (run-empty-server state :hq)
        (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
        (click-prompt state :runner "1")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (is (empty? (:hand (get-runner))) "No cards drawn by Obelus, already had successful HQ run")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
        (click-prompt state :runner "2")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (click-prompt state :runner "Card from hand")
        (click-prompt state :runner "No action")
        (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards"))))
  (testing "running and trashing Crisium Grid makes run neither successful/unsuccessful"
    (do-game
      (new-game {:corp {:deck ["Hedge Fund" "Crisium Grid"]}
                 :runner {:deck ["Obelus" (qty "Sure Gamble" 3)]}})
      (starting-hand state :corp ["Crisium Grid"])
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (core/rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus"])
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "R&D")
      (click-prompt state :runner "Crisium Grid")
      (click-prompt state :runner "Pay 5 [Credits] to trash")
      (click-prompt state :runner "Card from deck")
      (click-prompt state :runner "No action")
      (is (empty? (:hand (get-runner))) "Crisium Grid blocked successful run")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run")))
  (testing "using Hades Shard during run to increase draw"
    (do-game
      (new-game {:corp {:deck [(qty "Hedge Fund" 3) (qty "Restructure" 3)]}
                 :runner {:deck ["Obelus" "Hades Shard"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]}})
      (starting-hand state :corp ["Hedge Fund" "Hedge Fund"])
      (trash-from-hand state :corp "Hedge Fund")
      (trash-from-hand state :corp "Hedge Fund")
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus" "Hades Shard"])
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Obelus")
      (play-from-hand state :runner "Hades Shard")
      (run-empty-server state "R&D")
      (card-ability state :runner (get-resource state 0) 0)
      (click-prompt state :runner "No action")
      (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards")))
  (testing "running a remote server first doesn't block card draw"
    (do-game
      (new-game {:corp {:deck ["Urban Renewal" "Hedge Fund"]}
                 :runner {:deck ["Obelus" (qty "Sure Gamble" 3)]}})
      (starting-hand state :corp ["Urban Renewal"])
      (play-from-hand state :corp "Urban Renewal" "New remote")
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus"])
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "Server 1")
      (click-prompt state :runner "No action")
      (run-empty-server state "R&D")
      (click-prompt state :runner "No action")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run")))
  (testing "works with Paper Tripping"
    (do-game
      (new-game {:runner {:deck ["Obelus" "Paper Tripping"]}})
      (take-credits state :corp)
      (core/gain-tags state :runner 3)
      (is (= 3 (count-tags state)) "Runner starts with 3 tags")
      (play-from-hand state :runner "Obelus")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Paper Tripping")
      (is (zero? (count-tags state)) "Runner loses all tags"))))

(deftest omni-drive
    ;;Omni-drive
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:hand ["Omni-drive" "Inti"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Omni-drive")
      (let [omni (get-hardware state 0)]
          (card-ability state :runner omni 0)
          (click-card state :runner (find-card "Inti" (:hand (get-runner))))
          (let [inti (first (:hosted (refresh omni)))]
            (changes-val-macro -1 (:credit (get-runner))
                               "Used 1 credit from Omni-drive"
                               (card-ability state :runner inti 1)
                               (click-card state :runner omni)))))))

(deftest paragon
  ;; Paragon - Gain 1 credit and may look at and move top card of Stack to bottom
  (testing "Vanilla test"
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
     (let [top-cid (:cid (first (:deck (get-runner))))]
       (click-prompt state :runner "Yes")
       (is (= top-cid (:cid (last (:deck (get-runner))))) "Moved top card to bottom"))
     (run-empty-server state "HQ")
     (is (not (prompt-is-card? state :runner (get-hardware state 0))) "No prompt from Paragon")))
  (testing "Autoresolve"
    (do-game
     (new-game {:runner {:deck ["Paragon" (qty "Easy Mark" 3)]}})
     (starting-hand state :runner ["Paragon"])
     (take-credits state :corp)
     (play-from-hand state :runner "Paragon")
     (letfn [(toggle-paragon [setting]
               (card-ability state :runner (get-hardware state 0) 0)
               (click-prompt state :runner setting))]
       (doseq [set-to ["Never" "Ask" "Always"]]
         (is (changes-credits (get-runner) 0
                              (do (toggle-paragon set-to)
                                  (run-empty-server state "Archives") ; on first loop this actually triggers paragon, but we say 'no'
                                  (is (empty? (:prompt (get-runner))) "No Paragon prompt")))
             "Paragon does not fire"))
       (take-credits state :runner)
       (take-credits state :corp)
       ;; paragon is now set to 'Always'
       (is (changes-credits (get-runner) 1
                              (do (run-empty-server state "Archives") ; on first loop this actually triggers paragon, but we say 'no'
                                  (click-prompt state :runner "Yes") ; prompt to add a card to bottom
                                  (is (empty? (:prompt (get-runner))) "No Paragon prompt")))
             "Paragon fires automatically")))))

(deftest patchwork
  ;; Patchwork
  (testing "Play event"
    (do-game
      (new-game {:runner {:deck ["Patchwork" (qty "Sure Gamble" 2) "Easy Mark"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (card-ability state :runner (get-hardware state 0) 0)
      (play-from-hand state :runner "Sure Gamble")
      (is (= 5 (:credit (get-runner))) "Runner has not been charged credits yet")
      (is (empty? (:discard (get-runner))) "Sure Gamble is not in heap yet")
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 11 (:credit (get-runner))) "Runner was only charge 3 credits to play Sure Gamble")
      (is (= 2 (count (:discard (get-runner)))) "2 cards now in heap")
      (play-from-hand state :runner "Sure Gamble")
      (is (= 15 (:credit (get-runner))) "Patchwork is once-per-turn")))
  (testing "Install a card"
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Easy Mark" "Cyberfeeder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (card-ability state :runner (get-hardware state 0) 0)
      (play-from-hand state :runner "Cyberfeeder")
      (is (= 5 (:credit (get-runner))) "Runner has not been charged credits yet")
      (is (empty? (:discard (get-runner))) "Easy Mark is not in heap yet")
      (click-card state :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 5 (:credit (get-runner))) "Runner was charged 0 credits to play Cyberfeeder")))
  (testing "Play event with pay-credits prompt"
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
  (testing "Install a card with pay-credits prompt"
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
  (testing "Issue #4322: Trashing same card that is being installed"
    (do-game
      (new-game {:runner {:deck ["Patchwork" "Easy Mark" "Cyberfeeder"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 4)
      (play-from-hand state :runner "Patchwork")
      (play-from-hand state :runner "Cyberfeeder")
      (click-card state :runner (get-hardware state 0))
      (click-card state :runner (find-card "Cyberfeeder" (:hand (get-runner))))
      (is (= 2 (count (:hand (get-runner)))) "Cyberfeeder is still in hand")
      (is (not-empty (:prompt (get-runner))) "Prompt still open"))))

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
      (core/gain-tags state :runner 1)
      (play-from-hand state :corp "Scorched Earth")
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (is (= 1 (count (:hand (get-runner)))) "All meat damage prevented")
      (is (empty? (get-hardware state)) "Plascrete depleted and trashed"))))

(deftest public-terminal
  ;; Public Terminal
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Public Terminal" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Public Terminal")
      (let [pt (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from Public Terminal"
                           (play-from-hand state :runner "Dirty Laundry")
                           (click-card state :runner pt))))))

(deftest prepaid-voicepad
  ;; Prepaid VoicePAD
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Prepaid VoicePAD" "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Prepaid VoicePAD")
      (let [ppvp (get-hardware state 0)]
        (changes-val-macro -1 (:credit (get-runner))
                           "Used 1 credit from "
                           (play-from-hand state :runner "Dirty Laundry")
                           (click-card state :runner ppvp))))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game {:runner {:deck ["Sure Gamble" (qty "Rabbit Hole" 3)]}})
    (take-credits state :corp)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Rabbit Hole")
    (is (= 1 (:link (get-runner))))
    (click-prompt state :runner "Yes")
    (click-prompt state :runner "Yes")
    (is (= 3 (:link (get-runner))))
    (is (= 3 (count (get-hardware state))))
    (is (= 2 (:click (get-runner))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-runner))) "Paid 2c for each of 3 copies")))

(deftest ramujan-reliant-550-bmi
  ;; Prevent up to X net or brain damage.
  (testing "Basic test"
    (do-game
      (new-game {:corp {:deck ["Data Mine" "Snare!"]}
                 :runner {:deck [(qty "Ramujan-reliant 550 BMI" 4)
                                 (qty "Sure Gamble" 6)]}})
      (starting-hand state :runner
                     ["Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Ramujan-reliant 550 BMI" "Sure Gamble"])
      (play-from-hand state :corp "Data Mine" "Server 1")
      (play-from-hand state :corp "Snare!" "Server 1")
      (let [sn (get-content state :remote1 0)
            dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)
              rr2 (get-hardware state 1)
              rr3 (get-hardware state 2)]
          (run-on state "Server 1")
          (core/rez state :corp dm)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (click-prompt state :runner "1")
          (is (last-log-contains? state "Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 2 (count (:hand (get-runner)))) "1 net damage prevented")
          (run-successful state)
          (take-credits state :runner)
          (take-credits state :corp)
          (play-from-hand state :runner "Ramujan-reliant 550 BMI")
          (run-empty-server state "Server 1")
          (click-prompt state :corp "Yes")
          (card-ability state :runner rr2 0)
          (click-prompt state :runner "3")
          (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 1 (count (:hand (get-runner)))) "3 net damage prevented")))))
  (testing "Prevent up to X net or brain damage. Empty stack"
    (do-game
      (new-game {:corp {:deck ["Data Mine"]}
                 :runner {:deck ["Ramujan-reliant 550 BMI" "Sure Gamble"]}})
      (play-from-hand state :corp "Data Mine" "Server 1")
      (let [dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)]
          (run-on state "Server 1")
          (core/rez state :corp dm)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (click-prompt state :runner "Done")
          (is (zero? (count (:hand (get-runner)))) "Not enough cards in Stack for Ramujan to work"))))))

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
    (core/draw state :runner)
    (core/draw state :runner)
    (core/draw state :runner)
    (core/draw state :runner)
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
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd1 0)
      (click-prompt state :runner "3")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "No action")
      (is (= 5 (count (:hand (get-runner)))) "Runner took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Snare!")
      (card-ability state :corp hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :runner rd2 0)
      (is (nil? (:number (:choices (first (:prompt (get-runner)))))) "No choice to prevent damage from HOK")
      (click-prompt state :runner "Done")
      (is (= 4 (count (:hand (get-runner)))) "Runner took 1 net damage from HOK")
      (click-prompt state :corp "No")
      (click-prompt state :runner "No action")
      (core/lose state :runner :credit 100)
      ; can only stop 1 damage due to credits
      (core/gain state :runner :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Snare!")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd2 0)
      (is (= 1 (:number (:choices (first (:prompt (get-runner)))))) "Recon Drone choice limited to runner credits")
      (click-prompt state :runner "1")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "Pay 0 [Credits] to trash")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 2 net damage from Snare!")
      (core/gain state :runner :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Prisec")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd3 0)
      (is (= 1 (:number (:choices (first (:prompt (get-runner)))))) "Recon Drone choice limited to 1 meat")
      (click-prompt state :runner "1")
      (click-prompt state :runner "Done")
      (click-prompt state :runner "Pay 3 [Credits] to trash")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Cerebral Overwriter")
      (click-prompt state :corp "Yes")
      (card-ability state :runner rd4 0)
      (click-prompt state :runner "1")
      (click-prompt state :runner "Done")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no brain damage"))))

(deftest replicator
  ;; Replicator
  (testing "interaction with Bazaar. Issue #1511"
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
        (is (count-spy 6) "6 Spy Cameras installed")))))

(deftest respirocytes
  (testing "Should draw multiple cards when multiple respirocytes are in play"
    (do-game
      (new-game {:runner {:deck [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]
                          :hand ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"]}})
      (take-credits state :corp)
      (dotimes [_ 2]
        (play-from-hand state :runner "Respirocytes"))
      (is (= 2 (count (:discard (get-runner)))) "2 damage done")
      (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards")))
  (testing "Respirocytes should not trigger after being trashed (issue #3699)"
    (do-game
      (new-game {:runner {:deck ["Respirocytes" (qty "Sure Gamble" 20)]
                          :hand ["Respirocytes" "Sure Gamble"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Respirocytes")
      (is (= 1 (-> (get-runner) :discard count)) "Took 1 damage from Respirocytes")
      (is (= 1 (-> (get-runner) :hand count)) "Drew 1 from Respirocytes")
      (let [respirocytes (get-hardware state 0)]
        (is (= 1 (get-counters (refresh respirocytes) :power)) "Respirocytes drew once")
        (take-credits state :runner)
        (take-credits state :corp)
        (dotimes [n 2]
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
  (testing "Should wait to draw until after draws are handled. #4190"
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
      (is (= ["Acacia" "Bankroll"] (->> (get-runner) :prompt first :choices (map :title)))
          "First Respirocytes triggers The Class Act")
      (is (zero? (count (:hand (get-runner)))) "Runner hasn't drawn anything yet")
      (click-prompt state :runner "Acacia")
      (is (= ["Bankroll" "Cache"] (->> (get-runner) :hand (map :title)))
          "Acacia is on the bottom of the deck so Runner drew Cache"))))

(deftest rubicon-switch
  ;; Rubicon Switch
  (do-game
    (new-game {:corp {:deck ["Ice Wall" "Pachinko"]}
               :runner {:deck ["Rubicon Switch"]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Pachinko" "R&D")
    (let [iw (get-ice state :hq 0)
          pach (get-ice state :rd 0)]
      (core/rez state :corp iw)
      (take-credits state :corp)
      (play-from-hand state :runner "Rubicon Switch")
      (core/rez state :corp pach)
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
               :runner {:deck ["Security Nexus"]}})
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Security Nexus")
    (let [nexus (get-hardware state 0)]
      (run-on state :rd)
      (card-ability state :runner nexus 0)
      (is (zero? (count-tags state)) "Runner should have no tags to start")
      (click-prompt state :corp "0")
      (click-prompt state :runner "0")
      (is (not (:run @state)) "Run should end from losing Security Nexus trace")
      (is (= 1 (count-tags state)) "Runner should take 1 tag from losing Security Nexus trace")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :rd)
      (card-ability state :runner nexus 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "10")
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
    (run-successful state)
    (click-prompt state :runner "Card from deck")
    (click-prompt state :runner "No action") ; First card
    (click-prompt state :runner "Card from deck")
    (click-prompt state :runner "Steal") ; Second card, due to additional access
    (is (nil? (:run @state)) "Run is over")))

(deftest sifr
  ;; Sifr - Once per turn drop encountered ICE to zero strenght
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
      (core/rez state :corp arch)
      (core/rez state :corp ip)
      (is (= 4 (:current-strength (refresh ip))))
      (run-on state :hq)
      (is (= 2 (:position (:run @state))))
      (card-ability state :runner sifr 0)
      (is (zero? (:current-strength (refresh ip))))
      (run-continue state)
      (is (= 1 (:position (:run @state))))
      (is (= 2 (count (:hand (get-runner))))) ; pre archangel
      (card-subroutine state :corp arch 0) ; fire archangel
      (is (seq (:prompt (get-corp))) "Archangel trace prompt - corp")
      (click-prompt state :corp "0")
      (is (seq (:prompt (get-runner))) "Archangel trace prompt - runner")
      (click-prompt state :runner "0")
      (click-card state :corp sifr)
      (is (= 3 (count (:hand (get-runner))))) ; sifr got lifted to hand
      (run-jack-out state)
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Modded")
      (is (seq (:prompt (get-runner))) "Modded choice prompt exists")
      (click-card state :runner (find-card "Şifr" (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Clone Chip")
      (take-credits state :runner)
      (take-credits state :corp 4)
      (let [chip (get-hardware state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (zero? (count (:discard (get-corp)))) "No Corp cards trashed")
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Parasite" (:discard (get-runner))))
        (let [para (get-program state 0)]
          (click-card state :runner ip)
          (is (zero? (count (:discard (get-corp)))) "IP Block Not Trashed")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))

(deftest silencer
  ;; Silencer
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Silencer" "Dagger"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Silencer")
      (play-from-hand state :runner "Dagger")
      (let [sil (get-hardware state 0)
            dag (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 1 credit from Silencer"
                           (card-ability state :runner dag 1)
                           (click-card state :runner sil))))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (testing "Basic test"
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
        (core/rez state :corp cad)
        (card-subroutine state :corp cad 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
        (is (= 1 (count (:discard (get-runner)))))
        (is (= 4 (hand-size :runner)) "Reduced hand size"))))
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["Spinal Modem" "Inti"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Spinal Modem")
      (play-from-hand state :runner "Inti")
      (let [sm (get-hardware state 0)
            inti (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credits from Spinal Modem"
                           (card-ability state :runner inti 1)
                           (click-card state :runner sm)
                           (click-card state :runner sm))))))

(deftest sports-hopper
  ;; Sports Hopper
  (do-game
    (new-game {:runner {:deck [(qty "Sports Hopper" 3) (qty "Sure Gamble" 3)]}})
    (starting-hand state :runner ["Sports Hopper"])
    (take-credits state :corp)
    (play-from-hand state :runner "Sports Hopper")
    (is (= 1 (:link (get-runner))) "Gained 1 link")
    (card-ability state :runner (get-hardware state 0) 0)
    (is (= 1 (count (:discard (get-runner)))))
    (is (= 3 (count (:hand (get-runner)))) "Drew 3 cards")
    (is (zero? (:link (get-runner))) "Lost link")))

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
      (let [topcard (get-in (first (get-in @state [:runner :prompt])) [:msg])]
        (is (= "The top card of R&D is Hedge Fund" topcard)))
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
  (testing "Swift gives the runner +1[mu]"
    (do-game
      (new-game {:runner {:hand ["Swift"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Swift")
      (is (= 5 (core/available-mu state)))))
  (testing "Installing Swift then playing a run event gives you a click back"
    (do-game
      (new-game {:runner {:hand ["Swift", "Dirty Laundry"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Swift")
      (play-from-hand state :runner "Dirty Laundry")
      (is (= 3 (get-in @state [:runner :click]))))))

(deftest the-gauntlet
  (testing "Access additional cards on run on HQ, not with Gang Sign. Issue #2749"
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
      ;; This will throw error if The Gauntlet triggers.
      (click-prompt state :runner "Card from hand"))))

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
      (is (= 2 (:current-strength (refresh fae))))
      (play-from-hand state :runner "The Personal Touch")
      (click-card state :runner par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-icebreaker")
      (click-card state :runner fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (:current-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest the-toolbox
  ;; The Toolbox
  (testing "Pay-credits prompt"
    (do-game
      (new-game {:runner {:deck ["The Toolbox" "Inti"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 9)
      (play-from-hand state :runner "The Toolbox")
      (play-from-hand state :runner "Inti")
      (let [tt (get-hardware state 0)
            inti (get-program state 0)]
        (changes-val-macro 0 (:credit (get-runner))
                           "Used 2 credits from The Toolbox"
                           (card-ability state :runner inti 1)
                           (click-card state :runner tt)
                           (click-card state :runner tt))))))

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
    (is (empty? (:prompt (get-runner))) "Fall Guy didn't try to prevent trashing of Kati")
    (is (= 2 (count (:discard (get-runner)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)]
      (core/rez state :corp pup)
      (card-subroutine state :corp pup 0)
      (click-prompt state :runner "Suffer 1 net damage")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner)))) ; Ribs takes precedence over CP on Runner turn
      (is (= 3 (count (:discard (get-runner)))) "Chose card lost from 1 net damage")
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
  (testing "Basic test"
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
      (run-empty-server state :rd)
      (click-prompt state :runner "Yes") ;Top Hat Prompt
      (click-prompt state :runner "4") ;select ABT
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))) "Runner stole DNN")))
  (testing "Ash interaction"
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
        (core/rez state :corp ash)
        (take-credits state :corp)
        (core/gain state :runner :click 100)
        (core/gain state :runner :credit 100)
        (play-from-hand state :runner "Top Hat")
        (run-empty-server state :rd)
        (click-prompt state :runner "Yes") ;Top Hat Prompt
        (click-prompt state :corp "0") ;init Ash trace <<< fails here
        (click-prompt state :runner "0") ;lose Ash trace
        (is (empty? (:prompt (get-runner))) "No prompt to access cards.")))))

(deftest turntable
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (testing "Basic test"
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
  (testing "vs Mandatory Upgrades"
    ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
    ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
    (do-game
      (new-game {:corp {:deck [(qty "Mandatory Upgrades" 2) "Project Vitruvius"]}
                 :runner {:deck ["Turntable"]}})
      (score-agenda state :corp (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (let [tt (get-hardware state 0)]
        ;; steal Project Vitruvius and swap for Mandatory Upgrades
        (core/steal state :runner (find-card "Project Vitruvius" (:hand (get-corp))))
        (is (prompt-is-card? state :runner tt))
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Mandatory Upgrades" (:scored (get-corp))))
        (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn")
        ;; steal second Mandatory Upgrades and swap for Project Vitruvius
        (core/steal state :runner (find-card "Mandatory Upgrades" (:hand (get-corp))))
        (is (prompt-is-card? state :runner tt))
        (click-prompt state :runner "Yes")
        (click-card state :runner (find-card "Project Vitruvius" (:scored (get-corp))))
        (is (zero? (:click (get-corp))) "Corp doesn't gain a click on Runner's turn")
        (is (= 4 (:click-per-turn (get-corp))))))))

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

(deftest zer0
  ;; Zer0
  (testing "Basic ability"
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
  (testing "With Titanium Ribs"
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
  (testing "With Respirocytes"
    (do-game
      (new-game {:runner {:deck ["Zer0" "Titanium Ribs" "Respirocytes"(qty "Sure Gamble" 7)]}})
      (starting-hand state :runner ["Zer0" "Titanium Ribs" "Respirocytes" "Sure Gamble" "Sure Gamble" "Sure Gamble" "Sure Gamble"])
      (take-credits state :corp)
      (play-from-hand state :runner "Zer0")
      (play-from-hand state :runner "Titanium Ribs")
      (click-card state :runner (second (:hand (get-runner))))
      (click-card state :runner (nth (:hand (get-runner)) 2))
      (play-from-hand state :runner "Respirocytes")
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner))))
      ;; Now 1 Gamble in hand
      (is (= 3 (:credit (get-runner))) "Runner has 3 credits")
      (let  [z (get-hardware state 0)]
        (card-ability state :runner z 0)
        (is (= 3 (:credit (get-runner))) "Zer0 has not yet resolved because Ribs prompt is open")
        (is (= 1 (count (:hand (get-runner)))) "Zer0 has not yet resolved because Ribs prompt is open")
        (click-card state :runner (first (:hand (get-runner))))
        (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
        (is (= 3 (count (:hand (get-runner)))) "Runner has 3 cards")))))
