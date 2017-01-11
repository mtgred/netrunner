(ns test.cards.hardware
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest akamatsu-mem
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Akamatsu Mem Chip" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game (default-corp [(qty "Shock!" 1) (qty "Launch Campaign" 1)])
              (default-runner [(qty "Archives Interface" 1) (qty "Imp" 1)]))
    (take-credits state :corp)
    (core/move state :corp (find-card "Shock!" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "Launch Campaign" (:hand (get-corp))) :discard)
    (play-from-hand state :runner "Archives Interface")
    (run-empty-server state :archives)
    (prompt-choice :runner "Yes")
    (prompt-choice :runner (find-card "Shock!" (:discard (get-corp))))
    (is (= "Shock!" (:title (first (:rfg (get-corp))))) "Shock! removed from game")
    (is (empty? (:discard (get-runner))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe-memory
  ;; Astrolabe - Gain 1 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Astrolabe" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest astrolabe-draw
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game (default-corp [(qty "Snare!" 3)])
              (default-runner [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) (qty "Cloak" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner 3)
    ;; corp's turn. install something from HQ to trigger Astrolabe draw
    (play-from-hand state :corp "Snare!" "New remote")
    (is (= 5 (count (:hand (get-runner)))) "Drew 1 card from server install")
    ;; install over the old server; make sure nothing is drawn
    (play-from-hand state :corp "Snare!" "Server 0")
    (is (= 5 (count (:hand (get-runner)))) "Did not draw")
    (is (= 1 (count (:deck (get-runner)))) "1 card left in deck")))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Box-E" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Box-E")
   (is (= 6 (:memory (get-runner))))
   (is (= 7 (core/hand-size state :runner)))))

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Blackguard" 1)
                              (qty "Snitch" 1)]))
   (play-from-hand state :corp "Ice Wall" "Archives")
   (take-credits state :corp)
   (core/gain state :runner :credit 100)
   (play-from-hand state :runner "Blackguard")
   (is (= 6 (:memory (get-runner))) "Runner has 6 MU")
   (play-from-hand state :runner "Snitch")
   (let [snitch (get-in @state [:runner :rig :program 0])
         iwall (get-ice state :archives 0)]
     (run-on state :archives)
     (card-ability state :runner snitch 0)
     (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Brain Chip" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Brain Chip")
   (swap! state assoc-in [:runner :agenda-point] -2) ; hard set ap
   (is (= (core/hand-size state :runner) 5) "Hand size unaffected")
   (is (= (get-in @state [:runner :memory]) 4) "Memory limit unaffected")
   (swap! state assoc-in [:runner :agenda-point] 2)
   (is (= (core/hand-size state :runner) 7) "Hand size increased by 2")
   (is (= (get-in @state [:runner :memory]) 6) "Memory limit increased by 2")
   (core/move state :runner (get-in @state [:runner :rig :hardware 0]) :discard)
   (is (= (core/hand-size state :runner) 5) "Hand size reset")
   (is (= (get-in @state [:runner :memory]) 4) "Memory limit reset")))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Datasucker" 1) (qty "Clone Chip" 2)]))
    (take-credits state :corp)
    (trash-from-hand state :runner "Datasucker")
    (play-from-hand state :runner "Clone Chip")
    (let [chip (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip 0)
      (prompt-select :runner (find-card "Datasucker" (:discard (get-runner))))
      (let [ds (get-in @state [:runner :rig :program 0])]
        (is (not (nil? ds)))
        (is (= (:title ds) "Datasucker"))))))

(deftest comet-event-play
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Comet")
    (let [comet (get-in @state [:runner :rig :hardware 0])]
      (play-from-hand state :runner "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :runner comet 0)
      (is (= (:cid comet) (-> @state :runner :prompt first :card :cid)))
      (prompt-select :runner (find-card "Easy Mark" (:hand (get-runner))))
      (is (= 7 (:credit (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest cortez-chip
  ;; Cortez Chip - Trash to add 2 credits to rez cost of an ICE until end of turn
  (do-game
    (new-game (default-corp [(qty "Quandary" 1)])
              (default-runner [(qty "Cortez Chip" 1)]))
    (play-from-hand state :corp "Quandary" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Cortez Chip")
    (let [quan (get-ice state :rd 0)
          cortez (get-hardware state 0)]
      (card-ability state :runner cortez 0)
      (prompt-select :runner quan)
      (is (= 1 (count (:discard (get-runner)))) "Cortez Chip trashed")
      (core/rez state :corp quan)
      (is (= 4 (:credit (get-corp))) "Paid 3c instead of 1c to rez Quandary"))))

(deftest cybersolutions-mem-chip
  ;; CyberSolutions Mem Chip- Gain 2 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "CyberSolutions Mem Chip" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "CyberSolutions Mem Chip")
    (is (= 6 (:memory (get-runner))) "Gain 2 memory")))

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Desperado" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")
    (is (= 3 (:credit (get-runner))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus-strength-boost-mu-savings
  ;; Dinosaurus - Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Dinosaurus" 1) (qty "Battering Ram" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Dinosaurus")
    (let [dino (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner dino 0)
      (prompt-select :runner (find-card "Battering Ram" (:hand (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (= 0 (:credit (get-runner))))
      (is (= 4 (:memory (get-runner))) "Battering Ram 2 MU not deducted from available MU")
      (let [ram (first (:hosted (refresh dino)))]
        (is (= 5 (:current-strength (refresh ram)))
            "Dinosaurus giving +2 strength to Battering Ram")
        ;; Trash Battering Ram
        (core/move state :runner (find-card "Battering Ram" (:hosted (refresh dino))) :discard)
        (is (= 4 (:memory (get-runner))) "Battering Ram 2 MU not added to available MU")))))

(deftest doppelganger
  ;; Doppelgänger - run again when successful
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Doppelgänger" 1)]))
    (core/gain state :corp :bad-publicity 1)
    (take-credits state :corp)
    (play-from-hand state :runner "Doppelgänger")
    (run-empty-server state :hq)
    (prompt-choice :runner "OK")
    (is (= 0 (:run-credit (get-runner))) "Runner lost BP credits")
    (prompt-choice :runner "Yes")
    (prompt-choice :runner "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game (default-corp [(qty "Snare!" 1)])
              (default-runner [(qty "Dorm Computer" 1)]))
    (play-from-hand state :corp "Snare!" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner dorm 0)
      (prompt-choice :runner "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :corp "Yes")
      (is (= 0 (:tag (get-runner))) "Runner has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))))

(deftest snare
  ;; pay 4 on access, and do 3 net damage and give 1 tag
  (do-game
    (new-game (default-corp [(qty "Snare!" 1)])
              (default-runner [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Snare!" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (is (= :waiting (-> @state :runner :prompt first :prompt-type))
        "Runner has prompt to wait for Snare!")
    (prompt-choice :corp "Yes")
    (is (= 3 (:credit (get-corp))) "Corp had 7 and paid 4 for Snare! 3 left")
    (is (= 1 (:tag (get-runner))) "Runner has 1 tag")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 3 net damage")
    ))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game (default-corp [(qty "Data Mine" 1)
                             (qty "Cerebral Overwriter" 1)
                             (qty "Mushin No Shin" 1)])
              (default-runner [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Mushin No Shin")
    (prompt-select :corp (find-card "Cerebral Overwriter" (:hand (get-corp))))
    (play-from-hand state :corp "Data Mine" "Server 1")
    (let [co (get-content state :remote1 0)
          dm (get-ice state :remote1 0)]
      (is (= 3 (:advance-counter (refresh co))) "3 advancements on Overwriter")
      (take-credits state :corp)
      (play-from-hand state :runner "Sure Gamble")
      (play-from-hand state :runner "Feedback Filter")
      (is (= 7 (:credit (get-runner))))
      (let [ff (get-in @state [:runner :rig :hardware 0])]
        (run-on state "Server 1")
        (core/rez state :corp dm)
        (card-subroutine state :corp dm 0)
        (card-ability state :runner ff 0)
        (prompt-choice :runner "Done")
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (is (= 4 (:credit (get-runner))))
        (run-successful state)
        (prompt-choice :corp "Yes") ; pay 3 to fire Overwriter
        (card-ability state :runner ff 1)
        (prompt-choice :runner "Done")
        (prompt-choice :runner "Yes") ; trash Overwriter for 0
        (is (= 1 (:brain-damage (get-runner))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-runner)))))
        (is (empty? (get-in @state [:runner :rig :hardware])) "Feedback Filter trashed")))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus programs
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Grimoire" 1) (qty "Imp" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Grimoire")
    (is (= 6 (:memory (get-runner))) "Gained 2 MU")
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an icebreaker upon install
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "LLDS Processor" 2) (qty "Inti" 1) (qty "Passport" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "LLDS Processor")
    (play-from-hand state :runner "Inti")
    (play-from-hand state :runner "LLDS Processor")
    (play-from-hand state :runner "Passport")
    (let [inti (get-in @state [:runner :rig :program 0])
          pass (get-in @state [:runner :rig :program 1])]
      (is (= 2 (:current-strength (refresh inti))) "Strength boosted by 1; 1 copy of LLDS when installed")
      (is (= 4 (:current-strength (refresh pass))) "Strength boosted by 2; 2 copies of LLDS when installed")
      (take-credits state :runner)
      (is (= 1 (:current-strength (refresh inti))) "Strength reduced to default")
      (is (= 2 (:current-strength (refresh pass))) "Strength reduced to default"))))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-runner [(qty "Maya" 1) (qty "Sure Gamble" 3)]))
    (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    (play-from-hand state :runner "Maya")
    (let [maya (get-in @state [:runner :rig :hardware 0])
          accessed (first (:deck (get-corp)))]
      (run-empty-server state :rd)
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
      (card-ability state :runner maya 0)
      (is (empty? (:prompt (get-runner))) "No more prompts for runner")
      (is (not (:run @state)) "Run is ended")
      (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
      (take-credits state :runner)
      (core/draw state :corp)
      (take-credits state :corp)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
      (let [accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (prompt-choice :corp "Yes")
        (is (= 0 (count (:hand (get-runner)))) "Runner took Snare! net damage")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :runner maya 0)
        (is (empty? (:prompt (get-runner))) "No more prompts for runner")
        (is (not (:run @state)) "Run is ended")
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")))))

(deftest maya-multi-access
  ;; Maya - Does not interrupt multi-access.
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
              (default-runner [(qty "Maya" 1) (qty "Sure Gamble" 3) (qty "R&D Interface" 1)]))
    (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Maya")
    (play-from-hand state :runner "R&D Interface")
    (let [maya (get-in @state [:runner :rig :hardware 0])
          accessed (first (:deck (get-corp)))]
      (run-empty-server state :rd)
      (prompt-choice :runner "Card from deck")
      (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
      (card-ability state :runner maya 0)
      (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
      (is (:prompt (get-runner)) "Runner has next access prompt"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Obelus" 1) (qty "Nerve Agent" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
    (take-credits state :corp)
    (starting-hand state :runner ["Obelus" "Nerve Agent"])
    (core/gain state :runner :credit 10 :click 3)
    (play-from-hand state :runner "Nerve Agent")
    (let [nerve (get-in @state [:runner :rig :program 0])]
      (run-empty-server state :hq)
      (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
      (prompt-choice :runner "OK")
      (play-from-hand state :runner "Obelus")
      (core/gain state :runner :tag 1)
      (is (= 6 (core/hand-size state :runner)) "Max hand size is 6")
      (core/lose state :runner :tag 1)
      (is (= 5 (core/hand-size state :runner)) "Max hand size is 5")
      (run-empty-server state :hq)
      (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
      (prompt-choice :runner 1)
      (prompt-choice :runner "Card from hand")
      (prompt-choice :runner "OK")
      (prompt-choice :runner "Card from hand")
      (prompt-choice :runner "OK")
      (is (empty? (:hand (get-runner))) "No cards drawn by Obelus, already had successful HQ run")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
      (prompt-choice :runner 2)
      (prompt-choice :runner "Card from hand")
      (prompt-choice :runner "OK")
      (prompt-choice :runner "Card from hand")
      (prompt-choice :runner "OK")
      (prompt-choice :runner "Card from hand")
      (prompt-choice :runner "OK")
      (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards"))))

(deftest obelus-hades-shard
  ;; Obelus - using Hades Shard during run to increase draw
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 3)])
              (default-runner [(qty "Obelus" 1) (qty "Hades Shard" 1)
                               (qty "Sure Gamble" 3) (qty "Cache" 3)]))
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
    (prompt-choice :runner "OK")
    (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards")))

(deftest plascrete
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Plascrete Carapace" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Plascrete Carapace")
    (let [plas (get-in @state [:runner :rig :hardware 0])]
      (is (= 4 (get-counters (refresh plas) :power)) "4 counters on install")
      (take-credits state :runner)
      (core/gain state :runner :tag 1)
      (play-from-hand state :corp "Scorched Earth")
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (card-ability state :runner plas 0)
      (prompt-choice :runner "Done")
      (is (= 1 (count (:hand (get-runner)))) "All meat damage prevented")
      (is (empty? (get-in @state [:runner :rig :hardware])) "Plascrete depleted and trashed"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Sure Gamble" 1) (qty "Rabbit Hole" 3)]))
    (take-credits state :corp)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Rabbit Hole")
    (is (= 1 (:link (get-runner))))
    (prompt-choice :runner "Yes")
    (prompt-choice :runner "Yes")
    (is (= 3 (:link (get-runner))))
    (is (= 3 (count (get-in @state [:runner :rig :hardware]))))
    (is (= 2 (:click (get-runner))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-runner))) "Paid 2c for each of 3 copies")))

(deftest replicator-bazaar
  ;; Replicator - interaction with Bazaar. Issue #1511.
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Replicator" 1) (qty "Bazaar" 1) (qty "Spy Camera" 6)]))
    (letfn [(count-spy [n] (= n (count (filter #(= "Spy Camera" (:title %)) (-> (get-runner) :rig :hardware)))))]
      (take-credits state :corp)
      (starting-hand state :runner ["Replicator" "Bazaar" "Spy Camera"])
      (play-from-hand state :runner "Replicator")
      (play-from-hand state :runner "Bazaar")
      (play-from-hand state :runner "Spy Camera") ; 1 installed
      (is (count-spy 1) "1 Spy Cameras installed")
      (prompt-choice :runner "Yes") ; for now, choosing Replicator then shows its optional Yes/No
      (prompt-choice :runner "Yes") ; Bazaar triggers, 2 installed
      (is (count-spy 2) "2 Spy Cameras installed")
      (prompt-choice :runner "Yes")
      (prompt-choice :runner "Yes")  ; 3 installed
      (is (count-spy 3) "3 Spy Cameras installed")

      (prompt-choice :runner "Yes")
      (prompt-choice :runner "Yes")  ; 4 installed
      (is (count-spy 4) "4 Spy Cameras installed")

      (prompt-choice :runner "Yes")
      (prompt-choice :runner "Yes")  ; 5 installed
      (is (count-spy 5) "5 Spy Cameras installed")

      (prompt-choice :runner "Yes")
      (prompt-choice :runner "Yes")  ; 6 installed
      (is (count-spy 6) "6 Spy Cameras installed"))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game (default-corp [(qty "Caduceus" 1)])
              (default-runner [(qty "Spinal Modem" 1) (qty "Sure Gamble" 1)]))
    (play-from-hand state :corp "Caduceus" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Spinal Modem")
    (let [cad (get-ice state :hq 0)
          sm (get-hardware state 0)]
      (is (= 5 (:memory (get-runner))))
      (is (= 2 (:rec-counter (refresh sm))))
      (run-on state :hq)
      (core/rez state :corp cad)
      (card-subroutine state :corp cad 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (core/hand-size state :runner)) "Reduced hand size"))))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Spy Camera" 6) (qty "Sure Gamble" 1) (qty "Desperado" 1)
                               (qty "Diesel" 1) (qty "Corroder" 1) (qty "Patron" 1) (qty "Kati Jones" 1)]))
    (starting-hand state :runner ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-runner)))))
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    (core/gain state :runner :click 3)
    (loop [x 6]
      (when (pos? x)
        (do (play-from-hand state :runner "Spy Camera")
            (recur (dec x)))))
    (let [spy (get-hardware state 5)]
      ;; look at top 6 cards
      (card-ability state :runner spy 0)
      (prompt-choice :runner (find-card "Sure Gamble" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Desperado" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Diesel" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Corroder" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Patron" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Kati Jones" (:deck (get-runner))))
      ;; try starting over
      (prompt-choice :runner "Start over")
      (prompt-choice :runner (find-card "Kati Jones" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Patron" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Corroder" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Diesel" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Desperado" (:deck (get-runner))))
      (prompt-choice :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
      (prompt-choice :runner "Done")
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

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an icebreaker
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "The Personal Touch" 1)
                               (qty "Paricia" 1)
                               (qty "Faerie" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (play-from-hand state :runner "Faerie")
    (let [par (get-in @state [:runner :rig :program 0])
          fae (get-in @state [:runner :rig :program 1])]
      (is (= 2 (:current-strength (refresh fae))))
      (play-from-hand state :runner "The Personal Touch")
      (prompt-select :runner par)
      (is (nil? (:hosted (refresh par))) "TPT can't be hosted on a non-icebreaker")
      (prompt-select :runner fae)
      (is (= 1 (count (:hosted (refresh fae)))) "TPT hosted on Faerie")
      (is (= 3 (:current-strength (refresh fae))) "Faerie receiving +1 strength from TPT"))))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Corp turn against Chronos Protocol
  (do-game
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" [(qty "Pup" 1) (qty "Viktor 1.0" 1)
                                                                     (qty "Neural EMP" 1)])
              (default-runner [(qty "Titanium Ribs" 2) (qty "Sure Gamble" 1)
                               (qty "Fall Guy" 1) (qty "Kati Jones" 1)]))
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Viktor 1.0" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Titanium Ribs")
    (prompt-select :runner (find-card "Titanium Ribs" (:hand (get-runner))))
    (prompt-select :runner (find-card "Kati Jones" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Fall Guy didn't try to prevent trashing of Kati")

    (is (= 2 (count (:discard (get-runner)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)]
      (core/rez state :corp pup)
      (card-subroutine state :corp pup 0)
      (prompt-select :runner (find-card "Sure Gamble" (:hand (get-runner)))) ; Ribs takes precedence over CP on Runner turn
      (is (= 3 (count (:discard (get-runner)))) "Chose card lost from 1 net damage")
      (run-jack-out state)
      (take-credits state :runner)
      (core/move state :runner (find-card "Sure Gamble" (:discard (get-runner))) :hand)
      (core/move state :runner (find-card "Kati Jones" (:discard (get-runner))) :hand)
      (play-from-hand state :corp "Neural EMP")
      (prompt-choice :corp "Yes")
      (let [kati (find-card "Kati Jones" (:hand (get-runner)))]
        (prompt-choice :corp kati) ; Chronos Protocol takes precedence over Ribs on Corp turn
        (is (= 2 (count (:discard (get-runner)))) "Card chosen by Corp for first net damage")))))

(deftest turntable-swap
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1) (qty "Project Vitruvius" 1)])
              (default-runner [(qty "Turntable" 1)]))
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (let [ag1 (get-content state :remote1 0)]
      (score-agenda state :corp ag1)
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (is (= 3 (:credit (get-runner))))
      (let [tt (get-in @state [:runner :rig :hardware 0])]
        (run-empty-server state "HQ")
        (prompt-choice :runner "Steal")
        (is (= 0 (:agenda-point (get-runner))) "Stole Domestic Sleepers")
        (is (prompt-is-card? :runner tt))
        (prompt-choice :runner "Yes")
        (prompt-select :runner (find-card "Project Vitruvius" (:scored (get-corp))))
        (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
        (is (= 0 (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")))))

(deftest turntable-mandatory-upgrades
  ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
  ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
  (do-game
    (new-game (default-corp [(qty "Mandatory Upgrades" 2) (qty "Project Vitruvius" 1)])
              (default-runner [(qty "Turntable" 1)]))
    (score-agenda state :corp (find-card "Mandatory Upgrades" (:hand (get-corp))))
    (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
    (take-credits state :corp)
    (play-from-hand state :runner "Turntable")
    (let [tt (get-in @state [:runner :rig :hardware 0])]
      ;; steal Project Vitruvius and swap for Mandatory Upgrades
      (core/steal state :runner (find-card "Project Vitruvius" (:hand (get-corp))))
      (is (prompt-is-card? :runner tt))
      (prompt-choice :runner "Yes")
      (prompt-select :runner (find-card "Mandatory Upgrades" (:scored (get-corp))))
      (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn")
      ;; steal second Mandatory Upgrades and swap for Project Vitruvius
      (core/steal state :runner (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (is (prompt-is-card? :runner tt))
      (prompt-choice :runner "Yes")
      (prompt-select :runner (find-card "Project Vitruvius" (:scored (get-corp))))
      (is (= 0 (:click (get-corp))) "Corp doesn't gain a click on Runner's turn")
      (is (= 4 (:click-per-turn (get-corp)))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Corp HQ is filled to max hand size
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)])
              (default-runner [(qty "Vigil" 1) (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Vigil")
    (is (= 5 (:memory (get-runner))))
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (is (empty? (:hand (get-runner))))
    (take-credits state :runner)
    (is (= (count (:hand (get-corp))) (core/hand-size state :corp)) "Corp hand filled to max")
    (take-credits state :corp)
    (is (= 1 (count (:hand (get-runner)))) "Drew 1 card")
    (take-credits state :runner)
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (is (not= (count (:hand (get-corp))) (core/hand-size state :corp)) "Corp hand below max")
    (is (= 1 (count (:hand (get-runner)))) "No card drawn")))
