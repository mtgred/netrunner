(ns game-test.cards.hardware
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(use-fixtures :once load-all-cards (partial reset-card-defs "hardware"))

(deftest acacia
  ;; Acacia - Optionally gain credits for number of virus tokens then trash
  (do-game
    (new-game (default-corp)
              (default-runner ["Acacia" "Virus Breeding Ground" "Datasucker"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Acacia")
    (play-from-hand state :runner "Virus Breeding Ground")
    (play-from-hand state :runner "Datasucker")
    (core/add-counter state :runner (get-resource state 0) :virus 4)
    (core/add-counter state :runner (get-program state 0) :virus 3)
    (take-credits state :runner)
    (is (= 2 (:credit (get-runner))) "Runner initial credits")
    (core/purge state :corp)
    (prompt-choice :runner "Yes")
    (is (= 9 (:credit (get-runner))) "Runner gained 9 credits")
    (is (= 1 (count (:discard (get-runner)))) "Acacia has trashed")))

(deftest akamatsu-mem-chip
  ;; Akamatsu Mem Chip - Gain 1 memory
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Akamatsu Mem Chip" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 5 (core/available-mu state)) "Gain 1 memory")))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game (default-corp ["Shock!" "Launch Campaign"])
              (default-runner ["Archives Interface" "Imp"]))
    (take-credits state :corp)
    (core/move state :corp (find-card "Shock!" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "Launch Campaign" (:hand (get-corp))) :discard)
    (play-from-hand state :runner "Archives Interface")
    (run-empty-server state :archives)
    (prompt-choice :runner "Yes")
    (prompt-card :runner (find-card "Shock!" (:discard (get-corp))))
    (is (= "Shock!" (:title (first (:rfg (get-corp))))) "Shock! removed from game")
    (is (empty? (:discard (get-runner))) "Didn't access Shock!, no net damage taken")))

(deftest astrolabe
  ;; Astrolabe - Draw on new server install
  (do-game
    (new-game (default-corp [(qty "Snare!" 3)])
              (default-runner [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) "Cloak"]))
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

(deftest blackguard
  ;; Blackguard - +2 MU, forced rez of exposed ice
  (do-game
   (new-game (default-corp ["Ice Wall"])
             (default-runner ["Blackguard"
                              "Snitch"]))
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
     (is (:rezzed (refresh iwall)) "Ice Wall was rezzed"))))

(deftest box-e
  ;; Box-E - +2 MU, +2 max hand size
  (do-game
   (new-game (default-corp)
             (default-runner ["Box-E"]))
   (take-credits state :corp)
   (play-from-hand state :runner "Box-E")
   (is (= 6 (core/available-mu state)))
   (is (= 7 (core/hand-size state :runner)))))

(deftest brain-chip
  ;; Brain Chip handsize and memory limit
  (do-game
   (new-game (default-corp)
             (default-runner ["Brain Chip"]))
   (take-credits state :corp)
   (play-from-hand state :runner "Brain Chip")
   (swap! state assoc-in [:runner :agenda-point] -2) ; hard set ap
   (is (= 5 (core/hand-size state :runner)) "Hand size unaffected")
   (is (= 4 (core/available-mu state)) "Memory limit unaffected")
   (swap! state assoc-in [:runner :agenda-point] 2)
   (is (= 7 (core/hand-size state :runner)) "Hand size increased by 2")
   (is (= 6 (core/available-mu state)) "Memory limit increased by 2")
   (core/move state :runner (get-hardware state 0) :discard)
   (is (= 5 (core/hand-size state :runner)) "Hand size reset")
   (is (= 4 (core/available-mu state)) "Memory limit reset")))

(deftest clone-chip
  ;; Test clone chip usage- outside and during run
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["Datasucker" (qty "Clone Chip" 2)]))
      (take-credits state :corp)
      (trash-from-hand state :runner "Datasucker")
      (play-from-hand state :runner "Clone Chip")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (prompt-select :runner (find-card "Datasucker" (:discard (get-runner))))
        (let [ds (get-program state 0)]
          (is (not (nil? ds)))
          (is (= (:title ds) "Datasucker"))))))
  (testing "don't show inavalid choices"
    (do-game
      (new-game (default-corp)
                (default-runner ["Inti" "Magnum Opus" "Clone Chip"]))
      (take-credits state :corp)
      (trash-from-hand state :runner "Inti")
      (trash-from-hand state :runner "Magnum Opus")
      (play-from-hand state :runner "Clone Chip")
      (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left")
      (let [chip (get-hardware state 0)]
        (card-ability state :runner chip 0)
        (prompt-select :runner (find-card "Magnum Opus" (:discard (get-runner))))
        (is (nil? (get-program state 0)) "No program was installed"))
      (let [chip (get-hardware state 0)]
        (is (not (nil? chip)) "Clone Chip is still installed")
        (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left")
        (card-ability state :runner chip 0)
        (prompt-select :runner (find-card "Inti" (:discard (get-runner))))
        (let [inti (get-program state 0)]
          (is (not (nil? inti)) "Program was installed")
          (is (= (:title inti) "Inti") "Program is Inti")
          (is (= (get-in @state [:runner :click]) 3) "Runner has 3 clicks left"))))))

(deftest comet
  ;; Comet - Play event without spending a click after first event played
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Comet")
    (let [comet (get-hardware state 0)]
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
    (new-game (default-corp ["Quandary"])
              (default-runner ["Cortez Chip"]))
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
    (is (= 6 (core/available-mu state)) "Gain 2 memory")))

(deftest daredevil
  ;; Daredevil
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 2)])
              (default-runner ["Daredevil" (qty "Sure Gamble" 3) (qty "Easy Mark" 2)]))
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

(deftest desperado
  ;; Desperado - Gain 1 MU and gain 1 credit on successful run
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Desperado" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Desperado")
    (run-empty-server state :archives)
    (is (= 5 (core/available-mu state)) "Gain 1 memory")
    (is (= 3 (:credit (get-runner))) "Got 1c for successful run on Desperado")))

(deftest dinosaurus
  ;; Dinosaurus
  (testing "Hosting a breaker with strength based on unused MU should calculate correctly"
    (do-game
      (new-game (default-corp)
                (default-runner ["Adept" "Dinosaurus"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dinosaurus")
      (play-from-hand state :runner "Adept")
      (is (= 2 (core/available-mu state)) "2 MU used")
      (let [dino (get-hardware state 0)
            adpt (get-program state 0)]
        (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
        (card-ability state :runner dino 1)
        (prompt-select :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh dino)))]
          (is (= 4 (core/available-mu state)) "0 MU used")
          (is (= 8 (:current-strength (refresh hosted-adpt))) "Adept at 8 strength hosted")))))
  (testing "Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker"
    (do-game
      (new-game (default-corp)
                (default-runner ["Dinosaurus" "Battering Ram"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Dinosaurus")
      (let [dino (get-hardware state 0)]
        (card-ability state :runner dino 0)
        (prompt-select :runner (find-card "Battering Ram" (:hand (get-runner))))
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
    (new-game (default-corp)
              (default-runner ["Doppelgänger"]))
    (core/gain state :corp :bad-publicity 1)
    (take-credits state :corp)
    (play-from-hand state :runner "Doppelgänger")
    (run-empty-server state :hq)
    (prompt-choice :runner "No action")
    (is (zero? (:run-credit (get-runner))) "Runner lost BP credits")
    (prompt-choice :runner "Yes")
    (prompt-choice :runner "R&D")
    (is (:run @state) "New run started")
    (is (= [:rd] (:server (:run @state))) "Running on R&D")
    (is (= 1 (:run-credit (get-runner))) "Runner has 1 BP credit")))

(deftest dorm-computer
  ;; make a run and avoid all tags for the remainder of the run
  (do-game
    (new-game (default-corp ["Snare!"])
              (default-runner ["Dorm Computer"]))
    (play-from-hand state :corp "Snare!" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Dorm Computer")
    (let [dorm (get-hardware state 0)]
      (card-ability state :runner dorm 0)
      (prompt-choice :runner "Server 1")
      (run-empty-server state "Server 1")
      (is (:run @state) "New run started")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type))
          "Runner has prompt to wait for Snare!")
      (prompt-choice :corp "Yes")
      (is (zero? (:tag (get-runner))) "Runner has 0 tags")
      (is (= 3 (get-counters (refresh dorm) :power))))))

(deftest feedback-filter
  ;; Feedback Filter - Prevent net and brain damage
  (do-game
    (new-game (default-corp ["Data Mine"
                             "Cerebral Overwriter"
                             "Mushin No Shin"])
              (default-runner [(qty "Feedback Filter" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Mushin No Shin")
    (prompt-select :corp (find-card "Cerebral Overwriter" (:hand (get-corp))))
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
        (is (empty? (get-hardware state)) "Feedback Filter trashed")))))

(deftest flame-out
  ;; Flame-out - start with 9 credits, use for hosted program, trash hosted program at end of turn when credits used
  (testing "Basic behavior"
    (do-game
      (new-game (default-corp)
                (default-runner ["Flame-out" "Mimic"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (prompt-select :runner (find-card "Mimic" (:hand (get-runner))))
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
      (new-game (default-corp)
                (default-runner ["Flame-out" "Mimic"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Flame-out")
      (let [fo (get-hardware state 0)]
        (card-ability state :runner fo 2)
        (prompt-select :runner (find-card "Mimic" (:hand (get-runner))))
        (take-credits state :runner)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic hosted")
        (is (= 2 (:credit (get-runner))) "Runner starts with 2 credits")
        (card-ability state :runner fo 1)
        (is (= 1 (count (:hosted (refresh fo)))) "Mimic still hosted")
        (is (= 11 (:credit (get-runner))) "Runner gains 9 credit")
        (is (zero? (get-counters (refresh fo) :credit)) "Took all credits from Flame-out")
        (take-credits state :corp)
        (is (empty? (:hosted (refresh fo))) "Mimic trashed")))))

(deftest friday-chip
  ;; Friday Chip - gain counters for trashing cards, move a counter on turn start
  (do-game
    (new-game (default-corp ["Adonis Campaign" "Hedge Fund"])
              (default-runner ["Friday Chip" "Aumakua"]))
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
      (prompt-choice-partial :runner "Pay") ; trash Adonis Campaing
      (prompt-choice :runner "Yes") ; gain virus counter
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip gains a counter on trash")
      (is (zero? (get-counters (refresh aum) :virus)) "Aumakua doesn't gain a counter")
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (= 1 (get-counters (refresh fc) :virus)) "Friday Chip doesn't gain a counter on non-trash")
      (is (= 1 (get-counters (refresh aum) :virus)) "Aumakua gains a counter on non-trash")
      (take-credits state :runner)
      (take-credits state :corp)
      (prompt-select :runner aum)
      (is (= 2 (get-counters (refresh aum) :virus)) "Aumakua gained 1 counter")
      (is (zero? (get-counters (refresh fc) :virus)) "Friday Chip lost 1 counter"))))

(deftest grimoire
  ;; Grimoire - Gain 2 MU, add a free virus counter to installed virus programs
  (do-game
    (new-game (default-corp)
              (default-runner ["Grimoire" "Imp"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Grimoire")
    (is (= 6 (core/available-mu state)) "Gained 2 MU")
    (play-from-hand state :runner "Imp")
    (let [imp (get-program state 0)]
      (is (= 3 (get-counters (refresh imp) :virus)) "Imp received an extra virus counter on install"))))

(deftest heartbeat
  ;; Heartbeat - +1 MU, trash installed card to prevent 1 damage
  (do-game
    (new-game (default-corp ["Pup" "Neural Katana"])
              (make-deck "Apex: Invasive Predator" [(qty "Heartbeat" 2) (qty "Sure Gamble" 2) "Cache"]))
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Neural Katana" "R&D")
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (prompt-select :runner (find-card "Heartbeat" (:hand (get-runner))))
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
      (card-ability state :runner hb 0)
      (prompt-select :runner cache)
      (prompt-choice :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Prevented 1 net damage")
      (is (= 2 (count (:hand (get-runner)))))
      (card-subroutine state :corp (refresh nk) 0)
      (card-ability state :runner hb 0)
      (prompt-select :runner hbdown)
      (prompt-choice :runner "Done")
      (is (= 4 (count (:discard (get-runner)))) "Prevented 1 of 3 net damage; used facedown card"))))

(deftest hippo
  ;; Hippo - remove from game to trash outermost piece of ice if all subs broken
  (testing "No ice"
    (do-game
      (new-game (default-corp)
                (default-runner ["Hippo"]))
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (empty? (:rfg (get-runner))) "Hippo not RFGed")
      (is (not-empty (get-hardware state)) "Hippo still installed")))
  (testing "Single ice"
    (do-game
      (new-game (default-corp ["Ice Wall"])
                (default-runner ["Hippo"]))
      (play-from-hand state :corp "Ice Wall" "HQ")
      (core/rez state :corp (get-ice state :hq 0))
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Ice Wall installed")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (empty? (get-in @state [:corp :servers :hq :ices])) "Ice Wall removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice Wall trashed")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed")))
  (testing "Multiple ice"
    (do-game
      (new-game (default-corp ["Ice Wall" "Enigma"])
                (default-runner ["Hippo"]))
      (play-from-hand state :corp "Enigma" "HQ")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (take-credits state :corp)
      (play-from-hand state :runner "Hippo")
      (run-on state "HQ")
      (is (not-empty (get-hardware state)) "Hippo installed")
      (is (= 2 (count (get-in @state [:corp :servers :hq :ices]))) "2 ice installed")
      (is (= "Ice Wall" (:title (get-ice state :hq 1))) "Ice Wall outermost")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma innermost")
      (card-ability state :runner (get-hardware state 0) 0)
      (is (= 1 (count (get-in @state [:corp :servers :hq :ices]))) "Ice removed")
      (is (= 1 (count (:discard (get-corp)))) "Ice trashed")
      (is (= "Ice Wall" (:title (first (:discard (get-corp))))) "Ice Wall in trash")
      (is (= "Enigma" (:title (get-ice state :hq 0))) "Enigma still innermost")
      (is (= 1 (count (:rfg (get-runner)))) "Hippo RFGed")
      (is (empty? (get-hardware state)) "Hippo removed"))))

(deftest knobkierie
  ;; Knobkierie - first successful run, place a virus counter on a virus program
  (do-game
    (new-game (default-corp)
              (default-runner ["Knobkierie" "Hivemind" "Eater"]))
    (core/gain state :runner :credit 20)
    (take-credits state :corp)
    (play-from-hand state :runner "Knobkierie")
    (play-from-hand state :runner "Eater")
    (run-on state "HQ")
    (run-successful state)
    (prompt-choice :runner "No action")
    (is (empty? (:prompt (get-runner))) "No prompt if not virus program installed")
    (take-credits state :runner)
    (take-credits state :corp)
    (play-from-hand state :runner "Hivemind")
    (let [hv (find-card "Hivemind" (get-program state))]
      (is (= 1 (get-counters (refresh hv) :virus)) "Hivemind starts with 1 virus counters")
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :runner "Yes") ; gain virus counter
      (prompt-select :runner (find-card "Hivemind" (get-program state)))
      (prompt-choice :runner "No action")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind gains a counter on successful run")
      (run-on state "HQ")
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (empty? (:prompt (get-runner))) "No prompt after first run")
      (is (= 2 (get-counters (refresh hv) :virus)) "Hivemind doesn't gain a counter after first run"))))

(deftest llds-processor
  ;; LLDS Processor - Add 1 strength until end of turn to an icebreaker upon install
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "LLDS Processor" 2) "Inti" "Passport"]))
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

(deftest ^{:card-title "mâché"}
  mache
  ;; Mâché
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Ice Wall" "PAD Campaign"])
                (default-runner ["Imp" "Mâché" "Cache"]))
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
        (prompt-choice-partial :runner "Imp")
        (is (= counters (get-counters (refresh mache) :power)) "Mache should gain no counters from trashing a card with no trash cost")
        (run-empty-server state :remote1)
        (prompt-choice-partial :runner "Pay")
        (is (= (+ counters 4) (get-counters (refresh mache) :power)) "Mache should gain 4 counters for trashing a card with a trash cost of 4")
        (card-ability state :runner mache 0)
        (is (= (inc hand) (-> (get-runner) :hand count)) "Runner should draw one card for using Mache's ability")
        (is (= 1 (get-counters (refresh mache) :power)) "Mache ability should cost 3 counters"))))
  (testing "with Political Operative"
    (do-game
      (new-game (default-corp ["Ice Wall" "PAD Campaign"])
                (default-runner ["Mâché" "Political Operative" "Cache"]))
      (play-from-hand state :corp "PAD Campaign" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (starting-hand state :runner ["Mâché" "Political Operative"])
      (play-from-hand state :runner "Mâché")
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (play-from-hand state :runner "Political Operative")
      (take-credits state :runner)
      (let [pad (get-content state :remote1 0)
            mache (get-hardware state 0)
            polop (get-resource state 0)]
        (card-ability state :runner polop 0)
        (prompt-select :runner (refresh pad))
        (is (zero? (get-counters (refresh mache) :power)) "Mache should gain no counters from a trash outside of an access")))))

(deftest maw
  ;; Maw - Once per turn, first time runner declines to steal or trash, trash a HQ card at random
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "BOOM!" 5)])
                (default-runner ["Maw"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "No HQ card in discard before Maw installed")
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (zero? (count (:discard (get-corp)))) "HQ card not trashed by Maw as first decline already happened")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw")
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "2nd HQ card on same turn not trashed by Maw")))
  (testing "Check trashed card is trashed face-up if it's the card that is accessed, issue #2695"
    ;; Also checks Maw auto-trashes on Operation with no trash cost
    (do-game
      (new-game (default-corp ["Hedge Fund"])
                (default-runner ["Maw"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (is (zero? (count (:discard (get-corp)))) "HQ card not trashed by Maw yet")
      (prompt-choice :runner "No action")
      (is (= 1 (count (:discard (get-corp)))) "HQ card trashed by Maw now")
      (is (:seen (first (:discard (get-corp)))) "Trashed card is registered as seen since it was accessed")))
  (testing "with Hiro in hand - Hiro not moved to runner scored area on trash decline. #2638"
    (do-game
      (new-game (default-corp ["Chairman Hiro"])
                (default-runner ["Maw"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (zero? (count (:scored (get-runner)))) "Hiro not scored")
      (is (= 1 (count (:discard (get-corp)))) "Hiro trashed by Maw")))
  (testing "Maw shouldn't trigger on stolen agenda. #3433"
    (do-game
      (new-game (default-corp ["Hostile Takeover"
                               (qty "Ice Wall" 5)])
                (default-runner ["Maw"]))
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (take-credits state :corp)
      (core/gain state :runner :credit 20)
      (play-from-hand state :runner "Maw")
      (run-empty-server state :remote1)
      (prompt-choice :runner "Steal")
      (is (zero? (count (:discard (get-corp)))) "No HQ card in discard as agenda was stolen")))
  (testing "Maw shouldn't trigger when accessing a card in archives. #3388"
    (do-game
      (new-game (default-corp ["Rashida Jaheem" "Cyberdex Virus Suite" (qty "Ice Wall" 4)])
                (make-deck "Alice Merchant: Clan Agitator" ["Maw" "Imp"]))
      (core/move state :corp (find-card "Rashida Jaheem" (:hand (get-corp))) :deck)
      (trash-from-hand state :corp "Cyberdex Virus Suite")
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Imp")
      (run-empty-server state :archives)
      (prompt-card :corp (find-card "Ice Wall" (:hand (get-corp)))) ;; Alice's ability
      (prompt-choice :runner "Cyberdex Virus Suite")
      (prompt-choice :corp "Yes")
      (run-empty-server state :rd)
      (prompt-choice-partial :runner "Pay")
      (is (= 3 (count (:discard (get-corp)))) "Ice Wall, CVS, and Rashida")
      (is (empty? (:prompt (get-runner))) "No more prompts for runner")))
 (testing "Maw should trigger when declining to steal. #3388"
    (do-game
      (new-game (default-corp [(qty "Obokata Protocol" 2) (qty "Ice Wall" 4)])
                (make-deck "Alice Merchant: Clan Agitator" ["Maw" "Archives Interface"]))
      (trash-from-hand state :corp "Ice Wall")
      (starting-hand state :corp ["Obokata Protocol" "Obokata Protocol"])
      (take-credits state :corp)
      (core/gain state :runner :credit 100)
      (play-from-hand state :runner "Maw")
      (play-from-hand state :runner "Archives Interface")
      (run-empty-server state :archives)
      (prompt-card :corp (find-card "Obokata Protocol" (:hand (get-corp))))
      (prompt-choice :runner "Yes")
      (prompt-card :runner (find-card "Ice Wall" (:discard (get-corp))))
      (prompt-choice :runner "No action")
      (run-empty-server state :hq)
      (prompt-choice :runner "No action")
      (is (= 2 (count (:discard (get-corp)))) "Ice Wall and Obokata"))))

(deftest maya
  ;; Maya - Move accessed card to bottom of R&D
  (testing "Basic test"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 2) (qty "Snare!" 2) "Hostile Takeover" "Scorched Earth"])
                (default-runner ["Maya" (qty "Sure Gamble" 3)]))
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
        (take-credits state :corp)
        (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
        (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
        (let [accessed (first (:deck (get-corp)))]
          (run-empty-server state :rd)
          (prompt-choice :corp "Yes")
          (is (zero? (count (:hand (get-runner)))) "Runner took Snare! net damage")
          (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
          (card-ability state :runner maya 0)
          (is (empty? (:prompt (get-runner))) "No more prompts for runner")
          (is (not (:run @state)) "Run is ended")
          (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")))))
  (testing "Does not interrupt multi-access"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 2) (qty "Scorched Earth" 2) (qty "Snare!" 2)])
                (default-runner ["Maya" (qty "Sure Gamble" 3) "R&D Interface"]))
      (core/move state :corp (find-card "Scorched Earth" (:hand (get-corp))) :deck)
      (core/move state :corp (find-card "Snare!" (:hand (get-corp))) :deck)
      (take-credits state :corp)
      (core/gain state :runner :credit 10)
      (play-from-hand state :runner "Maya")
      (play-from-hand state :runner "R&D Interface")
      (let [maya (get-hardware state 0)
            accessed (first (:deck (get-corp)))]
        (run-empty-server state :rd)
        (prompt-choice :runner "Card from deck")
        (is (= (:cid accessed) (:cid (:card (first (:prompt (get-runner)))))) "Accessing the top card of R&D")
        (card-ability state :runner maya 0)
        (is (= (:cid accessed) (:cid (last (:deck (get-corp))))) "Maya moved the accessed card to the bottom of R&D")
        (is (:prompt (get-runner)) "Runner has next access prompt")))))

(deftest net-ready-eyes
  ;; Net-Ready Eyes
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Sure Gamble" 3) "Net-Ready Eyes" "Peacock"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Peacock")
    (play-from-hand state :runner "Net-Ready Eyes")
    (is (= 3 (count (:discard (get-runner)))) "Took 2 damage on NRE install")
    (run-on state "HQ")
    (let [pea (get-program state 0)]
      (prompt-select :runner pea)
      (is (= 3 (:current-strength (refresh pea))) "Peacock strength boosted")
      (run-continue state)
      (run-successful state)
      (prompt-choice :runner "No action")
      (is (= 2 (:current-strength (refresh pea))) "Peacock strength back to default"))))

(deftest obelus
  ;; Obelus - Increase max hand size with tags, draw cards on first successful HQ/R&D run
  (testing "Basic test"
    (do-game
      (new-game (default-corp)
                (default-runner ["Obelus" "Nerve Agent"
                                 (qty "Sure Gamble" 3) (qty "Cache" 3)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus" "Nerve Agent"])
      (core/gain state :runner :credit 10 :click 3)
      (play-from-hand state :runner "Nerve Agent")
      (let [nerve (get-program state 0)]
        (run-empty-server state :hq)
        (is (= 1 (get-counters (refresh nerve) :virus)) "1 virus counter on Nerve Agent")
        (prompt-choice :runner "No action")
        (play-from-hand state :runner "Obelus")
        (core/gain state :runner :tag 1)
        (is (= 6 (core/hand-size state :runner)) "Max hand size is 6")
        (core/lose state :runner :tag 1)
        (is (= 5 (core/hand-size state :runner)) "Max hand size is 5")
        (run-empty-server state :hq)
        (is (= 2 (get-counters (refresh nerve) :virus)) "2 virus counters on Nerve Agent")
        (prompt-choice :runner 1)
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "No action")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "No action")
        (is (empty? (:hand (get-runner))) "No cards drawn by Obelus, already had successful HQ run")
        (take-credits state :runner)
        (take-credits state :corp)
        (run-empty-server state :hq)
        (is (= 3 (get-counters (refresh nerve) :virus)) "3 virus counters on Nerve Agent")
        (prompt-choice :runner 2)
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "No action")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "No action")
        (prompt-choice :runner "Card from hand")
        (prompt-choice :runner "No action")
        (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards"))))
  (testing "running and trashing Crisium Grid makes run neither successful/unsuccessful"
    (do-game
      (new-game (default-corp ["Hedge Fund" "Crisium Grid"])
                (default-runner ["Obelus" (qty "Sure Gamble" 3)]))
      (starting-hand state :corp ["Crisium Grid"])
      (play-from-hand state :corp "Crisium Grid" "R&D")
      (core/rez state :corp (get-content state :rd 0))
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus"])
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "R&D")
      (prompt-choice :runner "Crisium Grid")
      (prompt-choice-partial :runner "Pay")
      (prompt-choice-partial :runner "Card")
      (prompt-choice-partial :runner "No")
      (is (empty? (:hand (get-runner))) "Crisium Grid blocked successful run")
      (run-empty-server state "R&D")
      (prompt-choice-partial :runner "No")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run")))
  (testing "using Hades Shard during run to increase draw"
    (do-game
      (new-game (default-corp [(qty "Hedge Fund" 3) (qty "Restructure" 3)])
                (default-runner ["Obelus" "Hades Shard"
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
      (prompt-choice :runner "No action")
      (is (= 3 (count (:hand (get-runner)))) "Obelus drew 3 cards")))
  (testing "running a remote server first doesn't block card draw"
    (do-game
      (new-game (default-corp ["Urban Renewal" "Hedge Fund"])
                (default-runner ["Obelus" (qty "Sure Gamble" 3)]))
      (starting-hand state :corp ["Urban Renewal"])
      (play-from-hand state :corp "Urban Renewal" "New remote")
      (take-credits state :corp)
      (starting-hand state :runner ["Obelus"])
      (play-from-hand state :runner "Obelus")
      (is (empty? (:hand (get-runner))) "No cards in hand")
      (run-empty-server state "Server 1")
      (prompt-choice-partial :runner "No")
      (run-empty-server state "R&D")
      (prompt-choice-partial :runner "No")
      (is (= 1 (count (:hand (get-runner)))) "Obelus drew a card on first successful run"))))

(deftest plascrete-carapace
  ;; Plascrete Carapace - Prevent meat damage
  (do-game
    (new-game (default-corp ["Scorched Earth"])
              (default-runner ["Plascrete Carapace" "Sure Gamble"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Plascrete Carapace")
    (let [plas (get-hardware state 0)]
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
      (is (empty? (get-hardware state)) "Plascrete depleted and trashed"))))

(deftest rabbit-hole
  ;; Rabbit Hole - +1 link, optionally search Stack to install more copies
  (do-game
    (new-game (default-corp)
              (default-runner ["Sure Gamble" (qty "Rabbit Hole" 3)]))
    (take-credits state :corp)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Rabbit Hole" (:hand (get-runner))) :deck)
    (play-from-hand state :runner "Sure Gamble")
    (play-from-hand state :runner "Rabbit Hole")
    (is (= 1 (:link (get-runner))))
    (prompt-choice :runner "Yes")
    (prompt-choice :runner "Yes")
    (is (= 3 (:link (get-runner))))
    (is (= 3 (count (get-hardware state))))
    (is (= 2 (:click (get-runner))) "Clickless installs of extra 2 copies")
    (is (= 3 (:credit (get-runner))) "Paid 2c for each of 3 copies")))

(deftest ramujan-reliant-550-bmi
  ;; Prevent up to X net or brain damage.
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Data Mine" "Snare!"])
                (default-runner [(qty "Ramujan-reliant 550 BMI" 4)
                                 (qty "Sure Gamble" 6)]))
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
          (prompt-choice :runner 1)
          (is (last-log-contains? state "Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 2 (count (:hand (get-runner)))) "1 net damage prevented")
          (run-successful state)
          (take-credits state :runner)
          (take-credits state :corp)
          (play-from-hand state :runner "Ramujan-reliant 550 BMI")
          (run-empty-server state "Server 1")
          (prompt-choice :corp "Yes")
          (card-ability state :runner rr2 0)
          (prompt-choice :runner 3)
          (is (last-log-contains? state "Sure Gamble, Sure Gamble, Sure Gamble")
              "Ramujan did log trashed card names")
          (is (= 1 (count (:hand (get-runner)))) "3 net damage prevented")))))
  (testing "Prevent up to X net or brain damage. Empty stack"
    (do-game
      (new-game (default-corp ["Data Mine"])
                (default-runner ["Ramujan-reliant 550 BMI" "Sure Gamble"]))
      (play-from-hand state :corp "Data Mine" "Server 1")
      (let [dm (get-ice state :remote1 0)]
        (take-credits state :corp)
        (play-from-hand state :runner "Ramujan-reliant 550 BMI")
        (let [rr1 (get-hardware state 0)]
          (run-on state "Server 1")
          (core/rez state :corp dm)
          (card-subroutine state :corp dm 0)
          (card-ability state :runner rr1 0)
          (prompt-choice :runner 1)
          (is (zero? (count (:hand (get-runner)))) "Not enough cards in Stack for Ramujan to work"))))))

(deftest recon-drone
  ;; trash and pay X to prevent that much damage from a card you are accessing
  (do-game
    (new-game (default-corp ["Snare!" "House of Knives"
                             "Prisec" "Cerebral Overwriter"])
              (default-runner [(qty "Recon Drone" 10)]))
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
      (prompt-choice :corp "Yes")
      (card-ability state :runner rd1 0)
      (prompt-choice :runner 3)
      (prompt-choice :runner "Done")
      (prompt-choice :runner "No action")
      (is (= 5 (count (:hand (get-runner)))) "Runner took no net damage")
      ; fire HOK while accessing Snare!
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Snare!")
      (card-ability state :corp hok 0)
      ; Recon Drone ability won't fire as we are not accessing HOK
      (card-ability state :runner rd2 0)
      (is (nil? (:number (:choices (first (:prompt (get-runner)))))) "No choice to prevent damage from HOK")
      (prompt-choice :runner "Done")
      (is (= 4 (count (:hand (get-runner)))) "Runner took 1 net damage from HOK")
      (prompt-choice :corp "No")
      (prompt-choice :runner "No action")
      (core/lose state :runner :credit 100)
      ; can only stop 1 damage due to credits
      (core/gain state :runner :credit 1)
      (run-empty-server state "Server 2")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Snare!")
      (prompt-choice :corp "Yes")
      (card-ability state :runner rd2 0)
      (is (= 1 (:number (:choices (first (:prompt (get-runner)))))) "Recon Drone choice limited to runner credits")
      (prompt-choice :runner 1)
      (prompt-choice :runner "Done")
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (count (:hand (get-runner)))) "Runner took 2 net damage from Snare!")
      (core/gain state :runner :credit 100)
      (run-empty-server state "Server 3")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Prisec")
      (prompt-choice :corp "Yes")
      (card-ability state :runner rd3 0)
      (is (= 1 (:number (:choices (first (:prompt (get-runner)))))) "Recon Drone choice limited to 1 meat")
      (prompt-choice :runner 1)
      (prompt-choice :runner "Done")
      (prompt-choice-partial :runner "Pay")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no meat damage")
      (run-empty-server state "Server 4")
      (is (= :waiting (-> @state :runner :prompt first :prompt-type)) "Runner has prompt to wait for Cerebral Overwriter")
      (prompt-choice :corp "Yes")
      (card-ability state :runner rd4 0)
      (prompt-choice :runner 1)
      (prompt-choice :runner "Done")
      (is (= 2 (count (:hand (get-runner)))) "Runner took no brain damage"))))

(deftest replicator
  ;; Replicator
  (testing "interaction with Bazaar. Issue #1511"
    (do-game
      (new-game (default-corp)
                (default-runner ["Replicator" "Bazaar" (qty "Spy Camera" 6)]))
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
        (is (count-spy 6) "6 Spy Cameras installed")))))

(deftest respirocytes
  (testing "Should draw multiple cards when multiple respirocytes are in play"
    (do-game
      (new-game (default-corp)
                (default-runner [(qty "Respirocytes" 3) (qty "Sure Gamble" 3)]))
      (take-credits state :corp)
      (starting-hand state :runner ["Respirocytes" "Respirocytes" "Respirocytes" "Sure Gamble"])
      (dotimes [_ 2]
        (play-from-hand state :runner "Respirocytes"))
      (is (= 2 (count (:discard (get-runner)))) "2 damage done")
      (is (= 2 (count (:hand (get-runner)))) "Drew 2 cards"))))

(deftest rubicon-switch
  ;; Rubicon Switch
  (do-game
   (new-game (default-corp ["Ice Wall" "Pachinko"])
             (default-runner ["Rubicon Switch"]))
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
       (prompt-choice :runner 1)
       (prompt-select :runner (refresh iw))
       (is (:rezzed (refresh iw)) "Ice Wall rezzed last turn can't be targeted")
       (prompt-select :runner (refresh pach))
       (is (not (:rezzed (refresh pach))) "Pachinko derezzed")
       (is (= 2 (:click (get-runner))) "Spent 1 click")
       (is (= 1 (:credit (get-runner))) "Spent 1c")))))

(deftest security-nexus
  ;; Security Nexus
  (do-game
    (new-game (default-corp ["Ice Wall"])
              (default-runner ["Security Nexus"]))
    (play-from-hand state :corp "Ice Wall" "R&D")
    (take-credits state :corp)
    (core/gain state :runner :credit 100)
    (play-from-hand state :runner "Security Nexus")
    (let [nexus (get-hardware state 0)]
      (run-on state :rd)
      (card-ability state :runner nexus 0)
      (is (zero? (:tag (get-runner))) "Runner should have no tags to start")
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (not (:run @state)) "Run should end from losing Security Nexus trace")
      (is (= 1 (:tag (get-runner))) "Runner should take 1 tag from losing Security Nexus trace")
      (take-credits state :runner)
      (take-credits state :corp)
      (run-on state :rd)
      (card-ability state :runner nexus 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 10)
      (is (:run @state) "Run should still be going on from winning Security Nexus trace")
      (is (= 1 (:tag (get-runner))) "Runner should still only have 1 tag"))))

(deftest sifr
  ;; Sifr - Once per turn drop encountered ICE to zero strenght
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game (default-corp ["Archangel" "IP Block" "Hedge Fund"])
              (default-runner ["Modded" "Clone Chip" "Şifr" "Parasite"]))
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
      (is (not (empty? (:prompt (get-corp)))) "Archangel trace prompt - corp")
      (prompt-choice :corp 0)
      (is (not (empty? (:prompt (get-runner)))) "Archangel trace prompt - runner")
      (prompt-choice :runner 0)
      (prompt-select :corp sifr)
      (is (= 3 (count (:hand (get-runner))))) ; sifr got lifted to hand
      (run-jack-out state)
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Modded")
      (is (not (empty? (:prompt (get-runner)))) "Modded choice prompt exists")
      (prompt-select :runner (find-card "Şifr" (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Clone Chip")
      (take-credits state :runner)
      (take-credits state :corp 4)
      (let [chip (get-hardware state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (zero? (count (:discard (get-corp)))) "No Corp cards trashed")
        (card-ability state :runner chip 0)
        (prompt-select :runner (find-card "Parasite" (:discard (get-runner))))
        (let [para (get-program state 0)]
          (prompt-select :runner ip)
          (is (zero? (count (:discard (get-corp)))) "IP Block Not Trashed")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))

(deftest spinal-modem
  ;; Spinal Modem - +1 MU, 2 recurring credits, take 1 brain damage on successful trace during run
  (do-game
    (new-game (default-corp ["Caduceus"])
              (default-runner ["Spinal Modem" "Sure Gamble"]))
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
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (:brain-damage (get-runner))) "Took 1 brain damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (core/hand-size state :runner)) "Reduced hand size"))))

(deftest sports-hopper
  ;; Sports Hopper
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Sports Hopper" 3) (qty "Sure Gamble" 3)]))
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
    (new-game (default-corp)
              (default-runner [(qty "Spy Camera" 6) "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron" "Kati Jones"]))
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
      (prompt-card :runner (find-card "Sure Gamble" (:deck (get-runner))))
      (prompt-card :runner (find-card "Desperado" (:deck (get-runner))))
      (prompt-card :runner (find-card "Diesel" (:deck (get-runner))))
      (prompt-card :runner (find-card "Corroder" (:deck (get-runner))))
      (prompt-card :runner (find-card "Patron" (:deck (get-runner))))
      (prompt-card :runner (find-card "Kati Jones" (:deck (get-runner))))
      ;; try starting over
      (prompt-choice :runner "Start over")
      (prompt-card :runner (find-card "Kati Jones" (:deck (get-runner))))
      (prompt-card :runner (find-card "Patron" (:deck (get-runner))))
      (prompt-card :runner (find-card "Corroder" (:deck (get-runner))))
      (prompt-card :runner (find-card "Diesel" (:deck (get-runner))))
      (prompt-card :runner (find-card "Desperado" (:deck (get-runner))))
      (prompt-card :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
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

(deftest the-gauntlet
  (testing "Access additional cards on run on HQ, not with Gang Sign. Issue #2749"
    (do-game
      (new-game (default-corp ["Hostile Takeover"
                               (qty "Hedge Fund" 3)])
                (default-runner ["The Gauntlet"
                                 "Gang Sign"]))
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Gang Sign")
      (play-from-hand state :runner "The Gauntlet")
      (take-credits state :runner)
      (play-from-hand state :corp "Hostile Takeover" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      ;; Gang Sign should trigger, without The Gauntlet pop-up
      (let [gs (get-resource state 0)]
        (prompt-is-card? :runner gs))
      ;; This will throw error if The Gauntlet triggers.
      (prompt-choice :runner "Card from hand"))))

(deftest the-personal-touch
  ;; The Personal Touch - Give +1 strength to an icebreaker
  (do-game
    (new-game (default-corp)
              (default-runner ["The Personal Touch"
                               "Paricia"
                               "Faerie"]))
    (take-credits state :corp)
    (play-from-hand state :runner "Paricia")
    (play-from-hand state :runner "Faerie")
    (let [par (get-program state 0)
          fae (get-program state 1)]
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
    (new-game (make-deck "Chronos Protocol: Selective Mind-mapping" ["Pup" "Viktor 1.0"
                                                                     "Neural EMP"])
              (default-runner [(qty "Titanium Ribs" 2) "Sure Gamble"
                               "Fall Guy" "Kati Jones"]))
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

(deftest turntable
  ;; Turntable - Swap a stolen agenda for a scored agenda
  (testing "Basic test"
    (do-game
      (new-game (default-corp ["Domestic Sleepers" "Project Vitruvius"])
                (default-runner ["Turntable"]))
      (play-from-hand state :corp "Project Vitruvius" "New remote")
      (let [ag1 (get-content state :remote1 0)]
        (score-agenda state :corp ag1)
        (take-credits state :corp)
        (play-from-hand state :runner "Turntable")
        (is (= 3 (:credit (get-runner))))
        (let [tt (get-hardware state 0)]
          (run-empty-server state "HQ")
          (prompt-choice :runner "Steal")
          (is (zero? (:agenda-point (get-runner))) "Stole Domestic Sleepers")
          (is (prompt-is-card? :runner tt))
          (prompt-choice :runner "Yes")
          (prompt-select :runner (find-card "Project Vitruvius" (:scored (get-corp))))
          (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
          (is (zero? (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")))))
  (testing "vs Mandatory Upgrades"
    ;; Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn
    ;;           - Corp doesn't gain a click on the Runner's turn when it receives a Mandatory Upgrades
    (do-game
      (new-game (default-corp [(qty "Mandatory Upgrades" 2) "Project Vitruvius"])
                (default-runner ["Turntable"]))
      (score-agenda state :corp (find-card "Mandatory Upgrades" (:hand (get-corp))))
      (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (let [tt (get-hardware state 0)]
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
        (is (zero? (:click (get-corp))) "Corp doesn't gain a click on Runner's turn")
        (is (= 4 (:click-per-turn (get-corp))))))))

(deftest vigil
  ;; Vigil - Draw 1 card when turn begins if Corp HQ is filled to max hand size
  (do-game
    (new-game (default-corp [(qty "Hedge Fund" 3) (qty "PAD Campaign" 2)])
              (default-runner ["Vigil" (qty "Sure Gamble" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Vigil")
    (is (= 5 (core/available-mu state)))
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

(deftest zamba
  ;; Zamba - Whenever corp card is exposed you may gain 1 credit
  (do-game
   (new-game (default-corp ["Ice Wall"])
             (default-runner ["Zamba" (qty "Infiltration" 2)]))
   (play-from-hand state :corp "Ice Wall" "Archives")
   (take-credits state :corp)
   (play-from-hand state :runner "Zamba")
   (is (= 6 (core/available-mu state)) "Gain 2 memory")
   (is (= 1 (:credit (get-runner))) "At 1 credit")
   (play-from-hand state :runner "Infiltration")
   (prompt-choice :runner "Expose a card")
   (prompt-select :runner (get-ice state :archives 0))
   (is (= 2 (:credit (get-runner))) "Gained 1 credit from exposing")
   (play-from-hand state :runner "Infiltration")
   (prompt-choice :runner "Expose a card")
   (prompt-select :runner (get-ice state :archives 0))
   (is (= 3 (:credit (get-runner))) "Gained 1 more credit from exposing")))

(deftest zer0
  ;; Zer0 - Once per turn, deal 1 damage to self, to gain 1 credit and 2 cards.
  (do-game
    (new-game (default-corp)
              (default-runner ["Zer0" "Corroder" (qty "Sure Gamble" 2)]))
    (starting-hand state :runner ["Zer0" "Corroder"])
    (take-credits state :corp)
    (play-from-hand state :runner "Zer0")
    (is (= 4 (:credit (get-runner))) "Runner has 4 credits")
    (let  [z (get-hardware state 0)]
      (card-ability state :runner z 0)
      (is (= 5 (:credit (get-runner))) "Runner has 5 credits")
      (is (= 2 (count (:hand (get-runner)))) "Runner has 2 cards")
      (is (find-card "Corroder" (:discard (get-runner))) "Corroder is in heap"))))
