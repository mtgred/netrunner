(in-ns 'test.core)

(deftest astrolabe-memory
  "Astrolabe - Gain 1 memory"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Astrolabe" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest astrolabe-draw
  "Astrolabe - Draw on new server install"
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

(deftest brain-chip
  "Brain Chip handsize and memory limit"
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
  "Test clone chip usage- outside and during run"
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
  "Comet - Play event without spending a click after first event played"
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

(deftest dinosaurus-strength-boost-mu-savings
  "Dinosaurus - Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker"
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

(deftest feedback-filter
  "Feedback Filter - Prevent net and brain damage"
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
        (card-ability state :corp dm 0)
        (card-ability state :runner ff 0)
        (prompt-choice :runner "Done")
        (is (= 3 (count (:hand (get-runner)))) "1 net damage prevented")
        (is (= 4 (:credit (get-runner))))
        (run-successful state)
        (prompt-choice :corp "Yes") ; pay 3 to fire Overwriter
        (prompt-choice :runner "Yes") ; trash Overwriter for 0 to get to prevention prompt
        (card-ability state :runner ff 1)
        (prompt-choice :runner "Done")
        (is (= 1 (:brain-damage (get-runner))) "2 of the 3 brain damage prevented")
        (is (= 2 (count (:hand (get-runner)))))
        (is (empty? (get-in @state [:runner :rig :hardware])) "Feedback Filter trashed")))))

(deftest grimoire
  "Grimoire - Gain 2 MU, add a free virus counter to installed virus programs"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Grimoire" 1) (qty "Imp" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Grimoire")
    (is (= 6 (:memory (get-runner))) "Gained 2 MU")
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (= 3 (:counter (refresh imp))) "Imp received an extra virus counter on install"))))

(deftest maya
  "Maya - Move accessed card to bottom of R&D"
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

(deftest plascrete
  "Plascrete Carapace - Prevent meat damage"
  (do-game
    (new-game (default-corp [(qty "Scorched Earth" 1)])
              (default-runner [(qty "Plascrete Carapace" 1) (qty "Sure Gamble" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Plascrete Carapace")
    (let [plas (get-in @state [:runner :rig :hardware 0])]
      (is (= 4 (:counter (refresh plas))) "4 counters on install")
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

(deftest the-personal-touch
  "The Personal Touch - Give +1 strength to an icebreaker"
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

(deftest turntable-swap
  "Turntable - Swap a stolen agenda for a scored agenda"
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
        (is (= true (:swap (core/get-card state tt)))) ; Turntable ability enabled by steal
        (card-ability state :runner tt 0)
        ;; Turntable prompt should be active
        (is (= (:cid tt) (-> @state :runner :prompt first :card :cid)))
        (prompt-select :runner (find-card "Project Vitruvius" (:scored (get-corp))))
        (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
        (is (= 0 (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")
        (is (nil? (:swap (core/get-card state tt))) "Turntable ability disabled")))))

(deftest turntable-mandatory-upgrades
  "Turntable - Swap a Mandatory Upgrades away from the Corp reduces Corp clicks per turn"
  (do-game
    (new-game (default-corp [(qty "Mandatory Upgrades" 1) (qty "Project Vitruvius" 1)])
              (default-runner [(qty "Turntable" 1)]))
    (play-from-hand state :corp "Mandatory Upgrades" "New remote")
    (let [manups (get-content state :remote1 0)]
      (score-agenda state :corp manups)
      (is (= 4 (:click-per-turn (get-corp))) "Up to 4 clicks per turn")
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (let [tt (get-in @state [:runner :rig :hardware 0])]
        (run-empty-server state "HQ")
        (prompt-choice :runner "Steal")
        (is (= 2 (:agenda-point (get-runner))) "Stole Project Vitruvius")
        (card-ability state :runner tt 0)
        (prompt-select :runner (find-card "Mandatory Upgrades" (:scored (get-corp))))
        (is (= 3 (:click-per-turn (get-corp))) "Back down to 3 clicks per turn")
        (is (nil? (:swap (core/get-card state tt))) "Turntable ability disabled")))))
