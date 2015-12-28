(in-ns 'test.core)

(deftest astrolabe-memory
  "Astrolabe - Gain 1 memory"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Astrolabe" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (is (= 5 (:memory (get-runner))) "Gain 1 memory")))

(deftest astrolabe-draw
  "Astrolabe - Draw on new server install"
  (do-game
    (new-game (default-corp [(qty "Snare!" 3) (qty "Shock!" 3) (qty "Project Junebug" 3)])
              (default-runner [(qty "Astrolabe" 3) (qty "Sure Gamble" 3) (qty "Cloak" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Astrolabe")
    (take-credits state :runner 3)
    ; corp's turn. install something from HQ to trigger Astrolabe draw
    (core/play state :corp {:card (first (:hand (get-corp))) :server "New remote"})
    (is (= 5 (count (:hand (get-runner)))) "Drew 1 card from server install")
    ; install over the old server; make sure nothing is drawn
    (core/play state :corp {:card (first (:hand (get-corp))) :server "Server 0"})
    (is (= 5 (count (:hand (get-runner)))) "Did not draw")
    (is (= 1 (count (:deck (get-runner)))) "1 card left in deck")))

(deftest brain-chip
  "Brain Chip handsize and memory limit"
  (do-game
   (new-game (default-corp) (default-runner [(qty "Brain Chip" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Brain Chip")
   (swap! state assoc-in [:runner :agenda-point] -2) ; hard set ap
   (is (= (get-in @state [:runner :max-hand-size]) 5) "Hand size unaffected")
   (is (= (get-in @state [:runner :memory]) 4) "Memory limit unaffected")
   (swap! state assoc-in [:runner :agenda-point] 2)
   (is (= (get-in @state [:runner :max-hand-size]) 7) "Hand size increased by 2")
   (is (= (get-in @state [:runner :memory]) 6) "Memory limit increased by 2")
   (core/move state :runner (get-in @state [:runner :rig :hardware 0]) :discard)
   (is (= (get-in @state [:runner :max-hand-size]) 5) "Hand size reset")
   (is (= (get-in @state [:runner :memory]) 4) "Memory limit reset")))

(deftest clone-chip
  "Test clone chip usage- outside and during run"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Datasucker" 1) (qty "Clone Chip" 2)]))
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
    (new-game (default-corp) (default-runner [(qty "Comet" 3) (qty "Easy Mark" 2)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Comet")
    (let [comet (get-in @state [:runner :rig :hardware 0])]
      (play-from-hand state :runner "Easy Mark")
      (is (= true (:comet-event (core/get-card state comet)))) ; Comet ability enabled
      (card-ability state :runner comet 0)
      (is (= (:cid comet) (get-in @state [:runner :prompt 0 :card :cid])))
      (core/select state :runner {:card (find-card "Easy Mark" (:hand (get-runner)))})
      (is (= 7 (:credit (get-runner))))
      (is (= 2 (:click (get-runner))))
      (is (nil? (:comet-event (core/get-card state comet))) "Comet ability disabled"))))

(deftest dinosaurus-strength-boost-mu-savings
  "Dinosaurus - Boost strength of hosted icebreaker; keep MU the same when hosting or trashing hosted breaker"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Dinosaurus" 1) (qty "Battering Ram" 1)]))
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Dinosaurus")
    (let [dino (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner dino 0)
      (core/select state :runner {:card (find-card "Battering Ram" (:hand (get-runner)))})
      (is (= 2 (:click (get-runner))))
      (is (= 0 (:credit (get-runner))))
      (is (= 4 (:memory (get-runner))) "Battering Ram 2 MU not deducted from available MU")
      (let [ram (first (:hosted (refresh dino)))]
        (is (= 5 (:current-strength (refresh ram))) "Dinosaurus giving +2 strength to Battering Ram")
        (core/move state :runner (find-card "Battering Ram" (:hosted (refresh dino))) :discard) ; trash Battering Ram
        (is (= 4 (:memory (get-runner))) "Battering Ram 2 MU not added to available MU")))))

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

(deftest turntable-swap
  "Turntable - Swap a stolen agenda for a scored agenda"
  (do-game
    (new-game (default-corp [(qty "Domestic Sleepers" 1) (qty "Project Vitruvius" 1)])
              (default-runner [(qty "Turntable" 1)]))
    (play-from-hand state :corp "Project Vitruvius" "New remote")
    (let [ag1 (get-in @state [:corp :servers :remote1 :content 0])]
      (core/gain state :corp :click 1)
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/advance state :corp {:card (refresh ag1)})
      (core/score state :corp {:card (refresh ag1)})
      (take-credits state :corp)
      (play-from-hand state :runner "Turntable")
      (is (= 3 (:credit (get-runner))))
      (let [tt (get-in @state [:runner :rig :hardware 0])]
        (core/click-run state :runner {:server "HQ"})
        (core/no-action state :corp nil)
        (core/successful-run state :runner nil)
        (prompt-choice :runner "Steal")
        (is (= 0 (:agenda-point (get-runner))) "Stole Domestic Sleepers")
        (is (= true (:swap (core/get-card state tt)))) ; Turntable ability enabled by steal
        (card-ability state :runner tt 0)
        ; Turntable prompt should be active
        (is (= (:cid tt) (get-in @state [:runner :prompt 0 :card :cid])))
        (prompt-choice :runner "Yes")
        (core/select state :runner {:card (find-card "Project Vitruvius" (:scored (get-corp)))})
        (is (= 2 (:agenda-point (get-runner))) "Took Project Vitruvius from Corp")
        (is (= 0 (:agenda-point (get-corp))) "Swapped Domestic Sleepers to Corp")
        (is (nil? (:swap (core/get-card state tt))) "Turntable ability disabled")))))
