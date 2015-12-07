(in-ns 'test.core)

(deftest account-siphon-ability
  "Account Siphon - Use ability"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :corp) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))))

    ; play Account Siphon, use ability
    (play-run-event state (first (:hand (get-runner))) :hq)
    (prompt-choice :runner "Run ability")
    (is (= 2 (:tag (get-runner)))) ; gained 2 tags
    (is (= 15 (:credit (get-runner)))) ; gained 10 credits
    (is (= 3 (:credit (get-corp)))))) ; corp lost 5 credits

(deftest account-siphon-access
  "Account Siphon - Access"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Account Siphon" 3)]))
    (take-credits state :corp) ; pass to runner's turn by taking credits
    (is (= 8 (:credit (get-corp))))
    ; play another Siphon, do not use ability
    (play-run-event state (first (get-in @state [:runner :hand])) :hq)
    (prompt-choice :runner "Access")
    (is (= 0 (:tag (get-runner)))) ; no new tags
    (is (= 5 (:credit (get-runner)))) ; no change in credits
    (is (= 8 (:credit (get-corp))))))

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
    (core/click-run state :runner {:server "Archives"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (core/click-run state :runner {:server "R&D"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (core/click-run state :runner {:server "HQ"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (play-from-hand state :runner "Apocalypse")
    (is (= 0 (count (core/all-installed state :corp))) "All installed Corp cards trashed")
    (is (= 3 (count (:discard (get-corp)))) "3 Corp cards in Archives")
    (let [tmc (get-in @state [:runner :rig :facedown 0])]
      (is (:facedown (refresh tmc)) "Tri-maf Contact is facedown")
      (is (= 3 (count (:hand (get-runner)))) "No meat damage dealt by Tri-maf's leave play effect"))))

(deftest demolition-run
  "Demolition Run - Trash at no cost"
  (do-game
    (new-game (default-corp [(qty "False Lead" 1) (qty "Shell Corporation" 1)(qty "Hedge Fund" 3)])
              (default-runner [(qty "Demolition Run" 1)]))
    (core/move state :corp (find-card "False Lead" (:hand (get-corp))) :deck) ; put False Lead back in R&D
    (play-from-hand state :corp "Shell Corporation" "R&D") ; install upgrade with a trash cost in root of R&D
    (take-credits state :corp 2) ; pass to runner's turn by taking credits
    (play-from-hand state :runner "Demolition Run")
    (is (= 3 (:credit (get-runner))) "Paid 2 credits for the event")
    (prompt-choice :runner "R&D")
    (is (= [:rd] (get-in @state [:run :server])) "Run initiated on R&D")
    (prompt-choice :runner "OK") ; dismiss instructional prompt for Demolition Run
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    (let [demo (get-in @state [:runner :play-area 0])] ; Demolition Run "hack" is to put it out in the play area
      (prompt-choice :runner "Unrezzed upgrade in R&D")
      (card-ability state :runner demo 0)
      (is (= 3 (:credit (get-runner))) "Trashed Shell Corporation at no cost")
      (prompt-choice :runner "Card from deck")
      (card-ability state :runner demo 0)  ; trash False Lead instead of stealing
      (is (= 0 (:agenda-point (get-runner))) "Didn't steal False Lead")
      (is (= 2 (count (:discard (get-corp)))) "2 cards in Archives")
      (is (empty? (:prompt (get-runner))) "Run concluded"))))

(deftest sure-gamble
  "Sure Gamble"
  (do-game
    (new-game (default-corp) (default-runner))
    (take-credits state :corp)
    (is (= 5 (:credit (get-runner))))
    (core/play state :runner {:card (first (:hand (get-runner)))})
    (is (= 9 (:credit (get-runner))))))

(deftest blackmail
  "Prevent rezzing of ice for one run"
  (do-game
    (new-game
      (default-corp [(qty "Ice Wall" 3)])
      (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is 1 (get-in @state [:corp :bad-publicity]))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (play-from-hand state :runner "Blackmail")
    (prompt-choice :runner "HQ")
    (let [iwall1 (get-in @state [:corp :servers :hq :ices 0])
          iwall2 (get-in @state [:corp :servers :hq :ices 1])]
      (core/rez state :corp iwall1)
      (is (not (get-in (refresh iwall1) [:rezzed])))
      (core/no-action state :corp nil)
      (core/continue state :runner nil)
      (core/rez state :corp iwall2)
      (is (not (get-in (refresh iwall2) [:rezzed])))
      (core/jack-out state :runner nil)
      ;Do another run, where the ice should rez
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp iwall1)
      (is (get-in (refresh iwall1) [:rezzed]))
    )))

(deftest blackmail-tmi-interaction
  "Regression test for a rezzed tmi breaking game state on a blackmail run"
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (make-deck "Valencia Estevez: The Angel of Cayambe" [(qty "Blackmail" 3)]))
    (is 1 (get-in @state [:corp :bad-publicity]))
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp tmi)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (get-in (refresh tmi) [:rezzed]))
      (take-credits state :corp)
      (play-from-hand state :runner "Blackmail")
      (prompt-choice :runner "HQ")
      (core/no-action state :corp nil)
      (core/continue state :runner nil)
      (core/jack-out state :runner nil)
      (core/click-run state :runner {:server "Archives"})
      )))