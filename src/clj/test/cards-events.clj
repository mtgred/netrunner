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

;Surge and virus counter flag tests
(deftest virus-counter-flag-on-enter
  "Set counter flag when virus card enters play with counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Surge" 1) (qty "Imp" 1) (qty "Crypsis" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (get-in imp [:added-virus-counter]) "Counter flag was not set on Imp")
      )))

(deftest virus-counter-flag-on-add-prop
  "Set counter flag when add-prop is called on a virus"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Crypsis" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner crypsis 2) ;click to add a virus counter
      (is (= 1 (get-in (refresh crypsis) [:counter])) "Crypsis did not add a virus token")
      (is (get-in (refresh crypsis) [:added-virus-counter] "Counter flag was not set on Crypsis"))
      )))

(deftest virus-counter-flag-clear-on-end-turn
  "Clear the virus counter flag at the end of each turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Crypsis" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Crypsis")
    (let [crypsis (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner crypsis 2) ;click to add a virus counter
      (take-credits state :runner 2)
      (take-credits state :corp 1)
      (is (not (get-in (refresh crypsis) [:added-virus-counter])) "Counter flag was not cleared on Crypsis")
      )))

(deftest surge-valid-target
  "Add counters if target is a virus and had a counter added this turn"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (= 2 (get-in imp [:counter])))
      (play-from-hand state :runner "Surge")
      (prompt-select :runner imp)
      (is (= 4 (get-in (refresh imp) [:counter])))
      )))

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
      (is (not (contains? st :counter)))
      )
    ))

(deftest surge-target-no-token-this-turn
  "Don't fire surge if target does not have virus counter flag set"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Imp" 1) (qty "Surge" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Imp")
    (let [imp (get-in @state [:runner :rig :program 0])]
      (is (= 2 (get-in imp [:counter])))
      (take-credits state :runner 2)
      (take-credits state :corp)
      (play-from-hand state :runner "Surge")
      (prompt-select :runner imp)
      (is (= 2 (get-in (refresh imp) [:counter])))
      )))