(in-ns 'test.core)

(deftest cyberdex-virus-suite-purge
  "Cyberdex Virus Suite - Purge ability"
  (do-game
    (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
              (default-runner [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :corp "Cyberdex Virus Suite" "HQ")
    (take-credits state :corp 2)
    ; runner's turn
    ; install cache and medium
    (play-from-hand state :runner "Cache")
    (is (= 3 (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program])))))
    (play-from-hand state :runner "Medium")
    (take-credits state :runner 2)
    (let [cvs (first (get-in @state [:corp :servers :hq :content]))]
      (core/rez state :corp cvs)
      (card-ability state :corp cvs 0)
      ; nothing in hq content
      (is (empty? (get-in @state [:corp :servers :hq :content])) "CVS was trashed")
      ; purged counters
      (is (zero? (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program]))))
          "Cache has no counters")
      (is (zero? (core/get-virus-counters state :runner (find-card "Medium" (get-in @state [:runner :rig :program]))))
          "Medium has no counters"))))

(deftest cyberdex-virus-suite-access
  "Cyberdex Virus Suite - Purge on access"
  (do-game
    (new-game (default-corp [(qty "Cyberdex Virus Suite" 3)])
              (default-runner [(qty "Cache" 1) (qty "Medium" 1)]))
    (play-from-hand state :corp "Cyberdex Virus Suite" "New remote")
    (take-credits state :corp 2)
    ; runner's turn
    ; install cache and medium
    (play-from-hand state :runner "Cache")
    (is (= 3 (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program])))))
    (play-from-hand state :runner "Medium")
    (core/click-run state :runner {:server "Server 1"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    ; corp now has optional prompt to trigger virus purge
    (prompt-choice :corp "Yes")
    ; runner has prompt to trash CVS
    (prompt-choice :runner "Yes")
    ; purged counters
    (is (zero? (core/get-virus-counters state :runner (find-card "Cache" (get-in @state [:runner :rig :program]))))
        "Cache has no counters")
    (is (zero? (core/get-virus-counters state :runner (find-card "Medium" (get-in @state [:runner :rig :program]))))
        "Medium has no counters")))

(deftest old-hollywood-grid
  "Old Hollywood Grid - Ability"
  (do-game
    (new-game (default-corp [(qty "Old Hollywood Grid" 1) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Old Hollywood Grid" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)
    (let [ohg (find-card "Old Hollywood Grid" (get-in @state [:corp :servers :remote1 :content]))
          hok (find-card "House of Knives" (get-in @state [:corp :servers :remote1 :content]))]
      (core/click-run state :runner {:server "Server 1"})
      (core/rez state :corp ohg)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; runner now chooses which to access.
      (prompt-select :runner hok)
      ; prompt shows "You cannot steal"
      (prompt-choice :runner "OK")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas")
      (prompt-select :runner ohg)
      (prompt-choice :runner "No")
      (core/steal state :runner (find-card "House of Knives" (:hand (get-corp))))
      (core/click-run state :runner {:server "Server 1"})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 2 (count (:scored (get-runner)))) "2 scored agendas"))))

(deftest old-hollywood-grid-central
  "Old Hollywood Grid - Central server"
  (do-game
    (new-game (default-corp [(qty "Old Hollywood Grid" 1) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Old Hollywood Grid" "HQ")
    (take-credits state :corp 2)
    (let [ohg (find-card "Old Hollywood Grid" (get-in @state [:corp :servers :hq :content]))]
      (core/click-run state :runner {:server "HQ"})
      (core/rez state :corp ohg)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; runner now chooses which to access.
      (prompt-choice :runner "Card from hand")
      ; prompt shows "You cannot steal"
      (prompt-choice :runner "OK")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas"))))

(deftest red-herrings
  "Red Herrings - Ability"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-runner))
    (play-from-hand state :corp "Red Herrings" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (let [rh (find-card "Red Herrings" (get-in @state [:corp :servers :remote1 :content]))
          hok (find-card "House of Knives" (get-in @state [:corp :servers :remote1 :content]))]
      (core/click-run state :runner {:server "Server 1"})
      (core/rez state :corp rh)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; runner now chooses which to access.
      (prompt-select :runner hok)
      ; prompt should be asking for the 5cr cost
      (is (= "House of Knives" (:title (:card (first (:prompt (get-runner)))))) "Prompt to pay 5cr")
      (prompt-choice :runner "No")
      (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas")
      (prompt-select :runner rh)
      (prompt-choice :runner "No")
      (core/click-run state :runner {:server "Server 1"})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 0 (:credit (get-runner))) "Runner was charged 5cr")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest red-herrings-trash
  "Red Herrings - Cost increase even when trashed"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 3) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Red Herrings" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (core/gain state :runner :credit 1)
    (let [rh (find-card "Red Herrings" (get-in @state [:corp :servers :remote1 :content]))
          hok (find-card "House of Knives" (get-in @state [:corp :servers :remote1 :content]))]
      (core/click-run state :runner {:server "Server 1"})
      (core/rez state :corp rh)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; runner now chooses which to access.
      ;(prn (get-in @state [:events :pre-steal-cost]))
      (prompt-select :runner rh)
      (prompt-choice :runner "Yes") ; pay to trash
      ;(prn (get-in @state [:events :pre-steal-cost]))
      (prompt-select :runner hok)
      ; should now have prompt to pay 5cr for HoK
      (prompt-choice :runner "Yes")
      (is (= 0 (:credit (get-runner))) "Runner was charged 5cr")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest red-herrings-trash-from-hand
  "Red Herrings - Trashed from Hand"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-runner))
    (core/move-card state :corp {:card (find-card "Red Herrings" (:hand (get-corp))) :server "Archives"})
    (is (= 1 (count (:discard (get-corp)))) "1 card in Archives")
    (take-credits state :corp 2)

    (core/click-run state :runner {:server "HQ"})
    (core/no-action state :corp nil)
    (core/successful-run state :runner nil)
    ; prompt should be asking to steal HoK
    (prn (:prompt (get-runner)))
    (is (= "Steal" (first (:choices (first (:prompt (get-runner)))))) "Runner being asked to Steal")))

(deftest red-herrings-other-server
  "Red Herrings - Don't affect runs on other servers"
  (do-game
    (new-game (default-corp [(qty "Red Herrings" 1) (qty "House of Knives" 1)])
              (default-runner))
    (play-from-hand state :corp "Red Herrings" "New remote")
    (play-from-hand state :corp "House of Knives" "New remote")
    (take-credits state :corp 1)

    (let [rh (find-card "Red Herrings" (get-in @state [:corp :servers :remote1 :content]))]
      (core/click-run state :runner {:server "Server 2"})
      (core/rez state :corp rh)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      ; access is automatic
      (prompt-choice :runner "Steal")
      (is (= 5 (:credit (get-runner))) "Runner was not charged 5cr")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest strongbox
  "Strongbox - Ability"
  (do-game
    (new-game (default-corp [(qty "Strongbox" 1) (qty "House of Knives" 1)])
              (default-runner))
    (play-from-hand state :corp "Strongbox" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (let [sb (find-card "Strongbox" (get-in @state [:corp :servers :remote1 :content]))
          hok (find-card "House of Knives" (get-in @state [:corp :servers :remote1 :content]))]
      (core/click-run state :runner {:server "Server 1"})
      (core/rez state :corp sb)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-select :runner hok)
      (is (= "House of Knives" (:title (:card (first (:prompt (get-runner)))))) "Prompt to pay 5cr")
      (prompt-choice :runner "No")
      (is (= 3 (:click (get-runner))) "Runner was not charged 1click")
      (is (= 0 (count (:scored (get-runner)))) "No scored agendas")
      (prompt-select :runner sb)
      (prompt-choice :runner "No")
      (core/click-run state :runner {:server "Server 1"})
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 1 (:click (get-runner))) "Runner was charged 1click")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))

(deftest strongbox-trash
  "Strongbox - Click cost even when trashed"
  (do-game
    (new-game (default-corp [(qty "Strongbox" 3) (qty "House of Knives" 3)])
              (default-runner))
    (play-from-hand state :corp "Strongbox" "New remote")
    (play-from-hand state :corp "House of Knives" "Server 1")
    (take-credits state :corp 1)

    (core/gain state :runner :credit 1)
    (let [sb (find-card "Strongbox" (get-in @state [:corp :servers :remote1 :content]))
          hok (find-card "House of Knives" (get-in @state [:corp :servers :remote1 :content]))]
      (core/click-run state :runner {:server "Server 1"})
      (core/rez state :corp sb)
      (core/no-action state :corp nil)
      (core/successful-run state :runner nil)
      (prompt-select :runner sb)
      (prompt-choice :runner "Yes") ; pay to trash
      (prompt-select :runner hok)
      (prompt-choice :runner "Yes")
      (is (= 2 (:click (get-runner))) "Runner was charged 1click")
      (is (= 1 (count (:scored (get-runner)))) "1 scored agenda"))))