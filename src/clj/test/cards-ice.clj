(in-ns 'test.core)

(deftest end-the-run
  "Since all ETR ice share a common ability, we only need one test"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (core/click-run state :runner {:server "HQ"})
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp iwall)
      (card-ability state :corp iwall 0)
      (is (not (:run @state)) "Run is ended")
      (is (get-in @state [:runner :register :unsuccessful-run]) "Run was unsuccessful"))))

(deftest architect-untrashable
  "Architect is untrashable while installed and rezzed, but trashable if derezzed or from HQ"
  (do-game
    (new-game (default-corp [(qty "Architect" 3)])
              (default-runner))
    (play-from-hand state :corp "Architect" "HQ")
    (let [architect (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp architect)
      (core/trash state :corp (refresh architect))
      (is (not= nil (get-in @state [:corp :servers :hq :ices 0])) "Architect was trashed, but should be untrashable")
      (core/derez state :corp (refresh architect))
      (core/trash state :corp (refresh architect))
      (is (= nil (get-in @state [:corp :servers :hq :ices 0])) "Architect was not trashed, but should be trashable")
      (core/trash state :corp (get-in @state [:corp :hand 0]))
      (is (= (get-in @state [:corp :discard 0 :title]) "Architect"))
      (is (= (get-in @state [:corp :discard 1 :title]) "Architect"))
      )))

(deftest special-offer-trash-ice-during-run
  "Special Offer trashes itself and updates the run position"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1) (qty "Special Offer" 1)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Special Offer" "HQ")
    (take-credits state :corp 1)
    (core/click-run state :runner {:server "HQ"})
    (is (= 2 (:position (get-in @state [:run]))) "Initial position approaching Special Offer")
    (let [special (get-in @state [:corp :servers :hq :ices 1])]
      (core/rez state :corp special)
      (is (= 4 (:credit (get-corp))))
      (card-ability state :corp special 0)
      (is (= 9 (:credit (get-corp))) "Special Offer paid 5 credits")
      (is (= 1 (:position (get-in @state [:run]))) "Run position updated; now approaching Ice Wall"))))

(deftest tmi
  "TMI ICE test"
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (default-runner))
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp tmi)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (get-in (refresh tmi) [:rezzed]))
      )))

(deftest tmi-derez
  "TMI ICE trace derez"
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (make-deck "Sunny Lebeau: Security Specialist" [(qty "Blackmail" 3)]))
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp tmi)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (not (get-in (refresh tmi) [:rezzed])))
      )))
