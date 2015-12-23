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
      (is (= (get-in @state [:corp :discard 1 :title]) "Architect")))))

(deftest curtain-wall
  "Curtain Wall - Strength boost when outermost ICE"
  (do-game
    (new-game (default-corp [(qty "Curtain Wall" 1) (qty "Paper Wall" 1)])
              (default-runner))
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Curtain Wall" "HQ")
    (let [curt (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp curt)
      (is (= 10 (:current-strength (refresh curt))) "Curtain Wall has +4 strength as outermost ICE")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (let [paper (get-in @state [:corp :servers :hq :ices 1])]
        (core/rez state :corp paper)
        (is (= 6 (:current-strength (refresh curt))) "Curtain Wall back to default 6 strength")))))

(deftest lotus-field-unlowerable
  "Lotus Field strength cannot be lowered"
  (do-game
    (new-game (default-corp [(qty "Lotus Field" 1) (qty "Lag Time" 1)])
              (default-runner [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :corp "Lotus Field" "Archives")
    (take-credits state :corp 2)
    (let [lotus (first (get-in @state [:corp :servers :archives :ices]))]
      (core/rez state :corp lotus)
      (play-from-hand state :runner "Ice Carver")
      (core/click-run state :runner {:server "Archives"})
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (core/jack-out state :runner nil)
      (play-from-hand state :runner "Parasite")
      (prompt-select :runner lotus)
      (is (= 1 (count (:hosted (refresh lotus)))) "Parasite hosted on Lotus Field")
      (take-credits state :runner 1)
      (take-credits state :corp)
      (is (= 1 (core/get-virus-counters state :runner (first (:hosted (refresh lotus))))) "Parasite has 1 virus counter")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (take-credits state :runner)
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased")
      (take-credits state :corp 2)
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased"))))

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
      (is (get-in (refresh tmi) [:rezzed])))))

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
      (is (not (get-in (refresh tmi) [:rezzed]))))))

(deftest wraparound
  "Wraparound - Strength boosted when no fracter is installed"
  (do-game
    (new-game (default-corp [(qty "Wraparound" 1)])
              (default-runner [(qty "Corroder" 1)]))
    (play-from-hand state :corp "Wraparound" "HQ")
    (let [wrap (get-in @state [:corp :servers :hq :ices 0])]
      (core/rez state :corp wrap)
      (is (= 7 (:current-strength (refresh wrap))) "Wraparound +7 strength with no fracter in play")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (is (= 0 (:current-strength (refresh wrap))) "Wraparound 0 strength after Corroder installed"))))
