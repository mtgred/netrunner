(in-ns 'test.core)

(deftest end-the-run
  "Since all ETR ice share a common ability, we only need one test"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 3) (qty "Hedge Fund" 3) (qty "Restructure" 2)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp 2)
    (run-on state "HQ")
    (is (= [:hq] (get-in @state [:run :server])))
    (let [iwall (get-ice state :hq 0)]
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
    (let [architect (get-ice state :hq 0)]
      (core/rez state :corp architect)
      (core/trash state :corp (refresh architect))
      (is (not= nil (get-ice state :hq 0)) "Architect was trashed, but should be untrashable")
      (core/derez state :corp (refresh architect))
      (core/trash state :corp (refresh architect))
      (is (= nil (get-ice state :hq 0)) "Architect was not trashed, but should be trashable")
      (core/trash state :corp (get-in @state [:corp :hand 0]))
      (is (= (get-in @state [:corp :discard 0 :title]) "Architect"))
      (is (= (get-in @state [:corp :discard 1 :title]) "Architect")))))

(deftest asteroid-belt
  "Asteroid Belt - Space ICE rez cost reduced by 3 credits per advancement"
  (do-game
    (new-game (default-corp [(qty "Asteroid Belt" 1)])
              (default-runner))
    (core/gain state :corp :credit 5)
    (play-from-hand state :corp "Asteroid Belt" "HQ")
    (let [ab (get-ice state :hq 0)]
      (core/advance state :corp {:card (refresh ab)})
      (core/advance state :corp {:card (refresh ab)})
      (is (= 8 (:credit (get-corp))))
      (is (= 2 (:advance-counter (refresh ab))))
      (core/rez state :corp (refresh ab))
      (is (= 5 (:credit (get-corp))) "Paid 3 credits to rez; 2 advancments on Asteroid Belt"))))

(deftest bandwidth
  "Bandwidth - Give the Runner 1 tag; remove 1 tag if the run is successful"
  (do-game
    (new-game (default-corp [(qty "Bandwidth" 1)])
              (default-runner))
    (play-from-hand state :corp "Bandwidth" "Archives")
    (let [bw (get-ice state :archives 0)]
      (take-credits state :corp)
      (run-on state "Archives")
      (core/rez state :corp bw)
      (card-ability state :corp bw 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
      (run-successful state)
      (is (= 0 (:tag (get-runner))) "Run successful; Runner lost 1 tag")
      (run-on state "Archives")
      (card-ability state :corp bw 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
      (run-jack-out state)
      (is (= 1 (:tag (get-runner))) "Run unsuccessful; Runner kept 1 tag"))))

(deftest cortex-lock
  "Cortex Lock - Do net damage equal to Runner's unused memory"
  (do-game
    (new-game (default-corp [(qty "Cortex Lock" 1)])
              (default-runner [(qty "Corroder" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Cortex Lock" "HQ")
    (take-credits state :corp)
    (let [cort (get-ice state :hq 0)]
      (play-from-hand state :runner "Corroder")
      (is (= 3 (:memory (get-runner))))
      (run-on state "HQ")
      (core/rez state :corp cort)
      (card-ability state :corp cort 0)
      (is (= 3 (count (:discard (get-runner)))) "Runner suffered 3 net damage"))))

(deftest curtain-wall
  "Curtain Wall - Strength boost when outermost ICE"
  (do-game
    (new-game (default-corp [(qty "Curtain Wall" 1) (qty "Paper Wall" 1)])
              (default-runner))
    (core/gain state :corp :credit 10)
    (play-from-hand state :corp "Curtain Wall" "HQ")
    (let [curt (get-ice state :hq 0)]
      (core/rez state :corp curt)
      (is (= 10 (:current-strength (refresh curt)))
          "Curtain Wall has +4 strength as outermost ICE")
      (play-from-hand state :corp "Paper Wall" "HQ")
      (let [paper (get-ice state :hq 1)]
        (core/rez state :corp paper)
        (is (= 6 (:current-strength (refresh curt))) "Curtain Wall back to default 6 strength")))))

(deftest draco
  "Dracō - Pay credits when rezzed to increase strength; trace to give 1 tag and end the run"
  (do-game
    (new-game (default-corp [(qty "Dracō" 1)])
              (default-runner))
    (play-from-hand state :corp "Dracō" "HQ")
    (take-credits state :corp)
    (let [drac (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp drac)
      (prompt-choice :corp 4)
      (is (= 4 (:counter (refresh drac))) "Dracō has 4 power counters")
      (is (= 4 (:current-strength (refresh drac))) "Dracō is 4 strength")
      (card-ability state :corp drac 0)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (= 1 (:tag (get-runner))) "Runner took 1 tag")
      (is (nil? (get-in @state [:run])) "Run was ended"))))

(deftest enigma
  "Enigma - Force Runner to lose 1 click if able"
  (do-game
    (new-game (default-corp [(qty "Enigma" 1)])
              (default-runner))
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (let [enig (get-ice state :hq 0)]
      (run-on state "HQ")
      (is (= 3 (:click (get-runner))))
      (core/rez state :corp enig)
      (card-ability state :corp enig 0)
      (is (= 2 (:click (get-runner))) "Runner lost 1 click"))))

(deftest fenris
  "Fenris - Illicit ICE give Corp 1 bad publicity when rezzed"
  (do-game
    (new-game (default-corp [(qty "Fenris" 1)])
              (default-runner))
    (play-from-hand state :corp "Fenris" "HQ")
    (take-credits state :corp)
    (let [fen (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp fen)
      (is (= 1 (:bad-publicity (get-corp))) "Gained 1 bad pub")
      (card-ability state :corp fen 0)
      (is (= 1 (:brain-damage (get-runner))) "Runner took 1 brain damage")
      (is (= 1 (count (:discard (get-runner)))))
      (is (= 4 (core/hand-size state :runner))))))

(deftest gemini-kicker
  "Gemini - Successfully trace to do 1 net damage; do 1 net damage if trace strength is 5 or more regardless of success"
  (do-game
    (new-game (default-corp [(qty "Gemini" 1) (qty "Hedge Fund" 2)])
              (default-runner [(qty "Sure Gamble" 3) (qty "Dirty Laundry" 2)]))
    (play-from-hand state :corp "Gemini" "HQ")
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (take-credits state :corp)
    (let [gem (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp gem)
      (card-ability state :corp gem 0)
      (prompt-choice :corp 3) ; boost to trace strength 5
      (prompt-choice :runner 0)
      (is (= 2 (count (:discard (get-runner)))) "Did 2 net damage")
      (card-ability state :corp gem 0)
      (prompt-choice :corp 3) ; boost to trace strength 5
      (prompt-choice :runner 5) ; match trace
      (is (= 3 (count (:discard (get-runner)))) "Did only 1 net damage for having trace strength 5 or more"))))

(deftest lockdown
  "Lockdown - Prevent Runner from drawing cards for the rest of the turn"
  (do-game
    (new-game (default-corp [(qty "Lockdown" 1)])
              (default-runner [(qty "Diesel" 2) (qty "Sure Gamble" 3)]))
    (play-from-hand state :corp "Lockdown" "R&D")
    (take-credits state :corp)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (core/move state :runner (find-card "Sure Gamble" (:hand (get-runner))) :deck)
    (let [lock (get-ice state :rd 0)]
      (run-on state "R&D")
      (core/rez state :corp lock)
      (card-ability state :corp lock 0)
      (run-successful state)
      (play-from-hand state :runner "Diesel")
      (is (= 1 (count (:hand (get-runner)))) "No cards drawn")
      (take-credits state :runner)
      (take-credits state :corp)
      (play-from-hand state :runner "Diesel")
      (is (= 3 (count (:hand (get-runner))))
          "New turn ends prevention; remaining 3 cards drawn from Stack"))))

(deftest lotus-field-unlowerable
  "Lotus Field strength cannot be lowered"
  (do-game
    (new-game (default-corp [(qty "Lotus Field" 1) (qty "Lag Time" 1)])
              (default-runner [(qty "Ice Carver" 1) (qty "Parasite" 1)]))
    (play-from-hand state :corp "Lotus Field" "Archives")
    (take-credits state :corp 2)
    (let [lotus (get-ice state :archives 0)]
      (core/rez state :corp lotus)
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (run-jack-out state)
      (play-from-hand state :runner "Parasite")
      (prompt-select :runner lotus)
      (is (= 1 (count (:hosted (refresh lotus)))) "Parasite hosted on Lotus Field")
      (take-credits state :runner 1)
      (take-credits state :corp)
      (is (= 1 (core/get-virus-counters state :runner (first (:hosted (refresh lotus)))))
          "Parasite has 1 virus counter")
      (is (= 4 (:current-strength (refresh lotus))) "Lotus Field strength unchanged")
      (take-credits state :runner)
      (play-from-hand state :corp "Lag Time")
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased")
      (take-credits state :corp 2)
      (is (= 5 (:current-strength (refresh lotus))) "Lotus Field strength increased"))))

(deftest morph-ice-subtype-changing
  "Morph ice gain and lose subtypes from normal advancements and placed advancements"
  (do-game
    (new-game (default-corp [(qty "Wendigo" 1)
                             (qty "Shipment from SanSan" 1)
                             (qty "Superior Cyberwalls" 1)])
              (default-runner))
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Superior Cyberwalls" "New remote")
    (let [sc (get-content state :remote1 0)]
      (score-agenda state :corp sc)
      (play-from-hand state :corp "Wendigo" "HQ")
      (let [wend (get-ice state :hq 0)]
        (core/rez state :corp wend)
        (is (= 4 (:current-strength (refresh wend))) "Wendigo at normal 4 strength")
        (core/advance state :corp {:card (refresh wend)})
        (is (= true (has? (refresh wend) :subtype "Barrier")) "Wendigo gained Barrier")
        (is (= false (has? (refresh wend) :subtype "Code Gate")) "Wendigo lost Code Gate")
        (is (= 5 (:current-strength (refresh wend))) "Wendigo boosted to 5 strength by scored Superior Cyberwalls")
        (play-from-hand state :corp "Shipment from SanSan")
        (prompt-choice :corp "1")
        (prompt-select :corp wend)
        (is (= false (has? (refresh wend) :subtype "Barrier")) "Wendigo lost Barrier")
        (is (= true (has? (refresh wend) :subtype "Code Gate")) "Wendigo gained Code Gate")
        (is (= 4 (:current-strength (refresh wend))) "Wendigo returned to normal 4 strength")))))

(deftest next-bronze
  "NEXT Bronze - Add 1 strength for every rezzed NEXT ice"
  (do-game
    (new-game (default-corp [(qty "NEXT Bronze" 2) (qty "NEXT Silver" 1)])
              (default-runner))
    (core/gain state :corp :credit 2)
    (play-from-hand state :corp "NEXT Bronze" "HQ")
    (play-from-hand state :corp "NEXT Bronze" "R&D")
    (play-from-hand state :corp "NEXT Silver" "Archives")
    (let [nb1 (get-ice state :hq 0)
          nb2 (get-ice state :rd 0)
          ns1 (get-ice state :archives 0)]
      (core/rez state :corp nb1)
      (is (= 1 (:current-strength (refresh nb1)))
          "NEXT Bronze at 1 strength: 1 rezzed NEXT ice")
      (core/rez state :corp nb2)
      (is (= 2 (:current-strength (refresh nb1)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (is (= 2 (:current-strength (refresh nb2)))
          "NEXT Bronze at 2 strength: 2 rezzed NEXT ice")
      (core/rez state :corp ns1)
      (is (= 3 (:current-strength (refresh nb1)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice")
      (is (= 3 (:current-strength (refresh nb2)))
          "NEXT Bronze at 3 strength: 3 rezzed NEXT ice"))))

(deftest special-offer-trash-ice-during-run
  "Special Offer trashes itself and updates the run position"
  (do-game
    (new-game (default-corp [(qty "Ice Wall" 1) (qty "Special Offer" 1)])
              (default-runner))
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Special Offer" "HQ")
    (take-credits state :corp 1)
    (run-on state "HQ")
    (is (= 2 (:position (get-in @state [:run]))) "Initial position approaching Special Offer")
    (let [special (get-ice state :hq 1)]
      (core/rez state :corp special)
      (is (= 4 (:credit (get-corp))))
      (card-ability state :corp special 0)
      (is (= 9 (:credit (get-corp))) "Special Offer paid 5 credits")
      (is (= 1 (:position (get-in @state [:run])))
          "Run position updated; now approaching Ice Wall"))))

(deftest tmi
  "TMI ICE test"
  (do-game
    (new-game (default-corp [(qty "TMI" 3)])
              (default-runner))
    (play-from-hand state :corp "TMI" "HQ")
    (let [tmi (get-ice state :hq 0)]
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
    (let [tmi (get-ice state :hq 0)]
      (core/rez state :corp tmi)
      (prompt-choice :corp 0)
      (prompt-choice :runner 0)
      (is (not (get-in (refresh tmi) [:rezzed]))))))

(deftest turing-positional-strength
  "Turing - Strength boosted when protecting a remote server"
  (do-game
    (new-game (default-corp [(qty "Turing" 2) (qty "Hedge Fund" 1)])
              (default-runner))
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Turing" "HQ")
    (play-from-hand state :corp "Turing" "New remote")
    (let [t1 (get-ice state :hq 0)
          t2 (get-ice state :remote1 0)]
      (core/rez state :corp t1)
      (is (= 2 (:current-strength (refresh t1)))
          "Turing default 2 strength over a central server")
      (core/rez state :corp t2)
      (is (= 5 (:current-strength (refresh t2)))
          "Turing increased to 5 strength over a remote server"))))

(deftest wraparound
  "Wraparound - Strength boosted when no fracter is installed"
  (do-game
    (new-game (default-corp [(qty "Wraparound" 1)])
              (default-runner [(qty "Corroder" 1)]))
    (play-from-hand state :corp "Wraparound" "HQ")
    (let [wrap (get-ice state :hq 0)]
      (core/rez state :corp wrap)
      (is (= 7 (:current-strength (refresh wrap)))
          "Wraparound +7 strength with no fracter in play")
      (take-credits state :corp)
      (play-from-hand state :runner "Corroder")
      (is (= 0 (:current-strength (refresh wrap)))
          "Wraparound 0 strength after Corroder installed"))))
