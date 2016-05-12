(ns test.cards.icebreakers
  (:require [game.core :as core]
            [test.core :refer :all]
            [test.utils :refer :all]
            [test.macros :refer :all]
            [clojure.test :refer :all]))


(deftest atman-install-0
  "Atman - Installing with 0 power counters"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (prompt-choice :runner 0)
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 0 (:counter atman)) "0 power counters")
      (is (= 0 (:current-strength atman)) "0 current strength"))))

(deftest atman-install-2
  "Atman - Installing with 2 power counters"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Atman" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Atman")
    (prompt-choice :runner 2)
    (is (= 3 (:memory (get-runner))))
    (let [atman (get-in @state [:runner :rig :program 0])]
      (is (= 2 (:counter atman)) "2 power counters")
      (is (= 2 (:current-strength atman)) "2 current strength"))))

(deftest chameleon-clonechip
  "Chameleon - Install on corp turn, only returns to hand at end of runner's turn"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Chameleon" 1) (qty "Clone Chip" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Clone Chip")
    (core/move state :runner (find-card "Chameleon" (:hand (get-runner))) :discard)
    (take-credits state :runner)
    (is (= 0 (count (:hand (get-runner)))))
    ; Install Chameleon on corp turn
    (take-credits state :corp 1)
    (let [chip (get-in @state [:runner :rig :hardware 0])]
      (card-ability state :runner chip 0)
      (prompt-select :runner (find-card "Chameleon" (:discard (get-runner))))
      (prompt-choice :runner "Sentry"))
    (take-credits state :corp)
    (is (= 0 (count (:hand (get-runner)))) "Chameleon not returned to hand at end of corp turn")
    (take-credits state :runner)
    (is (= 1 (count (:hand (get-runner)))) "Chameleon returned to hand at end of runner's turn")))

(deftest chameleon-scheherazade
  "Chameleon - Returns to hand after hosting. #977"
  (do-game
    (new-game (default-corp) (default-runner [(qty "Chameleon" 2) (qty "Scheherazade" 1)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Chameleon")
    (prompt-choice :runner "Barrier")
    (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon")
    ; Host the Chameleon on Scheherazade that was jsut played (as in Personal Workshop/Hayley ability scenarios)
    (play-from-hand state :runner "Scheherazade")
    (let [scheherazade (get-in @state [:runner :rig :program 1])]
      (card-ability state :runner scheherazade 1) ; Host an installed program
      (prompt-select :runner (find-card "Chameleon" (:program (:rig (get-runner)))))
      (is (= 4 (:credit (get-runner))) "+1 from hosting onto Scheherazade")
      ; Install another Chameleon directly onto Scheherazade
      (card-ability state :runner scheherazade 0) ; Install and host a program from Grip
      (prompt-select :runner (find-card "Chameleon" (:hand (get-runner))))
      (prompt-choice :runner "Code Gate")
      (is (= 2 (count (:hosted (refresh scheherazade)))) "2 Chameleons hosted on Scheherazade")
      (is (= 3 (:credit (get-runner))) "-2 from playing Chameleon, +1 from installing onto Scheherazade"))
    (is (= 0 (count (:hand (get-runner)))) "Both Chameleons in play - hand size 0")
    (take-credits state :runner)
    (is (= 2 (count (:hand (get-runner)))) "Both Chameleons returned to hand - hand size 2")))

(deftest cerberus
  "Cerberus - boost 1 for 1 cred. Break for 1 counter"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Cerberus \"Rex\" H2" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Cerberus \"Rex\" H2")
   (is (= 2 (:credit (get-runner))) "2 credits left after install")
   (let [rex (get-in @state [:runner :rig :program 0])]
     (is (= 4 (:counter rex)) "Start with 4 counters")
     ;; boost strength
     (card-ability state :runner rex 1)
     (is (= 1 (:credit (get-runner))) "Spend 1 credit to boost")
     (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
     ;; break
     (card-ability state :runner rex 0)
     (is (= 1 (:credit (get-runner))) "No credits spent to break")
     (is (= 3 (:counter (refresh rex))) "One counter used to break"))))

(deftest faust-pump
  "Faust - Pump by discarding"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Faust" 1) (qty "Sure Gamble" 3)]))
    (take-credits state :corp)
    (play-from-hand state :runner "Faust")
    (let [faust (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner faust 1)
      (prompt-card :runner (first (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh faust))) "4 current strength")
      (is (= 1 (count (:discard (get-runner)))) "1 card trashed"))))

(deftest faust-pump
  "Faust - Pump does not trigger trash prevention. #760"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Faust" 1)
                               (qty "Sacrificial Construct" 1)
                               (qty "Fall Guy" 1)
                               (qty "Astrolabe" 1)
                               (qty "Gordian Blade" 1 )
                               (qty "Armitage Codebusting" 1)]))
    (take-credits state :corp)
    (core/draw state :runner 1)
    (play-from-hand state :runner "Faust")
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Sacrificial Construct")
    (is (= 2 (count (get-in @state [:runner :rig :resource]))) "Resources installed")
    (let [faust (get-in @state [:runner :rig :program 0])]
      (card-ability state :runner faust 1)
      (prompt-card :runner (find-card "Astrolabe" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for hardware")
      (card-ability state :runner faust 1)
      (prompt-card :runner (find-card "Gordian Blade" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for program")
      (card-ability state :runner faust 1)
      (prompt-card :runner (find-card "Armitage Codebusting" (:hand (get-runner))))
      (is (empty? (:prompt (get-runner))) "No trash-prevention prompt for resource"))))

(deftest femme-counter
  "Femme Fatale counter test"
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Femme Fatale" 2)]))
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (core/gain state :runner :credit 18)
   (let [iw (get-ice state :hq 0)]
    (play-from-hand state :runner "Femme Fatale")
    (prompt-select :runner iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :runner (get-in @state [:runner :rig :program 0]))
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after Femme trashed")
    (play-from-hand state :runner "Femme Fatale")
    (prompt-select :runner iw)
    (is (:icon (refresh iw)) "Ice Wall has an icon")
    (core/trash state :corp iw)
    (is (not (:icon (refresh iw))) "Ice Wall does not have an icon after itself trashed"))))

(deftest overmind-counters
  "Overmind - Start with counters equal to unused MU"
  (do-game
    (new-game (default-corp)
              (default-runner [(qty "Overmind" 1) (qty "Akamatsu Mem Chip" 2)]))
    (take-credits state :corp)
    (take-credits state :runner 1)
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (play-from-hand state :runner "Akamatsu Mem Chip")
    (is (= 6 (:memory (get-runner))))
    (play-from-hand state :runner "Overmind")
    (is (= 5 (:memory (get-runner))))
    (let [ov (get-in @state [:runner :rig :program 0])]
      (is (= 5 (:counter (refresh ov))) "Overmind has 5 counters"))))

(deftest shiv
  "Shiv - Gain 1 strength for each installed breaker; no MU cost when 2+ link"
  (do-game
    (new-game
      (default-corp)
      (make-deck "Nasir Meidan: Cyber Explorer" [(qty "Shiv" 1) (qty "Inti" 2)
                                                 (qty "Access to Globalsec" 1)]))
    (is (= 1 (:link (get-runner))) "1 link")
    (take-credits state :corp)
    (play-from-hand state :runner "Shiv")
    (let [shiv (get-program state 0)]
      (is (= 1 (:current-strength (refresh shiv))) "1 installed breaker; 1 strength")
      (play-from-hand state :runner "Inti")
      (is (= 2 (:current-strength (refresh shiv))) "2 installed breakers; 2 strength")
      (play-from-hand state :runner "Inti")
      (is (= 3 (:current-strength (refresh shiv))) "3 installed breakers; 3 strength")
      (is (= 1 (:memory (get-runner))) "3 MU consumed")
      (play-from-hand state :runner "Access to Globalsec")
      (is (= 2 (:link (get-runner))) "2 link")
      (is (= 2 (:memory (get-runner))) "Shiv stops using MU when 2+ link"))))

(deftest snowball
  "Snowball - Strength boost until end of run when used to break a subroutine"
  (do-game
   (new-game (default-corp [(qty "Spiderweb" 1) (qty "Fire Wall" 1) (qty "Hedge Fund" 1)])
             (default-runner [(qty "Snowball" 1)]))
   (play-from-hand state :corp "Hedge Fund")
   (play-from-hand state :corp "Fire Wall" "HQ")
   (play-from-hand state :corp "Spiderweb" "HQ")
   (take-credits state :corp)
   (core/gain state :runner :credit 10)
   (play-from-hand state :runner "Snowball")
   (let [sp (get-ice state :hq 1)
         fw (get-ice state :hq 0)
         snow (get-program state 0)]
     (run-on state "HQ")
     (core/rez state :corp sp)
     (core/rez state :corp fw)
     (card-ability state :runner snow 1) ; match strength
     (is (= 2 (:current-strength (refresh snow))))
     (card-ability state :runner snow 0) ; strength matched, break a sub
     (card-ability state :runner snow 0) ; break a sub
     (is (= 4 (:current-strength (refresh snow))) "Broke 2 subs, gained 2 more strength")
     (run-continue state)
     (is (= 3 (:current-strength (refresh snow))) "Has +2 strength until end of run; lost 1 per-encounter boost")
     (card-ability state :runner snow 1)
     (card-ability state :runner snow 1) ; match strength
     (is (= 5 (:current-strength (refresh snow))) "Matched strength, gained 2")
     (card-ability state :runner snow 0) ; strength matched, break a sub
     (is (= 6 (:current-strength (refresh snow))) "Broke 1 sub, gained 1 more strength")
     (run-continue state)
     (is (= 4 (:current-strength (refresh snow))) "+3 until-end-of-run strength")
     (run-jack-out state)
     (is (= 1 (:current-strength (refresh snow))) "Back to default strength"))))

(deftest study-guide
  "Study Guide - 2c to add a power counter; +1 strength per counter"
  (do-game
   (new-game (default-corp)
             (default-runner [(qty "Study Guide" 1) (qty "Sure Gamble" 1)]))
   (take-credits state :corp)
   (play-from-hand state :runner "Sure Gamble")
   (play-from-hand state :runner "Study Guide")
   (let [sg (get-program state 0)]
     (card-ability state :runner sg 1)
     (is (= 4 (:credit (get-runner))) "Paid 2c")
     (is (= 1 (:counter (refresh sg))) "Has 1 power counter")
     (is (= 1 (:current-strength (refresh sg))) "1 strength")
     (card-ability state :runner sg 1)
     (is (= 2 (:credit (get-runner))) "Paid 2c")
     (is (= 2 (:counter (refresh sg))) "Has 2 power counters")
     (is (= 2 (:current-strength (refresh sg))) "2 strength"))))

(deftest wyrm
  "Wyrm reduces strength of ice"
  (do-game
   (new-game (default-corp [(qty "Ice Wall" 1)])
             (default-runner [(qty "Wyrm" 1)]))
   (play-from-hand state :corp "Ice Wall" "HQ")
   (take-credits state :corp)
   (play-from-hand state :runner "Wyrm")
   (run-on state "HQ")
   (let [ice-wall (get-ice state :hq 0)
         wyrm (get-in @state [:runner :rig :program 0])]
     (core/rez state :corp ice-wall)
     (card-ability state :runner wyrm 1)
     (is (= 0 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to 0")
     (card-ability state :runner wyrm 1)
     (is (= -1 (:current-strength (refresh ice-wall))) "Strength of Ice Wall reduced to -1"))))
