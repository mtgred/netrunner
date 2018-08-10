(ns game-test.cards.hardware.sifr
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sifr
  ;; Sifr - Once per turn drop encountered ICE to zero strenght
  ;; Also handle archangel then re-install sifr should not break the game #2576
  (do-game
    (new-game {:corp {:deck ["Archangel" "IP Block" "Hedge Fund"]}
               :runner {:deck ["Modded" "Clone Chip" "Şifr" "Parasite"]}})
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
      (click-prompt state :corp "0")
      (is (not (empty? (:prompt (get-runner)))) "Archangel trace prompt - runner")
      (click-prompt state :runner "0")
      (click-card state :corp sifr)
      (is (= 3 (count (:hand (get-runner))))) ; sifr got lifted to hand
      (run-jack-out state)
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Modded")
      (is (not (empty? (:prompt (get-runner)))) "Modded choice prompt exists")
      (click-card state :runner (find-card "Şifr" (:hand (get-runner))))
      (is (= 4 (:current-strength (refresh ip))) "IP Block back to standard strength")
      (play-from-hand state :runner "Clone Chip")
      (take-credits state :runner)
      (take-credits state :corp 4)
      (let [chip (get-hardware state 1)]
        (is (nil? (:sifr-target (refresh sifr))) "Sifr cleaned up on leave play")
        (is (zero? (count (:discard (get-corp)))) "No Corp cards trashed")
        (card-ability state :runner chip 0)
        (click-card state :runner (find-card "Parasite" (:discard (get-runner))))
        (let [para (get-program state 0)]
          (click-card state :runner ip)
          (is (zero? (count (:discard (get-corp)))) "IP Block Not Trashed")
          (is (= 1 (count (:hosted (refresh ip)))) "Parasite is hosted"))))))
