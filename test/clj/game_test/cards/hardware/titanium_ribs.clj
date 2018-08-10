(ns game-test.cards.hardware.titanium-ribs
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest titanium-ribs
  ;; Titanium Ribs - Choose cards lost to damage, but not on Corp turn against Chronos Protocol
  (do-game
    (new-game {:corp {:id "Chronos Protocol: Selective Mind-mapping"
                      :deck ["Pup" "Viktor 1.0"
                             "Neural EMP"]}
               :runner {:deck [(qty "Titanium Ribs" 2) "Sure Gamble"
                               "Fall Guy" "Kati Jones"]}})
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Viktor 1.0" "R&D")
    (take-credits state :corp)
    (play-from-hand state :runner "Fall Guy")
    (play-from-hand state :runner "Titanium Ribs")
    (click-card state :runner (find-card "Titanium Ribs" (:hand (get-runner))))
    (click-card state :runner (find-card "Kati Jones" (:hand (get-runner))))
    (is (empty? (:prompt (get-runner))) "Fall Guy didn't try to prevent trashing of Kati")
    (is (= 2 (count (:discard (get-runner)))) "2 cards trashed for Ribs installation meat damage")
    (run-on state "HQ")
    (let [pup (get-ice state :hq 0)]
      (core/rez state :corp pup)
      (card-subroutine state :corp pup 0)
      (click-card state :runner (find-card "Sure Gamble" (:hand (get-runner)))) ; Ribs takes precedence over CP on Runner turn
      (is (= 3 (count (:discard (get-runner)))) "Chose card lost from 1 net damage")
      (run-jack-out state)
      (take-credits state :runner)
      (core/move state :runner (find-card "Sure Gamble" (:discard (get-runner))) :hand)
      (core/move state :runner (find-card "Kati Jones" (:discard (get-runner))) :hand)
      (play-from-hand state :corp "Neural EMP")
      (click-prompt state :corp "Yes")
      (let [kati (find-card "Kati Jones" (:hand (get-runner)))]
        (click-prompt state :corp kati) ; Chronos Protocol takes precedence over Ribs on Corp turn
        (is (= 2 (count (:discard (get-runner)))) "Card chosen by Corp for first net damage")))))
