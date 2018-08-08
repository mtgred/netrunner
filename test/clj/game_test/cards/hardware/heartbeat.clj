(ns game-test.cards.hardware.heartbeat
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest heartbeat
  ;; Heartbeat - +1 MU, trash installed card to prevent 1 damage
  (do-game
    (new-game {:corp {:deck ["Pup" "Neural Katana"]}
               :runner {:id "Apex: Invasive Predator"
                        :deck [(qty "Heartbeat" 2) (qty "Sure Gamble" 2) "Cache"]}})
    (play-from-hand state :corp "Pup" "HQ")
    (play-from-hand state :corp "Neural Katana" "R&D")
    (take-credits state :corp)
    (core/end-phase-12 state :runner nil)
    (click-card state :runner (find-card "Heartbeat" (:hand (get-runner))))
    (play-from-hand state :runner "Heartbeat")
    (is (= 5 (core/available-mu state)) "Gained 1 MU")
    (play-from-hand state :runner "Cache")
    (let [hb (get-hardware state 0)
          cache (get-program state 0)
          hbdown (get-runner-facedown state 0)
          pup (get-ice state :hq 0)
          nk (get-ice state :rd 0)]
      (core/rez state :corp pup)
      (core/rez state :corp nk)
      (card-subroutine state :corp (refresh pup) 0)
      (card-ability state :runner hb 0)
      (click-card state :runner cache)
      (click-prompt state :runner "Done")
      (is (= 1 (count (:discard (get-runner)))) "Prevented 1 net damage")
      (is (= 2 (count (:hand (get-runner)))))
      (card-subroutine state :corp (refresh nk) 0)
      (card-ability state :runner hb 0)
      (click-card state :runner hbdown)
      (click-prompt state :runner "Done")
      (is (= 4 (count (:discard (get-runner)))) "Prevented 1 of 3 net damage; used facedown card"))))
