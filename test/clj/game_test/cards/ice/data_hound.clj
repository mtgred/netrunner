(ns game-test.cards.ice.data-hound
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest data-hound
  ;; Data Hound - Full test
  (do-game
    (new-game {:corp {:deck ["Data Hound"]}
               :runner {:deck [(qty "Sure Gamble" 2) "Desperado"
                               "Corroder" "Patron"]}})
    (starting-hand state :runner ["Sure Gamble"]) ;move all other cards to stack
    (play-from-hand state :corp "Data Hound" "HQ")
    (take-credits state :corp)
    (let [dh (get-ice state :hq 0)]
      (run-on state "HQ")
      (core/rez state :corp dh)
      (card-subroutine state :corp dh 0)
      (click-prompt state :corp "2")
      (click-prompt state :runner "0")
      ;; trash 1 card and rearrange the other 3
      (click-prompt state :corp (find-card "Desperado" (:deck (get-runner))))
      (is (= 1 (count (:discard (get-runner)))))
      (click-prompt state :corp (find-card "Sure Gamble" (:deck (get-runner))))
      (click-prompt state :corp (find-card "Corroder" (:deck (get-runner))))
      (click-prompt state :corp (find-card "Patron" (:deck (get-runner))))
      ;; try starting over
      (click-prompt state :corp "Start over")
      (click-prompt state :corp (find-card "Patron" (:deck (get-runner))))
      (click-prompt state :corp (find-card "Corroder" (:deck (get-runner))))
      (click-prompt state :corp (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
      (click-prompt state :corp "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Corroder" (:title (second (:deck (get-runner))))))
      (is (= "Patron" (:title (second (rest (:deck (get-runner)))))))
      (run-jack-out state)
      (run-on state "HQ")
      (card-subroutine state :corp dh 0)
      (click-prompt state :corp "0")
      (click-prompt state :runner "1")
      ;; trash the only card automatically
      (is (= 2 (count (:discard (get-runner)))))
      (is (= "Corroder" (:title (first (:deck (get-runner)))))))))
