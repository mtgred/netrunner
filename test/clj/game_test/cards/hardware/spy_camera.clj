(ns game-test.cards.hardware.spy-camera
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest spy-camera
  ;; Spy Camera - Full test
  (do-game
    (new-game {:runner {:deck [(qty "Spy Camera" 6) "Sure Gamble" "Desperado"
                               "Diesel" "Corroder" "Patron" "Kati Jones"]}})
    (starting-hand state :runner ["Spy Camera" "Spy Camera" "Spy Camera"
                                  "Spy Camera" "Spy Camera" "Spy Camera"])
    (is (= 6 (count (:hand (get-runner)))))
    (core/move state :corp (find-card "Hedge Fund" (:hand (get-corp))) :deck)
    (take-credits state :corp)
    (core/gain state :runner :click 3)
    (dotimes [_ 6] (play-from-hand state :runner "Spy Camera"))
    (let [spy (get-hardware state 5)]
      ;; look at top 6 cards
      (card-ability state :runner spy 0)
      (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Kati Jones" (:deck (get-runner))))
      ;; try starting over
      (click-prompt state :runner "Start over")
      (click-prompt state :runner (find-card "Kati Jones" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Patron" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Corroder" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Diesel" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Desperado" (:deck (get-runner))))
      (click-prompt state :runner (find-card "Sure Gamble" (:deck (get-runner)))) ;this is the top card on stack
      (click-prompt state :runner "Done")
      (is (= "Sure Gamble" (:title (first (:deck (get-runner))))))
      (is (= "Desperado" (:title (second (:deck (get-runner))))))
      (is (= "Diesel" (:title (second (rest (:deck (get-runner)))))))
      (is (= "Corroder" (:title (second (rest (rest (:deck (get-runner))))))))
      (is (= "Patron" (:title (second (rest (rest (rest (:deck (get-runner)))))))))
      (is (= "Kati Jones" (:title (second (rest (rest (rest (rest (:deck (get-runner))))))))))
      ;; look at top card of R&D
      (card-ability state :runner spy 1)
      (let [topcard (get-in (first (get-in @state [:runner :prompt])) [:msg])]
        (is (= "The top card of R&D is Hedge Fund" topcard)))
      (is (= 1 (count (:discard (get-runner))))))))
