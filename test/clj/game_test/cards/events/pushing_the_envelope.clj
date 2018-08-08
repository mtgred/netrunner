(ns game-test.cards.events.pushing-the-envelope
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest pushing-the-envelope
  ;; Run. Add 2 strength to each installer breaker.
  (do-game
    (new-game {:runner {:deck [(qty "Pushing the Envelope" 3) (qty "Corroder" 2) "Atman"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 20)
    (core/gain state :runner :click 10)
    (core/draw state :runner)
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Atman")
    (click-prompt state :runner "0")
    (let [atman (get-program state 1)
          corr (get-program state 0)]
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (play-from-hand state :runner "Pushing the Envelope")
      (click-prompt state :runner "Archives")
      ; 3 cards in hand - no boost
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (play-from-hand state :runner "Pushing the Envelope")
      (click-prompt state :runner "Archives")
      (run-continue state)
      ; 2 cards in hand - boost
      (is (= 2 (:current-strength (refresh atman))) "Atman 2 current strength")
      (is (= 4 (:current-strength (refresh corr))) "Corroder 2 current strength")
      (run-successful state)
      (is (zero? (:current-strength (refresh atman))) "Atman 0 current strength")
      (is (= 2 (:current-strength (refresh corr))) "Corroder 2 current strength"))))
