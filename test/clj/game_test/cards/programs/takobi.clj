(ns game-test.cards.programs.takobi
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest takobi
  ;; Takobi - 2 power counter to add +3 strength to a non-AI icebreaker for encounter
  (do-game
    (new-game {:corp {:deck ["Enigma"]}
               :runner {:deck ["Takobi" "Corroder" "Faust"]}})
    (play-from-hand state :corp "Enigma" "HQ")
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Takobi")
    (play-from-hand state :runner "Corroder")
    (play-from-hand state :runner "Faust")
    (let [tako (get-program state 0)
          corr (get-program state 1)
          faus (get-program state 2)]
      (dotimes [_ 3]
        (card-ability state :runner tako 0))
      (is (= 3 (get-counters (refresh tako) :power)) "3 counters on Takobi")
      (run-on state "HQ")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt for un-rezzed ice")
      (core/rez state :corp (get-ice state :hq 0))
      (card-ability state :runner tako 1)
      (click-card state :runner (refresh faus))
      (is (not-empty (:prompt (get-runner))) "Can't select AI breakers")
      (click-card state :runner (refresh corr))
      (is (empty? (:prompt (get-runner))) "Can select non-AI breakers")
      (is (= 5 (:current-strength (refresh corr))) "Corroder at +3 strength")
      (is (= 1 (get-counters (refresh tako) :power)) "1 counter on Takobi")
      (card-ability state :runner tako 1)
      (is (empty? (:prompt (get-runner))) "No prompt when too few power counters")
      (core/no-action state :corp nil)
      (run-continue state)
      (is (= 2 (:current-strength (refresh corr))) "Corroder returned to normal strength"))))
