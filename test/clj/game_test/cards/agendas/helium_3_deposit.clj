(ns game-test.cards.agendas.helium-3-deposit
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest helium-3-deposit
  ;; Helium-3 Deposit
  (do-game
    (new-game {:corp {:deck ["Helium-3 Deposit"
                             "Chief Slee"
                             "Ice Wall"]}})
    (play-from-hand state :corp "Chief Slee" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (take-credits state :corp)
    (let [cs (get-content state :remote1 0)
          iw (get-ice state :hq 0)]
      (is (zero? (get-counters (refresh cs) :power)) "Chief Slee should start with 0 power counters")
      (core/rez state :corp iw)
      (run-on state "HQ")
      (card-ability state :corp cs 0)
      (is (= 1 (get-counters (refresh cs) :power)) "Chief Slee should gain 1 power counter")
      (take-credits state :runner)
      (play-and-score state "Helium-3 Deposit")
      (click-prompt state :corp "2")
      (click-card state :corp cs)
      (is (= 3 (get-counters (refresh cs) :power)) "Chief Slee should gain 2 power counters from 1 to 3"))))
