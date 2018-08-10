(ns game-test.cards.icebreakers.cerberus-rex-h2
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cerberus-rex-h2
  ;; Cerberus "Rex" H2 - boost 1 for 1 cred, break for 1 counter
  (do-game
    (new-game {:runner {:deck ["Cerberus \"Rex\" H2"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Cerberus \"Rex\" H2")
    (is (= 2 (:credit (get-runner))) "2 credits left after install")
    (let [rex (get-program state 0)]
      (is (= 4 (get-counters rex :power)) "Start with 4 counters")
      ;; boost strength
      (card-ability state :runner rex 1)
      (is (= 1 (:credit (get-runner))) "Spend 1 credit to boost")
      (is (= 2 (:current-strength (refresh rex))) "At strength 2 after boost")
      ;; break
      (card-ability state :runner rex 0)
      (is (= 1 (:credit (get-runner))) "No credits spent to break")
      (is (= 3 (get-counters (refresh rex) :power)) "One counter used to break"))))
