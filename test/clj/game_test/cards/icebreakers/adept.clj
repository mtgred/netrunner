(ns game-test.cards.icebreakers.adept
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest adept
  ;; Adept - +1 str for each unused MU
  (do-game
    (new-game {:runner {:deck ["Adept" "Box-E"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 10)
    (play-from-hand state :runner "Adept")
    (let [ad (get-program state 0)]
      (is (= 2 (core/available-mu state)))
      (is (= 4 (:current-strength (refresh ad))) "+2 strength for 2 unused MU")
      (play-from-hand state :runner "Box-E")
      (is (= 4 (core/available-mu state)))
      (is (= 6 (:current-strength (refresh ad))) "+4 strength for 4 unused MU"))))
