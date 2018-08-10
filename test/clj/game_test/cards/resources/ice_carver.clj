(ns game-test.cards.resources.ice-carver
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest ice-carver
  ;; Ice Carver - lower ice strength on encounter
  (do-game
    (new-game {:corp {:deck ["Ice Wall"]}
               :runner {:deck ["Ice Carver"]}})
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :corp 2)
    (let [iwall (get-ice state :archives 0)]
      (core/rez state :corp iwall)
      (play-from-hand state :runner "Ice Carver")
      (run-on state "Archives")
      (is (zero? (:current-strength (refresh iwall))) "Ice Wall strength at 0 for encounter")
      (run-jack-out state)
      (is (= 1 (:current-strength (refresh iwall))) "Ice Wall strength at 1 after encounter"))))
