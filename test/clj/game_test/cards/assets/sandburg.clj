(ns game-test.cards.assets.sandburg
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest sandburg
  ;; Sandburg - +1 strength to all ICE for every 5c when Corp has over 10c
  (do-game
    (new-game {:corp {:deck ["Sandburg" (qty "Ice Wall" 2) (qty "Hedge Fund" 3)]}})
    (core/gain state :corp :click 3 :credit 3)
    (play-from-hand state :corp "Sandburg" "New remote")
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (let [sb (get-content state :remote1 0)
          iwall1 (get-ice state :hq 0)
          iwall2 (get-ice state :rd 0)]
      (core/rez state :corp iwall1)
      (core/rez state :corp iwall2)
      (core/rez state :corp sb)
      (is (= 6 (:credit (get-corp))))
      (play-from-hand state :corp "Hedge Fund")
      (is (= 10 (:credit (get-corp))))
      (is (= 3 (:current-strength (refresh iwall1))) "Strength boosted by 2")
      (is (= 3 (:current-strength (refresh iwall2))) "Strength boosted by 2")
      (play-from-hand state :corp "Hedge Fund")
      (play-from-hand state :corp "Hedge Fund")
      (is (= 18 (:credit (get-corp))))
      (is (= 4 (:current-strength (refresh iwall1))) "Strength boosted by 3")
      (is (= 4 (:current-strength (refresh iwall2))) "Strength boosted by 3")
      (take-credits state :corp)
      (run-empty-server state "Server 1")
      (click-prompt state :runner "Pay 4 [Credits] to trash")
      (is (= 1 (:current-strength (refresh iwall1))) "Strength back to default")
      (is (= 1 (:current-strength (refresh iwall2))) "Strength back to default"))))
