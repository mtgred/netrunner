(ns game-test.cards.resources.xanadu
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest xanadu
  ;; Xanadu - Increase all ICE rez cost by 1 credit
  (do-game
    (new-game {:corp {:deck [(qty "Paper Wall" 2) "Launch Campaign"]}
               :runner {:deck ["Xanadu"]}})
    (play-from-hand state :corp "Paper Wall" "HQ")
    (play-from-hand state :corp "Paper Wall" "R&D")
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (take-credits state :corp)
    (play-from-hand state :runner "Xanadu")
    (let [pw1 (get-ice state :hq 0)
          pw2 (get-ice state :rd 0)
          lc (get-content state :remote1 0)]
      (core/rez state :corp pw1)
      (is (= 4 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :corp pw2)
      (is (= 3 (:credit (get-corp))) "Paid 1 instead of 0 to rez Paper Wall")
      (core/rez state :corp lc)
      (is (= 2 (:credit (get-corp))) "Paid 1 to rez Launch Campaign; no effect on non-ICE"))))
