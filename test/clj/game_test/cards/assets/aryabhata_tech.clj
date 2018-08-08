(ns game-test.cards.assets.aryabhata-tech
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest aryabhata-tech
  ;; Aryabhata Tech
  (do-game
    (new-game {:corp {:deck ["Aryabhata Tech"
                             "Hunter"]}})
    (play-from-hand state :corp "Aryabhata Tech" "New remote")
    (play-from-hand state :corp "Hunter" "HQ")
    (let [at (get-content state :remote1 0)
          h (get-ice state :hq 0)]
      (core/rez state :corp (refresh at))
      (core/rez state :corp (refresh h))
      (take-credits state :corp)
      (run-on state :hq)
      (let [c-credits (:credit (get-corp))
            r-credits (:credit (get-runner))]
        (card-subroutine state :corp h 0)
        (click-prompt state :corp "0")
        (click-prompt state :runner "0")
        (is (= 1 (- (:credit (get-corp)) c-credits)))
        (is (= -1 (- (:credit (get-runner)) r-credits)))))))
