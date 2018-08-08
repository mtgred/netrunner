(ns game-test.cards.operations.mass-commercialization
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mass-commercialization
  ;; Mass Commercialization
  (do-game
    (new-game {:corp {:deck ["Mass Commercialization"
                             (qty "Ice Wall" 3)]}})
    (play-from-hand state :corp "Ice Wall" "HQ")
    (play-from-hand state :corp "Ice Wall" "R&D")
    (play-from-hand state :corp "Ice Wall" "Archives")
    (take-credits state :runner)
    (core/advance state :corp {:card (refresh (get-ice state :hq 0))})
    (core/advance state :corp {:card (refresh (get-ice state :archives 0))})
    (core/advance state :corp {:card (refresh (get-ice state :rd 0))})
    (take-credits state :runner)
    (play-from-hand state :corp "Mass Commercialization")
    (is (= 8 (:credit (get-corp))) "Gained 6 for 3 advanced ice from Mass Commercialization")))
