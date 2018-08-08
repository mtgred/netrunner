(ns game-test.cards.assets.tenma-line
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest tenma-line
  ;; Tenma Line - Swap 2 pieces of installed ICE
  (do-game
    (new-game {:corp {:deck ["Tenma Line" "Harvester"
                             "Aimor" "Lockdown"]}})
    (core/gain state :corp :click 2)
    (play-from-hand state :corp "Tenma Line" "New remote")
    (play-from-hand state :corp "Harvester" "HQ")
    (play-from-hand state :corp "Aimor" "HQ")
    (play-from-hand state :corp "Lockdown" "R&D")
    (core/rez state :corp (get-content state :rd 0))
    (core/rez state :corp (get-content state :remote1 0))
    (is (= 1 (:click (get-corp))))
    (card-ability state :corp (get-content state :remote1 0) 0)
    (click-card state :corp (get-ice state :rd 0))
    (click-card state :corp (get-ice state :hq 1))
    (is (empty? (:prompt (get-corp))))
    (is (zero? (:click (get-corp))) "Spent 1 click")
    (is (= "Aimor" (:title (get-ice state :rd 0))) "Aimor swapped to R&D")
    (is (= "Lockdown" (:title (get-ice state :hq 1))) "Lockdown swapped to HQ outer position")))
