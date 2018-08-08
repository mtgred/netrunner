(ns game-test.cards.events.system-outage
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest system-outage
  ;; When Corp draws 1+ cards, it loses 1 if it is not the first time he or she has drawn cards this turn
  (do-game
    (new-game {:corp {:deck [(qty "Turing" 10)]}
               :runner {:deck ["Street Peddler"
                               (qty "System Outage" 3)]}})
    (starting-hand state :corp [])
    (starting-hand state :runner ["Street Peddler" "System Outage"])
    (take-credits state :corp) ; corp at 8cr
    (play-from-hand state :runner "Street Peddler")
    (take-credits state :runner)
    (core/click-draw state :corp 1)
    (is (= 8 (:credit (get-corp))) "1st card drawn for free - System Outage on Peddler")
    (core/click-draw state :corp 1)
    (is (= 8 (:credit (get-corp))) "2nd card drawn for free - System Outage on Peddler")
    (take-credits state :corp) ; corp at 9cr
    (is (= 9 (:credit (get-corp))) "Corp at 9")
    (play-from-hand state :runner "System Outage")
    (take-credits state :runner)
    (core/click-draw state :corp 1)
    (is (= 8 (:credit (get-corp))) "1st card drawn cost 1cr - System Outage active")
    (core/click-draw state :corp 1)
    (is (= 7 (:credit (get-corp))) "2nd card drawn cost 1cr - System Outage active")))
