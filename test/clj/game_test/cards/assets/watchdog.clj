(ns game-test.cards.assets.watchdog
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest watchdog
  ;; Watchdog - Reduce rez cost of first ICE per turn by number of Runner tags
  (do-game
    (new-game {:corp {:deck ["Watchdog" "Architect" "Wraparound"]}})
    (play-from-hand state :corp "Watchdog" "New remote")
    (play-from-hand state :corp "Wraparound" "HQ")
    (play-from-hand state :corp "Architect" "HQ")
    (let [wd (get-content state :remote1 0)
          arch (get-ice state :hq 1)
          wrap (get-ice state :hq 0)]
      (take-credits state :corp)
      (is (= 4 (:credit (get-corp))))
      (core/gain state :runner :tag 2)
      (run-on state "HQ")
      (core/rez state :corp wd)
      (core/rez state :corp arch)
      (is (= 2 (:credit (get-corp))) "Only 2 credits to rez Architect")
      (core/rez state :corp wrap)
      (is (zero? (:credit (get-corp))) "No rez discount on Wraparound"))))
