(ns game-test.cards.ice.turing
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest turing
  ;; Turing - Strength boosted when protecting a remote server
  (do-game
    (new-game {:corp {:deck [(qty "Turing" 2) "Hedge Fund"]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Turing" "HQ")
    (play-from-hand state :corp "Turing" "New remote")
    (let [t1 (get-ice state :hq 0)
          t2 (get-ice state :remote1 0)]
      (core/rez state :corp t1)
      (is (= 2 (:current-strength (refresh t1)))
          "Turing default 2 strength over a central server")
      (core/rez state :corp t2)
      (is (= 5 (:current-strength (refresh t2)))
          "Turing increased to 5 strength over a remote server"))))
