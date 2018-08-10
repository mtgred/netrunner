(ns game-test.cards.identities.cerebral-imaging-infinite-frontiers
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest cerebral-imaging-infinite-frontiers
  ;; Cerebral Imaging - Maximum hand size equal to credits
  (do-game
    (new-game {:corp {:id "Cerebral Imaging: Infinite Frontiers"
                      :deck [(qty "Hedge Fund" 3)]}})
    (play-from-hand state :corp "Hedge Fund")
    (play-from-hand state :corp "Hedge Fund")
    (is (= 13 (:credit (get-corp))) "Has 13 credits")
    (is (= 13 (core/hand-size state :corp)) "Max hand size is 13")))
