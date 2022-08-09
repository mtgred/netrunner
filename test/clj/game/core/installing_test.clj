(ns game.core.installing-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core-test :refer :all]
   [game.core.card :refer :all]
   [game.macros-test :refer :all]
   [game.utils-test :refer :all]))

(deftest trash-existing-programs-test
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Endless Hunger" "Corroder" "Akamatsu Mem Chip"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Endless Hunger")
    (play-from-hand state :runner "Corroder")
    (click-card state :runner "Endless Hunger")
    (is (= "Corroder" (:title (get-program state 0))))
    (is (find-card "Endless Hunger" (:discard (get-runner))))))
