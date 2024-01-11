(ns game.core.installing-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.test-framework :refer :all]))

(deftest trash-existing-programs-test
  (do-game
    (new-game {:corp {:hand ["Hedge Fund"]
                      :deck [(qty "Hedge Fund" 100)]}
               :runner {:hand ["Endless Hunger" "Corroder"]}})
    (take-credits state :corp)
    (play-from-hand state :runner "Endless Hunger")
    (play-from-hand state :runner "Corroder")
    (is (= "Insufficient MU to install Corroder. Trash installed programs?" (:msg (prompt-map :runner))))
    (click-card state :runner "Endless Hunger")
    (is (= "Corroder" (:title (get-program state 0))))
    (is (find-card "Endless Hunger" (:discard (get-runner))))))
