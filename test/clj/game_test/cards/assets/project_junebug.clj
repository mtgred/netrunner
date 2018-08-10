(ns game-test.cards.assets.project-junebug
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest project-junebug
  ;; Project Junebug
  (do-game
    (new-game {:corp {:deck ["Project Junebug"]}
               :runner {:deck [(qty "Sure Gamble" 100)]}})
    (play-from-hand state :corp "Project Junebug" "New remote")
    (advance state (get-content state :remote1 0) 2)
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (let [credits (:credit (get-corp))]
      (click-prompt state :corp "Yes")
      (is (= (- credits 1) (:credit (get-corp))) "Corp should pay 1 for Project Junebug ability")
      (is (= 4 (-> (get-runner) :discard count)) "Project Junebug should do 4 net damage"))))
