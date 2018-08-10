(ns game-test.cards.hardware.archives-interface
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest archives-interface
  ;; Archives Interface - Remove 1 card in Archives from the game instead of accessing it
  (do-game
    (new-game {:corp {:deck ["Shock!" "Launch Campaign"]}
               :runner {:deck ["Archives Interface" "Imp"]}})
    (take-credits state :corp)
    (core/move state :corp (find-card "Shock!" (:hand (get-corp))) :discard)
    (core/move state :corp (find-card "Launch Campaign" (:hand (get-corp))) :discard)
    (play-from-hand state :runner "Archives Interface")
    (run-empty-server state :archives)
    (click-prompt state :runner "Yes")
    (click-prompt state :runner (find-card "Shock!" (:discard (get-corp))))
    (is (= "Shock!" (:title (first (:rfg (get-corp))))) "Shock! removed from game")
    (is (empty? (:discard (get-runner))) "Didn't access Shock!, no net damage taken")))
