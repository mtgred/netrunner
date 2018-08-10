(ns game-test.cards.operations.power-shutdown
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest power-shutdown
  ;; Power Shutdown - Trash cards from R&D to force Runner to trash a program or hardware
  (do-game
    (new-game {:corp {:deck [(qty "Power Shutdown" 3) (qty "Hive" 3)]}
               :runner {:deck ["Grimoire" "Cache"]}})
    (play-from-hand state :corp "Power Shutdown")
    (is (empty? (:discard (get-corp))) "Not played, no run last turn")
    (take-credits state :corp)
    (play-from-hand state :runner "Cache")
    (play-from-hand state :runner "Grimoire")
    (run-empty-server state :archives)
    (take-credits state :runner)
    (core/move state :corp (find-card "Hive" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Hive" (:hand (get-corp))) :deck)
    (core/move state :corp (find-card "Hive" (:hand (get-corp))) :deck)
    (play-from-hand state :corp "Power Shutdown")
    (click-prompt state :corp "2")
    (is (= 3 (count (:discard (get-corp)))) "2 cards trashed from R&D")
    (is (= 1 (count (:deck (get-corp)))) "1 card remaining in R&D")
    (click-card state :runner (get-hardware state 0)) ; try targeting Grimoire
    (is (empty? (:discard (get-runner))) "Grimoire too expensive to be targeted")
    (click-card state :runner (get-program state 0))
    (is (= 1 (count (:discard (get-runner)))) "Cache trashed")))
