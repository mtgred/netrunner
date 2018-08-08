(ns game-test.cards.ice.archangel
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest archangel
  ;; Archangel - accessing from R&D does not cause run to hang.
  (do-game
    (new-game {:corp {:deck ["Archangel" "Hedge Fund"]}
               :runner {:deck ["Bank Job"]}})
    (starting-hand state :corp ["Hedge Fund"])
    (take-credits state :corp)
    (play-from-hand state :runner "Bank Job")
    (run-empty-server state :rd)
    (click-prompt state :corp "Yes")
    (click-prompt state :runner "Yes")
    (click-prompt state :corp "0")
    (click-prompt state :runner "0")
    (click-card state :corp (get-resource state 0))
    (click-prompt state :runner "No action")
    (is (not (:run @state)) "Run ended")))
