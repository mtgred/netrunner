(ns game-test.cards.events.another-day-another-paycheck
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest another-day-another-paycheck
  ;; Another Day, Another Paycheck
  (do-game
    (new-game {:corp {:deck [(qty "Project Atlas" 3)]}
               :runner {:deck ["Street Peddler" (qty "Another Day, Another Paycheck" 2)]}})
    (starting-hand state :runner ["Street Peddler" "Another Day, Another Paycheck"])
    (play-from-hand state :corp "Project Atlas" "New remote")
    (score-agenda state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (play-from-hand state :runner "Street Peddler")
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (is (= 5 (:credit (get-runner))) "No trace, no gain")
    (play-from-hand state :runner "Another Day, Another Paycheck")
    (run-empty-server state :hq)
    (click-prompt state :runner "Steal")
    (click-prompt state :corp "0")
    (click-prompt state :runner "1")
    ;; 4 credits after trace, gain 6
    (is (= 10 (:credit (get-runner))) "Runner gained 6 credits")))
