(ns game-test.cards.identities.haarpsichord-studios-entertainment-unleashed
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest haarpsichord-studios-entertainment-unleashed
  ;; Haarpsichord Studios
  (testing "Prevent stealing more than 1 agenda per turn"
    (do-game
      (new-game {:corp {:id "Haarpsichord Studios: Entertainment Unleashed"
                        :deck [(qty "15 Minutes" 3)]}
                 :runner {:deck ["Gang Sign"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Gang Sign")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 1 (:agenda-point (get-runner))) "Second steal of turn prevented")
      (take-credits state :runner)
      (play-from-hand state :corp "15 Minutes" "New remote")
      (score-agenda state :corp (get-content state :remote1 0))
      (click-prompt state :runner "Card from hand")
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Steal prevention didn't carry over to Corp turn")))
  (testing "Interactions with Employee Strike. Issue #1313"
    (do-game
      (new-game {:corp {:id "Haarpsichord Studios: Entertainment Unleashed"
                        :deck [(qty "15 Minutes" 3)]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (take-credits state :corp)
      (core/gain state :runner :click 5)
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 1 (:agenda-point (get-runner))))
      (play-from-hand state :runner "Employee Strike")
      (run-empty-server state "HQ")
      (click-prompt state :runner "Steal")
      (is (= 2 (:agenda-point (get-runner))) "Second steal not prevented")
      (play-from-hand state :runner "Scrubbed")
      (run-empty-server state "HQ")
      (click-prompt state :runner "No action")
      (is (= 2 (:agenda-point (get-runner))) "Third steal prevented"))))
