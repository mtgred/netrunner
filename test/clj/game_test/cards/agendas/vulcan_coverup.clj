(ns game-test.cards.agendas.vulcan-coverup
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest vulcan-coverup
  ;; Vulcan Coverup
  (do-game
    (new-game {:corp {:deck [(qty "Vulcan Coverup" 2)]}})
    (play-from-hand state :corp "Vulcan Coverup" "New remote")
    (take-credits state :corp)
    (run-empty-server state :remote1)
    (click-prompt state :runner "Steal")
    (is (= 1 (:bad-publicity (get-corp))) "Took 1 bad pub from stolen agenda")
    (take-credits state :runner)
    (play-and-score state "Vulcan Coverup")
    (is (= 2 (count (:discard (get-runner)))) "Did 2 meat damage upon scoring")))
