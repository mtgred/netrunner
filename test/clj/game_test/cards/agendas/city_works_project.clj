(ns game-test.cards.agendas.city-works-project
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest city-works-project
  ;; City Works Project
  (do-game
    (new-game {:corp {:deck ["City Works Project"]}
               :runner {:deck [(qty "Sure Gamble" 4)]}})
    (play-from-hand state :corp "City Works Project" "New remote")
    (let [cwp (get-content state :remote1 0)]
      (core/advance state :corp {:card (refresh cwp)})
      (core/advance state :corp {:card (refresh cwp)}))
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 4 (count (:discard (get-runner)))) "Runner paid 4 meat damage")))
