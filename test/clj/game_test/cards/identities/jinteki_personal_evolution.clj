(ns game-test.cards.identities.jinteki-personal-evolution
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jinteki-personal-evolution
  ;; Personal Evolution - Prevent runner from running on remotes unless they first run on a central
  (do-game
    (new-game {:corp {:id "Jinteki: Personal Evolution"
                      :deck [(qty "Braintrust" 6)]}
               :runner {:deck [(qty "Sure Gamble" 3)]}})
    (play-from-hand state :corp "Braintrust" "New remote")
    (take-credits state :corp)
    (run-empty-server state "Server 1")
    (click-prompt state :runner "Steal")
    (is (= 2 (count (:hand (get-runner)))) "Runner took 1 net damage from steal")))
