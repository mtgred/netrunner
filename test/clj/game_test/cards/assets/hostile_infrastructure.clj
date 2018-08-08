(ns game-test.cards.assets.hostile-infrastructure
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest hostile-infrastructure
  ;; Hostile Infrastructure - do 1 net damage when runner trashes a corp card
  (do-game
    (new-game {:corp {:deck [(qty "Hostile Infrastructure" 3)]}})
    (core/gain state :runner :credit 50)
    (play-from-hand state :corp "Hostile Infrastructure" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (take-credits state :corp)
    (run-empty-server state :hq)
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (= 1 (count (:discard (get-runner)))) "Took 1 net damage")
    (run-empty-server state :remote1)
    (click-prompt state :runner "Pay 5 [Credits] to trash")
    (is (= 2 (count (:discard (get-runner)))) "Took 1 net damage")))
