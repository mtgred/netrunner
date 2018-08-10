(ns game-test.cards.assets.server-diagnostics
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest server-diagnostics
  ;; Server Diagnostics - Gain 2c when turn begins; trashed when ICE is installed
  (do-game
    (new-game {:corp {:deck ["Server Diagnostics" "Pup"
                             "Launch Campaign"]}})
    (play-from-hand state :corp "Server Diagnostics" "New remote")
    (core/rez state :corp (get-content state :remote1 0))
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (is (= 1 (count (get-content state :remote1))) "Non-ICE install didn't trash Serv Diag")
    (take-credits state :corp)
    (take-credits state :runner)
    (is (= 5 (:credit (get-corp))) "Gained 2c at start of turn")
    (play-from-hand state :corp "Pup" "HQ")
    (is (= 1 (count (:discard (get-corp)))) "Server Diagnostics trashed by ICE install")))
