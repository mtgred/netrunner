(ns game-test.cards.assets.blacklist
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest blacklist
  ;; Blacklist
  (testing "#2426.  Need to allow steal."
    (do-game
      (new-game {:corp {:deck [(qty "Fetal AI" 3) "Blacklist"]}})
      (trash-from-hand state :corp "Fetal AI")
      (play-from-hand state :corp "Blacklist" "New remote")
      (core/rez state :corp (get-content state :remote1 0))
      (is (= 1 (count (:discard (get-corp)))))
      (take-credits state :corp)
      (run-empty-server state :archives)
      (click-prompt state :runner "Pay 2 [Credits] to steal")
      (is (= 2 (:agenda-point (get-runner))) "Runner has 2 agenda points")
      (is (= 1 (count (:scored (get-runner))))))))
