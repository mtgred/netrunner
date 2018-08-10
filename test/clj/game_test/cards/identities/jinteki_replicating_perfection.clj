(ns game-test.cards.identities.jinteki-replicating-perfection
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest jinteki-replicating-perfection
  ;; Replicating Perfection - Prevent runner from running on remotes unless they first run on a central
  (testing "Basic test"
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (run-empty-server state "HQ")
      (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")))
  (testing "interaction with Employee Strike. Issue #1313 and #1956."
    (do-game
      (new-game {:corp {:id "Jinteki: Replicating Perfection"
                        :deck [(qty "Mental Health Clinic" 3)]}
                 :runner {:deck ["Employee Strike" "Scrubbed"]}})
      (play-from-hand state :corp "Mental Health Clinic" "New remote")
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (play-from-hand state :runner "Employee Strike")
      (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on remotes")
      (play-from-hand state :runner "Scrubbed")
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals"))))
