(ns game-test.cards.upgrades.off-the-grid
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest off-the-grid
  ;; Off the Grid run restriction - and interaction with RP
  (do-game
    (new-game {:corp {:id "Jinteki: Replicating Perfection"
                      :deck [(qty "Off the Grid" 3)
                             (qty "Mental Health Clinic" 3)]}})
    (play-from-hand state :corp "Off the Grid" "New remote")
    (play-from-hand state :corp "Mental Health Clinic" "Server 1")
    (let [otg (get-content state :remote1 0)]
      (take-credits state :corp)
      (core/rez state :corp (refresh otg))
      (is (not (core/can-run-server? state "Server 1")) "Runner can only run on centrals")
      (run-empty-server state "R&D")
      (is (not (core/can-run-server? state "Server 1")) "Runner cannot run on Off the Grid")
      (take-credits state :runner)
      (take-credits state :corp)
      (is (not (core/can-run-server? state "Server 1")) "Off the Grid prevention persisted")
      (run-empty-server state "HQ")
      (is (boolean (core/can-run-server? state "Server 1")) "Runner can run on Server 1")
      (is (= nil (refresh otg)) "Off the Grid trashed"))))
