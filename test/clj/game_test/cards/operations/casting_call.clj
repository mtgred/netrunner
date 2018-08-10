(ns game-test.cards.operations.casting-call
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest casting-call
  ;; Casting Call - Only do card-init on the Public agendas.  Issue #1128
  (do-game
    (new-game {:corp {:deck [(qty "Casting Call" 2) "Oaktown Renovation"
                             "Improved Tracers" "Hunter"]}})
    (core/gain state :corp :click 1)
    (play-from-hand state :corp "Hunter" "HQ")
    (let [hunter (get-ice state :hq 0)]
      (core/rez state :corp hunter)
      (is (= 4 (:current-strength (refresh hunter))))
      (play-from-hand state :corp "Casting Call")
      (click-card state :corp (find-card "Improved Tracers" (:hand (get-corp))))
      (click-prompt state :corp "New remote")
      (let [imptrac (get-content state :remote1 0)]
        (is (:rezzed (refresh imptrac)) "Improved Tracers is faceup")
        (is (= 4 (:current-strength (refresh hunter))) "Hunter hasn't gained strength")
        (play-from-hand state :corp "Casting Call")
        (click-card state :corp (find-card "Oaktown Renovation" (:hand (get-corp))))
        (click-prompt state :corp "New remote")
        (let [oak (get-content state :remote2 0)]
          (core/advance state :corp {:card (refresh oak)})
          (is (= 5 (:credit (get-corp))) "Events on Public agenda work; gained 2 credits from advancing")
          (take-credits state :corp)
          (run-empty-server state "Server 2")
          (click-card state :runner oak)
          (click-prompt state :runner "Steal")
          (is (= 2 (:tag (get-runner))) "Runner took 2 tags from accessing agenda with Casting Call hosted on it"))))))
