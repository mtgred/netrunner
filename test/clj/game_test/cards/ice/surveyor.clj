(ns game-test.cards.ice.surveyor
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest surveyor
  ;; Surveyor ice strength
  (do-game
    (new-game {:corp {:deck [(qty "Surveyor" 1) (qty "Ice Wall" 2)]}})
    (core/gain state :corp :credit 10)
    (core/gain state :runner :credit 10)
    (play-from-hand state :corp "Surveyor" "HQ")
    (let [surv (get-ice state :hq 0)]
      (core/rez state :corp surv)
      (is (= 2 (:current-strength (refresh surv))) "Surveyor has 2 strength for itself")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 4 (:current-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of ICE")
      (play-from-hand state :corp "Ice Wall" "HQ")
      (is (= 6 (:current-strength (refresh surv))) "Surveyor has 6 strength for 3 pieces of ICE")
      (run-on state "HQ")
      (card-subroutine state :corp surv 0)
      (is (= 6 (-> (get-corp) :prompt first :base)) "Trace should be base 6")
      (click-prompt state :corp "0")
      (click-prompt state :runner "5")
      (is (= 2 (:tag (get-runner))) "Runner took 2 tags from Surveyor Trace 6 with boost 5")
      (card-subroutine state :corp surv 0)
      (is (= 6 (-> (get-corp) :prompt first :base)) "Trace should be base 6")
      (click-prompt state :corp "0")
      (click-prompt state :runner "6")
      (is (= 2 (:tag (get-runner))) "Runner did not take tags from Surveyor Trace 6 with boost 6")
      (core/move-card state :corp {:card (get-ice state :hq 1) :server "Archives"})
      (is (= 4 (:current-strength (refresh surv))) "Surveyor has 4 strength for 2 pieces of ICE"))))
