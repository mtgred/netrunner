(ns game-test.cards.programs.leprechaun
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest leprechaun
  ;; Leprechaun - hosting a breaker with strength based on unused MU should calculate correctly
  (testing "Basic test"
    (do-game
      (new-game {:runner {:deck ["Adept" "Leprechaun"]}})
      (take-credits state :corp)
      (core/gain state :runner :credit 5)
      (play-from-hand state :runner "Leprechaun")
      (play-from-hand state :runner "Adept")
      (is (= 1 (core/available-mu state)) "3 MU used")
      (let [lep (get-program state 0)
            adpt (get-program state 1)]
        (is (= 3 (:current-strength (refresh adpt))) "Adept at 3 strength individually")
        (card-ability state :runner lep 1)
        (click-card state :runner (refresh adpt))
        (let [hosted-adpt (first (:hosted (refresh lep)))]
          (is (= 3 (core/available-mu state)) "1 MU used")
          (is (= 5 (:current-strength (refresh hosted-adpt))) "Adept at 5 strength hosted")))))
  (testing "Keep MU the same when hosting or trashing hosted programs"
    (do-game
      (new-game {:runner {:deck ["Leprechaun" "Hyperdriver" "Imp"]}})
      (take-credits state :corp)
      (play-from-hand state :runner "Leprechaun")
      (let [lep (get-program state 0)]
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Hyperdriver" (:hand (get-runner))))
        (is (= 2 (:click (get-runner))))
        (is (= 2 (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not deducted from available MU")
        (card-ability state :runner lep 0)
        (click-card state :runner (find-card "Imp" (:hand (get-runner))))
        (is (= 1 (:click (get-runner))))
        (is (zero? (:credit (get-runner))))
        (is (= 3 (core/available-mu state)) "Imp 1 MU not deducted from available MU")
        ;; Trash Hyperdriver
        (core/move state :runner (find-card "Hyperdriver" (:hosted (refresh lep))) :discard)
        (is (= 3 (core/available-mu state)) "Hyperdriver 3 MU not added to available MU")
        (core/move state :runner (find-card "Imp" (:hosted (refresh lep))) :discard) ; trash Imp
        (is (= 3 (core/available-mu state)) "Imp 1 MU not added to available MU")))))
