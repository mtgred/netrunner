(ns game-test.cards.programs.dhegdheer
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest dhegdheer
  ;; Dheghdheer - hosting a breaker with strength based on unused MU should calculate correctly
  (do-game
    (new-game {:runner {:deck ["Adept" "Dhegdheer"]}})
    (take-credits state :corp)
    (core/gain state :runner :credit 5)
    (play-from-hand state :runner "Dhegdheer")
    (play-from-hand state :runner "Adept")
    (is (= 3 (:credit (get-runner))) "3 credits left after individual installs")
    (is (= 2 (core/available-mu state)) "2 MU used")
    (let [dheg (get-program state 0)
          adpt (get-program state 1)]
      (is (= 4 (:current-strength (refresh adpt))) "Adept at 4 strength individually")
      (card-ability state :runner dheg 1)
      (click-card state :runner (refresh adpt))
      (let [hosted-adpt (first (:hosted (refresh dheg)))]
        (is (= 4 (:credit (get-runner))) "4 credits left after hosting")
        (is (= 4 (core/available-mu state)) "0 MU used")
        (is (= 6 (:current-strength (refresh hosted-adpt))) "Adept at 6 strength hosted")))))
