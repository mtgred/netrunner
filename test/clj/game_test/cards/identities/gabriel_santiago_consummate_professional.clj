(ns game-test.cards.identities.gabriel-santiago-consummate-professional
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest gabriel-santiago-consummate-professional
  ;; Gabriel Santiago - Gain 2c on first successful HQ run each turn
  (do-game
    (new-game {:runner {:id "Gabriel Santiago: Consummate Professional"
                        :deck ["Easy Mark"]}})
    (take-credits state :corp)
    (run-empty-server state :rd)
    (is (= 5 (:credit (get-runner))) "No credits gained")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "Gained 2c")
    (run-empty-server state :hq)
    (is (= 7 (:credit (get-runner))) "No credits gained")))
