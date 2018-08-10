(ns game-test.cards.assets.eve-campaign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest eve-campaign
  ;; Eve Campaign
  (do-game
    (new-game {:corp {:deck ["Eve Campaign"]}})
    (play-from-hand state :corp "Eve Campaign" "New remote")
    (let [eve (get-content state :remote1 0)]
      (core/rez state :corp eve)
      (is (zero? (:credit (get-corp))))
      (is (= 16 (get-counters (refresh eve) :credit)))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 4 (:credit (get-corp))))
      (is (= 14 (get-counters (refresh eve) :credit))))))
