(ns game-test.cards.assets.launch-campaign
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest launch-campaign
  ;; Launch Campaign
  (do-game
    (new-game {:corp {:deck ["Launch Campaign"]}})
    (play-from-hand state :corp "Launch Campaign" "New remote")
    (let [launch (get-content state :remote1 0)]
      (core/rez state :corp launch)
      (is (= 4 (:credit (get-corp))))
      (is (= 6 (get-counters (refresh launch) :credit)))
      (take-credits state :corp 2)
      (take-credits state :runner)
      (is (= 8 (:credit (get-corp))))
      (is (= 4 (get-counters (refresh launch) :credit))))))
