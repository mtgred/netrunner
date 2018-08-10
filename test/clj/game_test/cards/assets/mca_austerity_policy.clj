(ns game-test.cards.assets.mca-austerity-policy
  (:require [game.core :as core]
            [game-test.core :refer :all]
            [game-test.utils :refer :all]
            [game-test.macros :refer :all]
            [clojure.test :refer :all]))

(deftest mca-austerity-policy
  (do-game
    (new-game {:corp {:deck ["MCA Austerity Policy"]}})
    (play-from-hand state :corp "MCA Austerity Policy" "New remote")
    (let [mca (get-content state :remote1 0)]
      (core/rez state :corp mca)
      (card-ability state :corp mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      ; once per turn only
      (card-ability state :corp mca 0)
      (is (= 1 (get-counters (refresh mca) :power)))
      (take-credits state :corp)
      ; runner loses a click
      (is (= 3 (:click (get-runner))))
      (take-credits state :runner)
      (card-ability state :corp mca 0)
      (is (= 2 (get-counters (refresh mca) :power)))
      (take-credits state :corp)
      (take-credits state :runner)
      (card-ability state :corp mca 0)
      (is (= 3 (get-counters (refresh mca) :power)))
      ; Fire MCA
      (is (= 2 (:click (get-corp))))
      (card-ability state :corp (refresh mca) 1)
      (is (= 5 (:click (get-corp)))))))
